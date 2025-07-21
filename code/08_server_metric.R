# --- Setup ---
rm(list=ls())
library(tidyverse)
library(data.table)
library(cluster)
library(factoextra)
library(recipes)
library(pheatmap)
library(randomForest)
library(scales)

# --- Load Data ---
df <- fread("../data/processed/scaled/wimbledon_subset_m_training.csv")

# # how many aces did john isner serve
# df_isner <- df_clean %>% 
#     filter(ServerName == "John Isner", is_ace == 1) %>%
#     summarise(total_aces = n())

# --- Clean and prepare ---
df_clean <- df %>%
    filter(!is.na(ServeWidth), !is.na(ServeDepth), ServeWidth != "", ServeDepth != "") %>%
    filter(ServeNumber %in% c(1, 2)) %>%
    mutate(
        location_bin = paste0("W", ServeWidth, "_D", ServeDepth),
        ServerName   = tolower(if_else(ServeIndicator == 1, player1, player2)),
        is_ace       = if_else(ServeIndicator == 1, P1Ace, P2Ace)
    )

# --- Helper functions ---
compute_entropy <- function(x) {
    p <- prop.table(table(x))
    -sum(p * log2(p))
}
get_mode <- function(x) {
    ux <- unique(x)
    ux[which.max(tabulate(match(x, ux)))]
}

# --- Build per-player serve profiles ---
get_serve_profiles <- function(df, serve_number_label) {
    df %>%
        filter(ServeNumber == serve_number_label) %>%
        group_by(ServerName) %>%
        summarise(
            avg_speed        = mean(Speed_MPH, na.rm = TRUE),
            sd_speed         = sd(Speed_MPH, na.rm = TRUE),
            cv_speed         = sd_speed / avg_speed,
            win_rate         = mean(PointWinner == ServeIndicator, na.rm = TRUE),
            ace_pct          = mean(is_ace, na.rm = TRUE),
            location_entropy = compute_entropy(location_bin),
            modal_location   = get_mode(location_bin),
            n_serves         = n(),
            .groups          = "drop"
        ) %>%
        filter(n_serves > 20)
}

serve_profiles1 <- get_serve_profiles(df_clean, 1)
serve_profiles2 <- get_serve_profiles(df_clean, 2)

# --- Prepare encoded and scaled data for modeling (exclude modal_location) ---
model_data <- serve_profiles1 %>%
    select(-modal_location)

recipe_obj <- recipe(~ ., data = model_data) %>%
    update_role(ServerName, new_role = "id") %>%
    update_role(n_serves, new_role = "id") %>%
    step_center(all_numeric_predictors()) %>%
    step_scale(all_numeric_predictors()) %>%
    prep()

df_encoded <- bake(recipe_obj, new_data = NULL)
df_model <- df_encoded %>%
    column_to_rownames("ServerName") %>%
    select(-n_serves)

# --- Fit Random Forest ---
rf <- randomForest(
    win_rate ~ .,
    data = df_model,
    importance = TRUE
)

imp <- importance(rf, type = 1)
imp_sorted <- sort(imp[,1], decreasing = TRUE)
print(imp_sorted)

rf_weights <- imp_sorted / sum(imp_sorted)

# --- Predict serve score using RF model ---
serve_profiles1$serve_score_rf <- predict(rf, newdata = df_model)

# 1. Rescale for scoring only
features <- names(rf_weights)

rescaled_for_score <- serve_profiles1 %>%
    mutate(across(all_of(features), ~ rescale(.x, to = c(0, 1)), .names = "rescaled_{.col}"))

# 2. Compute serve score based on rescaled values
server_quality <- rescaled_for_score %>%
    rowwise() %>%
    mutate(
        serve_score_rf_weighted = sum(c_across(starts_with("rescaled_")) * rf_weights[features])
    ) %>%
    ungroup() %>%
    arrange(desc(serve_score_rf_weighted)) %>%
    # 3. Include original values, serve score, and rescaled values for transparency
    select(
        ServerName,
        serve_score_rf,
        serve_score_rf_weighted,
        all_of(features),                          # original values
        starts_with("rescaled_"),
        n_serves
    )

# 4. View final result
print(server_quality)

