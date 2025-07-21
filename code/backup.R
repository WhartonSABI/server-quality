# --- Setup ---
rm(list = ls())
library(tidyverse)
library(data.table)
library(recipes)
library(randomForest)
library(scales)
library(ggplot2)

# --- Load Data ---
df <- fread("../data/processed/scaled/wimbledon_subset_m_training.csv")

# --- Clean and prepare ---
df_clean <- df %>%
    filter(!is.na(ServeWidth), !is.na(ServeDepth), ServeWidth != "", ServeDepth != "") %>%
    filter(ServeNumber == 1) %>%
    mutate(
        location_bin = paste0("W", ServeWidth, "_D", ServeDepth),
        ServerName = ifelse(ServeIndicator == 1, player1, player2),
        is_ace = ifelse(ServeIndicator == 1, P1Ace, P2Ace)
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

# --- Build serve profiles (First Serves) ---
serve_profiles <- df_clean %>%
    group_by(ServerName) %>%
    summarise(
        avg_speed = mean(Speed_MPH, na.rm = TRUE),
        sd_speed = sd(Speed_MPH, na.rm = TRUE),
        ace_pct = mean(is_ace, na.rm = TRUE),
        win_rate = mean(PointWinner == ServeIndicator, na.rm = TRUE),
        location_entropy = compute_entropy(location_bin),
        modal_location = get_mode(location_bin),
        n_serves = n(),
        .groups = 'drop'
    ) %>%
    filter(n_serves > 50) %>%
    select(-n_serves)

# --- One-hot encode modal_location ---
rec <- recipe(~ ., data = serve_profiles) %>%
    update_role(ServerName, new_role = "id") %>%
    step_dummy(all_nominal_predictors()) %>%
    prep()

df_encoded <- bake(rec, new_data = NULL)
# --- Remove ServerName column and prepare model data ---
df_model <- df_encoded %>%
    column_to_rownames("ServerName")

df_rf <- df_model %>%
    select(-win_rate)  # Keep only predictors

# --- Fit Random Forest model ---
rf <- randomForest(
    x = df_rf,
    y = df_model$win_rate,
    importance = TRUE
)

rf_imp <- importance(rf, type = 1)
rf_imp_sorted <- sort(rf_imp[, 1], decreasing = TRUE)
write.csv(rf_imp_sorted, "../data/results/server_quality_models/rf_variable_importance.csv")

# --- Variable Importance Plot ---
imp_df <- data.frame(
    Variable = names(rf_imp_sorted),
    Importance = rf_imp_sorted
)

p <- ggplot(imp_df, aes(x = reorder(Variable, Importance), y = Importance)) +
    geom_bar(stat = "identity", fill = "steelblue") +
    coord_flip() +
    theme_minimal() +
    labs(title = "Random Forest Variable Importance",
         x = "Variable", y = "%IncMSE")

ggsave("../data/results/server_quality_models/rf_variable_importance.png",
       p, width = 7, height = 5, bg = "white")

# --- Predict serve quality using RF ---
serve_profiles$serve_score_rf <- predict(rf, newdata = df_encoded)

# --- Linear Regression ---
lm_model <- lm(
    win_rate ~ avg_speed + sd_speed + ace_pct + location_entropy + as.factor(modal_location),
    data = serve_profiles
)
serve_profiles$serve_score_lm <- predict(lm_model)
summary(lm_model)

# --- Logistic Regression ---
logit_model <- glm(
    win_rate ~ avg_speed + sd_speed + ace_pct + location_entropy + as.factor(modal_location),
    data = serve_profiles,
    family = "binomial"
)
serve_profiles$serve_score_logit <- predict(logit_model, type = "response")
summary(logit_model)

# --- Normalize and rescore for weighted composite ---
# 1. Select features
selected_features <- c("avg_speed", "sd_speed", "ace_pct", "location_entropy")

# 2. Normalize RF importances to get weights
rf_weights <- rf_imp_sorted[selected_features]
rf_weights <- rf_weights / sum(rf_weights)

# 3. Rescale features and compute serve score
normalized_profiles <- serve_profiles %>%
    select(ServerName, win_rate, all_of(selected_features)) %>%
    mutate(across(all_of(selected_features), ~ rescale(.x, to = c(0, 1)))) %>%
    rowwise() %>%
    mutate(
        serve_score_rf_weighted = sum(c_across(all_of(selected_features)) * rf_weights[selected_features])
    ) %>%
    ungroup() %>%
    arrange(desc(serve_score_rf_weighted))

# --- Save final server quality scores ---
write.csv(normalized_profiles, "../data/results/server_quality_models/server_quality_scores.csv", row.names = FALSE)

# --- Optional: top 10 servers
print(head(normalized_profiles, 10))

