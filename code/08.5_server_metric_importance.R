# --- Setup ---
rm(list = ls())
library(tidyverse)
library(data.table)
library(recipes)
library(caret)
library(randomForest)
library(ggrepel)
library(scales)
library(ggplot2)
library(xgboost)
library(pheatmap)
library(viridis)

# --- Config ---
outcome_var <- "win_rate"  # "serve_efficiency" or "win_rate"

# --- Helper Functions ---
compute_entropy <- function(x) {
    p <- prop.table(table(x))
    -sum(p * log2(p))
}

get_serve_profiles <- function(df, serve_numbers) {
    df_sub <- df %>%
        filter(ServeNumber %in% serve_numbers) %>%
        mutate(modal_location = paste0("W", ServeWidth, "_D", ServeDepth)) %>%
        group_by(ServerName)

    serve_stats <- df_sub %>%
        summarise(
            avg_speed = mean(Speed_MPH, na.rm = TRUE),
            sd_speed = sd(Speed_MPH, na.rm = TRUE),
            location_entropy = compute_entropy(modal_location),
            win_rate = mean(PointWinner == ServeIndicator, na.rm = TRUE),
            serve_efficiency = mean((PointWinner == ServeIndicator) & (RallyCount <= 3), na.rm = TRUE),
            modal_location = names(sort(table(modal_location), decreasing = TRUE))[1],
            n_serves = n(),
            .groups = "drop"
        ) %>%
        filter(n_serves > 20)

    modal_dummies <- model.matrix(~ modal_location - 1, data = serve_stats) %>% as.data.frame()
    modal_scaled <- as.data.frame(scale(modal_dummies))

    X_cont <- serve_stats %>% select(avg_speed, sd_speed, location_entropy)
    X_scaled <- as.data.frame(scale(X_cont))
    X_final <- bind_cols(X_scaled, modal_scaled)

    df_final <- bind_cols(serve_stats %>% select(ServerName, !!sym(outcome_var)), X_final)
    colnames(df_final)[2] <- outcome_var  # rename column to match outcome_var
    return(df_final)
}

run_pipeline_models <- function(df_clean, serve_label, output_dir) {
    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
    set.seed(42)

    profiles <- get_serve_profiles(df_clean, serve_label)
    y <- profiles[[outcome_var]]
    X <- profiles %>% select(-ServerName, -all_of(outcome_var))
    df_model_scaled <- data.frame(X)
    df_model_scaled[[outcome_var]] <- y
    df_model_scaled <- df_model_scaled %>% relocate(all_of(outcome_var))

    cv_ctrl <- trainControl(method = "cv", number = 5, savePredictions = "final")

    # Linear Regression
    lm_model <- train(reformulate(colnames(X), response = outcome_var), data = df_model_scaled, method = "lm", trControl = cv_ctrl)
    pred_lm <- lm_model$pred %>% arrange(rowIndex) %>% pull(pred)
    resid_lm <- y - pred_lm

    # Random Forest
    rf_model <- train(reformulate(colnames(X), response = outcome_var), data = df_model_scaled, method = "rf", importance = TRUE, trControl = cv_ctrl)
    pred_rf <- rf_model$pred %>% arrange(rowIndex) %>% pull(pred)
    resid_rf <- y - pred_rf

    rf_importance <- varImp(rf_model)$importance
    if (!is.null(rownames(rf_importance))) {
        rf_weights <- rf_importance$Overall
        names(rf_weights) <- rownames(rf_importance)
        rf_weights <- rf_weights / sum(rf_weights, na.rm = TRUE)
        common_vars <- intersect(names(rf_weights), colnames(X))
        if (length(common_vars) > 0) {
            baseline_score <- rowSums(t(t(X[, common_vars, drop = FALSE]) * rf_weights[common_vars]))
        } else {
            baseline_score <- rep(NA_real_, nrow(X))
        }
    } else {
        baseline_score <- rep(NA_real_, nrow(X))
    }

    # XGBoost
    xgb_grid <- expand.grid(nrounds = 100, max_depth = 3, eta = 0.1, gamma = 0,
                            colsample_bytree = 1, min_child_weight = 1, subsample = 1)
    xgb_model <- train(reformulate(colnames(X), response = outcome_var), data = df_model_scaled, method = "xgbTree",
                       tuneGrid = xgb_grid, trControl = cv_ctrl, verbose = 0)
    pred_xgb <- xgb_model$pred %>% arrange(rowIndex) %>% pull(pred)
    resid_xgb <- y - pred_xgb

    rescale_signed <- function(x) {
        max_abs <- max(abs(x), na.rm = TRUE)
        if (max_abs == 0) return(rep(0, length(x)))
        return(x / max_abs)  # Range becomes [-1, 1], centered on 0
    }
    
    profiles_extended <- profiles %>%
        mutate(
            overperf_lm = rescale_signed(resid_lm),
            overperf_rf = rescale_signed(resid_rf),
            overperf_xgb = rescale_signed(resid_xgb),
            pred_lm = pred_lm,
            pred_rf = pred_rf,
            pred_xgb = pred_xgb,
            rf_weighted_baseline = rescale_signed(baseline_score)
        )

    if (!is.null(rownames(rf_importance))) {
        importance_df <- rf_importance %>%
            rownames_to_column("Variable") %>%
            arrange(desc(Overall))

        p_importance <- ggplot(importance_df, aes(x = reorder(Variable, Overall), y = Overall)) +
            geom_col(fill = "steelblue") +
            coord_flip() +
            labs(title = "Random Forest Variable Importance", x = "Variable", y = "Importance Score") +
            theme_minimal(base_size = 13)

        ggsave(file.path(output_dir, "rf_variable_importance.png"),
               p_importance, width = 7, height = 5.5, bg = "white")
    }

    write.csv(profiles_extended, file.path(output_dir, "serve_quality_all_models.csv"), row.names = FALSE)
    return(list(profiles = profiles_extended))
}

# --- Load & Prepare Training Data ---
df_train <- fread("../data/processed/scaled/usopen_subset_m_training.csv")
df_clean <- df_train %>%
    filter(!is.na(ServeWidth), !is.na(ServeDepth), ServeWidth != "", ServeDepth != "") %>%
    filter(ServeNumber %in% c(1, 2)) %>%
    mutate(
        location_bin = paste0("W", ServeWidth, "_D", ServeDepth),
        modal_location = paste0("W", ServeWidth, "_D", ServeDepth),
        ServerName = tolower(if_else(ServeIndicator == 1, player1, player2)),
        is_ace = if_else(ServeIndicator == 1, P1Ace, P2Ace),
        win_rate = PointWinner == ServeIndicator,
        serve_efficiency = (PointWinner == ServeIndicator) & (RallyCount <= 3)
    )

# --- Run Models ---
first_results   <- run_pipeline_models(df_clean, 1, "../data/results/server_quality_models/first_serve")
second_results  <- run_pipeline_models(df_clean, 2, "../data/results/server_quality_models/second_serve")
combined_results <- run_pipeline_models(df_clean, c(1, 2), "../data/results/server_quality_models/combined")

# --- Load Test Data ---
df_test <- fread("../data/processed/scaled/usopen_subset_m_testing.csv")
df_test_clean <- df_test %>%
    filter(!is.na(ServeWidth), !is.na(ServeDepth), ServeWidth != "", ServeDepth != "") %>%
    filter(ServeNumber %in% c(1, 2)) %>%
    mutate(
        modal_location = paste0("W", ServeWidth, "_D", ServeDepth),
        ServerName = tolower(if_else(ServeIndicator == 1, player1, player2)),
        win_rate = PointWinner == ServeIndicator,
        serve_efficiency = (PointWinner == ServeIndicator) & (RallyCount <= 3)
    )

# --- Evaluation ---
evaluate_model_metric <- function(test_df, model_df, metric_col) {
    df <- test_df %>%
        inner_join(model_df %>% select(ServerName, !!sym(metric_col)), by = "ServerName") %>%
        group_by(ServerName) %>%
        summarise(
            actual = mean(!!sym(outcome_var)),
            metric_score = first(!!sym(metric_col)),
            .groups = "drop"
        )
    
    r <- cor(df$actual, df$metric_score, use = "complete.obs")
    lm_fit <- lm(actual ~ metric_score, data = df)
    pval <- summary(lm_fit)$coefficients["metric_score", "Pr(>|t|)"]
    
    tibble(
        cor = r,
        p_value = pval,
        avg_metric = mean(df$metric_score, na.rm = TRUE),
        n = nrow(df)
    )
}

save_evals <- function(name, model_df) {
    metrics <- c("overperf_lm", "overperf_rf", "overperf_xgb", "rf_weighted_baseline")
    out <- lapply(metrics, function(m) evaluate_model_metric(df_test_clean, model_df, m) %>% mutate(Model = m))
    result <- bind_rows(out) %>% mutate(Data = name)
    write.csv(result, paste0("../data/results/server_quality_models/comparison/test_eval_", name, ".csv"), row.names = FALSE)
    return(result)
}

eval_first   <- save_evals("first", first_results$profiles)
eval_second  <- save_evals("second", second_results$profiles)
eval_combined <- save_evals("combined", combined_results$profiles)

# --- Scatterplots ---
plot_scatter <- function(model_df, label) {
    outcome_label <- ifelse(outcome_var == "serve_efficiency", "Serve Efficiency", "Win Percentage")
    
    models <- c("LM" = "overperf_lm", "RF" = "overperf_rf", "XGB" = "overperf_xgb", "Baseline" = "rf_weighted_baseline")
    
    scatter_data <- bind_rows(
        imap(models, ~ model_df %>%
                 inner_join(df_test_clean, by = "ServerName") %>%
                 group_by(ServerName) %>%
                 summarise(
                     win_rate = mean(PointWinner == ServeIndicator, na.rm = TRUE),
                     serve_efficiency = mean((PointWinner == ServeIndicator) & (RallyCount <= 3), na.rm = TRUE),
                     metric_score = first(!!sym(.x)),
                     Model = .y,
                     .groups = "drop"
                 ))
    )
    
    p <- ggplot(scatter_data, aes(x = metric_score, y = !!sym(outcome_var), color = Model)) +
        geom_point(size = 2) +
        geom_smooth(method = "lm", se = FALSE, linetype = "dashed") +
        theme_minimal(base_size = 12) +
        labs(
            title = paste(label, "- Overperformance vs", outcome_label),
            x = "Predicted Metric Score",
            y = paste("Actual", outcome_label)
        )
    
    ggsave(paste0("../data/results/server_quality_models/comparison/scatter_", tolower(label), ".png"),
           p, width = 10, height = 6, bg = "white")
    
    for (m in names(models)) {
        var <- models[m]
        data <- model_df %>%
            inner_join(df_test_clean, by = "ServerName") %>%
            group_by(ServerName) %>%
            summarise(
                win_rate = mean(PointWinner == ServeIndicator, na.rm = TRUE),
                serve_efficiency = mean((PointWinner == ServeIndicator) & (RallyCount <= 3), na.rm = TRUE),
                metric_score = mean(!!sym(var)),
                label = first(ServerName),
                .groups = "drop"
            )
        
        fit <- lm(formula = as.formula(paste(outcome_var, "~ metric_score")), data = data)
        
        data <- data %>%
            mutate(
                predicted = predict(fit),
                residual = .[[outcome_var]] - predicted,
                above_line = residual > 0
            )
        
        p_individual <- ggplot(data, aes(x = metric_score, y = !!sym(outcome_var))) +
            geom_line(aes(y = predicted), linetype = "dotted", color = "black") +
            geom_point(aes(color = above_line, alpha = abs(residual)), size = 2.5) +
            scale_color_manual(values = c("red", "darkgreen")) +
            geom_text_repel(aes(label = ifelse(abs(residual) > 0.05, label, "")),
                            size = 3, max.overlaps = Inf) +
            theme_minimal(base_size = 12) +
            labs(
                title = paste(label, "-", m, "Model"),
                x = "Server Quality (Model-Based)",
                y = paste("Actual", outcome_label)
            )
        
        ggsave(paste0("../data/results/server_quality_models/comparison/scatter_", tolower(label), "_", tolower(m), ".png"),
               p_individual, width = 9, height = 6, bg = "white")
    }
}

plot_scatter(first_results$profiles, "First Serve")
plot_scatter(second_results$profiles, "Second Serve")
plot_scatter(combined_results$profiles, "Combined")

#--------------------------------------------------
# Analyze Serve Behavior vs. Score State (Ordered by Mean Importance)
#--------------------------------------------------

# --- Add modal_location column ---
df_clean <- df_clean %>%
    mutate(modal_location = paste0("W", ServeWidth, "_D", ServeDepth))

# --- Join server quality metric ---
df_clean_enriched <- df_clean %>%
    left_join(combined_results$profiles %>%
                  select(ServerName, serve_score_rf_weighted = rf_weighted_baseline),
              by = "ServerName") %>%
    filter(!is.na(serve_score_rf_weighted))

# --- Categorize servers by quality ---
median_quality <- median(df_clean_enriched$serve_score_rf_weighted, na.rm = TRUE)
df_clean_enriched <- df_clean_enriched %>%
    mutate(quality_group = if_else(serve_score_rf_weighted >= median_quality, "High Quality", "Low Quality"))

# --- Order score states by increasing mean importance ---
state_order <- df_clean_enriched %>%
    group_by(state) %>%
    summarise(mean_importance = mean(importance, na.rm = TRUE), .groups = "drop") %>%
    arrange(mean_importance) %>%
    pull(state)

df_clean_enriched <- df_clean_enriched %>%
    mutate(state = factor(state, levels = state_order))

# --- Create output directory ---
#importance_dir <- "../data/results/server_quality_models/importance_behavior"
#dir.create(importance_dir, recursive = TRUE, showWarnings = FALSE)
# 1) Start with your full, desired ordering (may have duplicates)
library(dplyr)
library(ggplot2)

# 1) Define & clean up your desired ordering
custom_order <- c(
  "40-AD", "30-40", "15-40", "0-40",
  "40-30", "30-30", "15-30", "0-30",
  "40-15", "30-15", "15-15", "0-15",
  "40-0",  "30-0",  "15-0",  "0-0"
)
# remove duplicates just in case
unique_order <- custom_order[!duplicated(custom_order)]

# 2) Keep only the states you actually have
observed_states <- unique(df_clean_enriched$state)
score_levels   <- unique_order[custom_order %in% observed_states]

# 3) Filter out any rows that didn’t match and factor
df_plot <- df_clean_enriched %>%
  filter(state %in% score_levels) %>%     # drop those NAs
  mutate(state = factor(state, levels = score_levels))

# 4) Re‑make the boxplot
p_speed <- ggplot(df_plot, aes(x = state, y = Speed_MPH, fill = quality_group)) +
  geom_boxplot(outlier.size = 0.5, alpha = 0.75) +
  scale_x_discrete(drop = TRUE) +          # ensure no unused levels
  labs(
    title = "Serve Speed by Score State",
    x     = "Score State",
    y     = "Serve Speed (MPH)",
    fill  = "Server Type"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "right"
  )
p_speed
ggsave(file.path(importance_dir, "boxplot_speed_by_state.png"),
       p_speed, width = 9, height = 5.5, bg = "white")

# --- Boxplot: Ace Percentage ---
ace_pct_df <- df_clean_enriched %>%
    group_by(ServerName, state, quality_group) %>%
    summarise(ace_pct = mean(is_ace, na.rm = TRUE), .groups = "drop")

p_ace <- ggplot(ace_pct_df, aes(x = state, y = ace_pct, fill = quality_group)) +
    geom_boxplot(outlier.size = 0.5, alpha = 0.75) +
    labs(
        title = "Ace Percentage by Score State (Ordered by Increasing Importance)",
        x = "Score State",
        y = "Ace Percentage",
        fill = "Server Type"
    ) +
    theme_minimal(base_size = 12) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(file.path(importance_dir, "boxplot_acepct_by_state.png"),
       p_ace, width = 9, height = 5.5, bg = "white")

# --- Boxplot: Serve Location Entropy ---
entropy_df <- df_clean_enriched %>%
    group_by(ServerName, state, quality_group) %>%
    summarise(location_entropy = compute_entropy(modal_location), .groups = "drop")

p_entropy <- ggplot(entropy_df, aes(x = state, y = location_entropy, fill = quality_group)) +
    geom_boxplot(outlier.size = 0.5, alpha = 0.75) +
    labs(
        title = "Serve Location Entropy by Score State (Ordered by Increasing Importance)",
        x = "Score State",
        y = "Serve Location Entropy",
        fill = "Server Type"
    ) +
    theme_minimal(base_size = 12) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(file.path(importance_dir, "boxplot_entropy_by_state.png"),
       p_entropy, width = 9, height = 5.5, bg = "white")

# --- Heatmap of Point Importance by Modal Location ---
heatmap_df <- df_clean %>%
    group_by(modal_location) %>%
    summarise(mean_importance = mean(importance, na.rm = TRUE), .groups = "drop") %>%
    separate(modal_location, into = c("Width", "Depth"), sep = "_D") %>%
    mutate(Width = str_remove(Width, "W"))

heatmap_matrix <- heatmap_df %>%
    pivot_wider(names_from = Depth, values_from = mean_importance) %>%
    column_to_rownames("Width") %>%
    as.matrix()

# --- Convert matrix to data frame for ggplot ---
heatmap_df <- as.data.frame(as.table(heatmap_matrix))  # turn into long format
colnames(heatmap_df) <- c("ServeWidth", "ServeDepth", "Importance")

# heatmap of point importance by serve location
p <- ggplot(heatmap_df, aes(x = ServeDepth, y = ServeWidth, fill = Importance)) +
    geom_tile(color = "white") +
    scale_fill_viridis(name = "Mean Importance", option = "D") +
    theme_minimal() +
    labs(
        title = "Mean Point Importance by Serve Location",
        x = "Serve Depth",
        y = "Serve Width"
    ) +
    theme(
        axis.text = element_text(size = 10),
        plot.title = element_text(hjust = 0.5, face = "bold")
    )

# --- Save the plot ---
ggsave("../data/results/server_quality_models/importance_behavior/modal_location_heatmap.png",
       p, width = 7, height = 5, bg = "white")

