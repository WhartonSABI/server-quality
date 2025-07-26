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

# --- Helper Functions ---
compute_entropy <- function(x) {
    p <- prop.table(table(x))
    -sum(p * log2(p))
}

get_serve_profiles <- function(df, serve_numbers) {
    df %>%
        filter(ServeNumber %in% serve_numbers) %>%
        group_by(ServerName) %>%
        summarise(
            avg_speed = mean(Speed_MPH, na.rm = TRUE),
            sd_speed = sd(Speed_MPH, na.rm = TRUE),
            location_entropy = compute_entropy(location_bin),
            win_rate = mean(PointWinner == ServeIndicator, na.rm = TRUE),
            serve_efficiency = mean((PointWinner == ServeIndicator) & (RallyCount <= 3), na.rm = TRUE),
            n_serves = n(),
            .groups = "drop"
        ) %>%
        filter(n_serves > 20)
}

run_pipeline_models <- function(df_clean, serve_label, output_dir) {
    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
    set.seed(42)
    
    profiles <- get_serve_profiles(df_clean, serve_label)
    X <- profiles %>% select(avg_speed, sd_speed, location_entropy)
    y <- profiles$serve_efficiency
    
    X_scaled <- as.data.frame(scale(X))
    df_model_scaled <- cbind(serve_efficiency = y, X_scaled)
    
    # CV control
    cv_ctrl <- trainControl(method = "cv", number = 5, savePredictions = "final")
    
    # Linear Regression
    lm_model <- train(serve_efficiency ~ ., data = df_model_scaled, method = "lm", trControl = cv_ctrl)
    pred_lm <- lm_model$pred %>% arrange(rowIndex) %>% pull(pred)
    resid_lm <- y - pred_lm
    
    # Random Forest
    rf_model <- train(serve_efficiency ~ ., data = df_model_scaled, method = "rf", importance = TRUE, trControl = cv_ctrl)
    pred_rf <- rf_model$pred %>% arrange(rowIndex) %>% pull(pred)
    resid_rf <- y - pred_rf
    
    rf_importance <- varImp(rf_model)$importance
    rf_weights <- rf_importance$Overall / sum(rf_importance$Overall)
    names(rf_weights) <- rownames(rf_importance)
    baseline_score <- rowSums(t(t(X_scaled) * rf_weights[colnames(X_scaled)]))
    
    # XGBoost
    xgb_grid <- expand.grid(nrounds = 100, max_depth = 3, eta = 0.1, gamma = 0, colsample_bytree = 1, min_child_weight = 1, subsample = 1)
    xgb_model <- train(serve_efficiency ~ ., data = df_model_scaled, method = "xgbTree", tuneGrid = xgb_grid, trControl = cv_ctrl, verbose = 0)
    pred_xgb <- xgb_model$pred %>% arrange(rowIndex) %>% pull(pred)
    resid_xgb <- y - pred_xgb
    
    profiles_extended <- profiles %>%
        mutate(
            overperf_lm = resid_lm,
            overperf_rf = resid_rf,
            overperf_xgb = resid_xgb,
            pred_lm = pred_lm,
            pred_rf = pred_rf,
            pred_xgb = pred_xgb,
            rf_weighted_baseline = baseline_score
        )
    
    # --- Save Random Forest Variable Importance Plot ---
    importance_df <- rf_importance %>%
        rownames_to_column("Variable") %>%
        arrange(desc(Overall))
    
    p_importance <- ggplot(importance_df, aes(x = reorder(Variable, Overall), y = Overall)) +
        geom_col(fill = "steelblue") +
        coord_flip() +
        labs(
            title = "Random Forest Variable Importance",
            x = "Variable",
            y = "Importance Score"
        ) +
        theme_minimal(base_size = 13)
    
    ggsave(file.path(output_dir, "rf_variable_importance.png"),
           p_importance, width = 7, height = 5.5, bg = "white")
    
    write.csv(profiles_extended, paste0(output_dir, "/serve_quality_all_models.csv"), row.names = FALSE)
    return(list(profiles = profiles_extended))
}

# --- Load & Prepare Training Data ---
df_train <- fread("../data/processed/scaled/usopen_subset_m_training.csv")
df_clean <- df_train %>%
    filter(!is.na(ServeWidth), !is.na(ServeDepth), ServeWidth != "", ServeDepth != "") %>%
    filter(ServeNumber %in% c(1, 2)) %>%
    mutate(
        location_bin = paste0("W", ServeWidth, "_D", ServeDepth),
        ServerName = tolower(if_else(ServeIndicator == 1, player1, player2)),
        is_ace = if_else(ServeIndicator == 1, P1Ace, P2Ace)
    )

# --- Run Models ---
first_results <- run_pipeline_models(df_clean, 1, "../data/results/server_quality_models/first_serve")
second_results <- run_pipeline_models(df_clean, 2, "../data/results/server_quality_models/second_serve")
combined_results <- run_pipeline_models(df_clean, c(1, 2), "../data/results/server_quality_models/combined")

# --- Load Test Data ---
df_test <- fread("../data/processed/scaled/usopen_subset_m_testing.csv")
df_test_clean <- df_test %>%
    filter(!is.na(ServeWidth), !is.na(ServeDepth), ServeWidth != "", ServeDepth != "") %>%
    filter(ServeNumber %in% c(1, 2)) %>%
    mutate(
        ServerName = tolower(if_else(ServeIndicator == 1, player1, player2)),
        won_point = PointWinner == ServeIndicator,
        efficient_serve = (PointWinner == ServeIndicator) & (RallyCount <= 3)
    )

# --- Evaluation ---
evaluate_model_metric <- function(test_df, model_df, metric_col) {
    df <- test_df %>%
        inner_join(model_df %>% select(ServerName, !!sym(metric_col)), by = "ServerName") %>%
        group_by(ServerName) %>%
        summarise(
            actual_serve_efficiency = mean(efficient_serve),
            metric_score = first(!!sym(metric_col)),
            .groups = "drop"
        ) %>%
        summarise(
            cor = cor(actual_serve_efficiency, metric_score, use = "complete.obs"),
            avg_metric = mean(metric_score),
            n = n()
        )
    return(df)
}

save_evals <- function(name, model_df) {
    metrics <- c("overperf_lm", "overperf_rf", "overperf_xgb", "rf_weighted_baseline")
    out <- lapply(metrics, function(m) evaluate_model_metric(df_test_clean, model_df, m) %>% mutate(Model = m))
    result <- bind_rows(out) %>% mutate(Data = name)
    write.csv(result, paste0("../data/results/server_quality_models/comparison/test_eval_", name, ".csv"), row.names = FALSE)
    return(result)
}

eval_first <- save_evals("first", first_results$profiles)
eval_second <- save_evals("second", second_results$profiles)
eval_combined <- save_evals("combined", combined_results$profiles)

# --- Scatterplots ---
plot_scatter <- function(model_df, label) {
    models <- c("LM" = "overperf_lm", "RF" = "overperf_rf", "XGB" = "overperf_xgb", "Baseline" = "rf_weighted_baseline")
    
    scatter_data <- bind_rows(
        imap(models, ~ model_df %>%
                 inner_join(df_test_clean, by = "ServerName") %>%
                 group_by(ServerName) %>%
                 summarise(
                     win_rate = mean(won_point),
                     serve_efficiency = mean((PointWinner == ServeIndicator) & (RallyCount <= 3), na.rm = TRUE),
                     metric_score = first(!!sym(.x)),
                     Model = .y,
                     .groups = "drop"
                 ))
    )
    
    p <- ggplot(scatter_data, aes(x = metric_score, y = serve_efficiency, color = Model)) +
        geom_point(size = 2) +
        geom_smooth(method = "lm", se = FALSE, linetype = "dashed") +
        theme_minimal(base_size = 12) +
        labs(title = paste(label, "- Overperformance vs Serve Efficiency"),
             x = "Predicted Metric Score", y = "Actual Serve Efficiency")
    
    ggsave(paste0("../data/results/server_quality_models/comparison/scatter_", tolower(label), ".png"),
           p, width = 10, height = 6, bg = "white")
    
    for (m in names(models)) {
        var <- models[m]
        data <- model_df %>%
            inner_join(df_test_clean, by = "ServerName") %>%
            group_by(ServerName) %>%
            summarise(
                win_rate = mean(won_point),
                serve_efficiency = mean((PointWinner == ServeIndicator) & (RallyCount <= 3), na.rm = TRUE),
                metric_score = mean(!!sym(var)),
                label = first(ServerName),
                .groups = "drop"
            )
        
        fit <- lm(serve_efficiency ~ metric_score, data = data)
        data <- data %>%
            mutate(predicted = predict(fit),
                   residual = serve_efficiency - predicted,
                   above_line = residual > 0)
        
        p_individual <- ggplot(data, aes(x = metric_score, y = serve_efficiency)) +
            geom_line(aes(y = predicted), linetype = "dotted", color = "black") +
            geom_point(aes(color = above_line, alpha = abs(residual)), size = 2.5) +
            scale_color_manual(values = c("red", "darkgreen")) +
            geom_text_repel(aes(label = ifelse(abs(residual) > 0.05, label, "")),
                            size = 3, max.overlaps = Inf) +
            theme_minimal(base_size = 12) +
            labs(title = paste(label, "-", m, "Model"),
                 x = "Server Quality (Model-Based)",
                 y = "Actual Serve Efficiency")
        
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
importance_dir <- "../data/results/server_quality_models/importance_behavior"
dir.create(importance_dir, recursive = TRUE, showWarnings = FALSE)

# --- Boxplot: Serve Speed ---
p_speed <- ggplot(df_clean_enriched, aes(x = state, y = Speed_MPH, fill = quality_group)) +
    geom_boxplot(outlier.size = 0.5, alpha = 0.75) +
    labs(
        title = "Serve Speed by Score State (Ordered by Increasing Importance)",
        x = "Score State",
        y = "Serve Speed (MPH)",
        fill = "Server Type"
    ) +
    theme_minimal(base_size = 12) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

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