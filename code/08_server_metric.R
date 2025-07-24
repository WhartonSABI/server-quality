# --- Setup ---
rm(list = ls())
library(tidyverse)
library(data.table)
library(recipes)
library(randomForest)
library(ggrepel)
library(scales)
library(ggplot2)

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
    y <- profiles$win_rate
    
    # Standardize for LM and GLM
    X_scaled <- as.data.frame(scale(X))
    df_model_scaled <- cbind(win_rate = y, X_scaled) %>% as.data.frame()
    
    # Use unscaled for RF
    df_model <- cbind(win_rate = y, X) %>% as.data.frame()
    
    # --- Linear Regression (scaled) ---
    lm_model <- lm(win_rate ~ ., data = df_model_scaled)
    pred_lm <- predict(lm_model, newdata = X_scaled)
    resid_lm <- y - pred_lm
    
    # --- Logistic Regression (scaled) ---
    # binary outcome variable of whether serving player's win probability is > 0.5
    glm_model <- glm(win_rate > 0.5 ~ ., data = df_model_scaled, family = "binomial")
    pred_glm <- predict(glm_model, newdata = X_scaled, type = "response")
    resid_glm <- y - pred_glm
    
    # --- Random Forest (unscaled) ---
    rf_model <- randomForest(win_rate ~ ., data = df_model, importance = TRUE)
    pred_rf <- predict(rf_model, newdata = X)
    resid_rf <- y - pred_rf
    
    # --- Variable Importance and Baseline Score (use scaled X) ---
    rf_importance <- importance(rf_model, type = 1)
    imp_df <- data.frame(Variable = rownames(rf_importance), Importance = rf_importance[, 1])
    rf_weights <- imp_df$Importance / sum(imp_df$Importance)
    names(rf_weights) <- imp_df$Variable
    
    baseline_score <- rowSums(t(t(X_scaled) * rf_weights[colnames(X_scaled)]))
    
    ggsave(
        paste0(output_dir, "/rf_importance.png"),
        ggplot(imp_df, aes(x = reorder(Variable, Importance), y = Importance)) +
            geom_col(fill = "steelblue") + coord_flip() +
            theme_minimal(base_size = 12) +
            labs(title = "Random Forest Variable Importance", x = "", y = "Importance"),
        width = 7, height = 5, bg = "white"
    )
    
    profiles_extended <- profiles %>%
        mutate(
            overperf_lm = resid_lm,
            overperf_glm = resid_glm,
            overperf_rf = resid_rf,
            pred_lm = pred_lm,
            pred_glm = pred_glm,
            pred_rf = pred_rf,
            rf_weighted_baseline = baseline_score
        )
    
    write.csv(profiles_extended, paste0(output_dir, "/serve_quality_all_models.csv"), row.names = FALSE)
    return(list(profiles = profiles_extended))
}

# --- Load & Prepare Training Data ---
df_train <- fread("../data/processed/scaled/wimbledon_subset_m_training.csv")
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
df_test <- fread("../data/processed/scaled/wimbledon_subset_m_testing.csv")
df_test_clean <- df_test %>%
    filter(!is.na(ServeWidth), !is.na(ServeDepth), ServeWidth != "", ServeDepth != "") %>%
    filter(ServeNumber %in% c(1, 2)) %>%
    mutate(
        ServerName = tolower(if_else(ServeIndicator == 1, player1, player2)),
        won_point = PointWinner == ServeIndicator
    )

# --- Evaluation ---
evaluate_model_metric <- function(test_df, model_df, metric_col) {
    df <- test_df %>%
        inner_join(model_df %>% select(ServerName, !!sym(metric_col)), by = "ServerName") %>%
        group_by(ServerName) %>%
        summarise(actual_win_rate = mean(won_point), metric_score = first(!!sym(metric_col)), .groups = "drop") %>%
        summarise(
            cor = cor(actual_win_rate, metric_score, use = "complete.obs"),
            avg_actual = mean(actual_win_rate),
            avg_metric = mean(metric_score),
            n = n()
        )
    return(df)
}

save_evals <- function(name, model_df) {
    metrics <- c("overperf_lm", "overperf_glm", "overperf_rf", "rf_weighted_baseline")
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
    models <- c("LM" = "overperf_lm", "GLM" = "overperf_glm", "RF" = "overperf_rf", "Baseline" = "rf_weighted_baseline")
    
    scatter_data <- bind_rows(
        imap(models, ~ model_df %>%
                 inner_join(df_test_clean, by = "ServerName") %>%
                 group_by(ServerName) %>%
                 summarise(
                     win_rate = mean(won_point),
                     metric_score = first(!!sym(.x)),
                     Model = .y,
                     .groups = "drop"
                 ))
    )
    
    p <- ggplot(scatter_data, aes(x = metric_score, y = win_rate, color = Model)) +
        geom_point(size = 2) +
        geom_smooth(method = "lm", se = FALSE, linetype = "dashed") +
        theme_minimal(base_size = 12) +
        labs(title = paste(label, "- Overperformance vs Actual Win Rate"),
             x = "Predicted Metric Score", y = "Actual Win Rate")
    
    ggsave(paste0("../data/results/server_quality_models/comparison/scatter_", tolower(label), ".png"),
           p, width = 10, height = 6, bg = "white")
    
    for (m in names(models)) {
        var <- models[m]
        data <- model_df %>%
            inner_join(df_test_clean, by = "ServerName") %>%
            group_by(ServerName) %>%
            summarise(
                win_rate = mean(won_point),
                metric_score = mean(!!sym(var)),
                label = first(ServerName),
                .groups = "drop"
            )
        
        fit <- lm(win_rate ~ metric_score, data = data)
        data <- data %>%
            mutate(predicted = predict(fit),
                   residual = win_rate - predicted,
                   above_line = residual > 0)
        
        p_individual <- ggplot(data, aes(x = metric_score, y = win_rate)) +
            geom_line(aes(y = predicted), linetype = "dotted", color = "black") +
            geom_point(aes(color = above_line, alpha = abs(residual)), size = 2.5) +
            scale_color_manual(values = c("red", "darkgreen")) +
            geom_text_repel(aes(label = ifelse(abs(residual) > 0.05, label, "")),
                            size = 3, max.overlaps = Inf) +
            theme_minimal(base_size = 12) +
            labs(title = paste(label, "-", m, "Model"),
                 x = "Predicted Score (Model-Based)",
                 y = "Actual Win Rate")
        
        ggsave(paste0("../data/results/server_quality_models/comparison/scatter_", tolower(label), "_", tolower(m), ".png"),
               p_individual, width = 9, height = 6, bg = "white")
    }
}

plot_scatter(first_results$profiles, "First Serve")
plot_scatter(second_results$profiles, "Second Serve")
plot_scatter(combined_results$profiles, "Combined")
