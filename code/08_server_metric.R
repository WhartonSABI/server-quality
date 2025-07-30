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
    colnames(df_final)[2] <- outcome_var
    return(df_final)
}

evaluate_model_metric <- function(test_df, model_df, metric_col) {
    df <- test_df %>%
        inner_join(model_df %>% select(ServerName, !!sym(metric_col)), by = "ServerName") %>%
        group_by(ServerName) %>%
        summarise(
            actual = mean(!!sym(outcome_var)),
            metric_score = first(!!sym(metric_col)),
            .groups = "drop"
        )
    
    df <- df %>%
        mutate(across(c(actual, metric_score), scale))
    
    r <- cor(df$actual, df$metric_score, use = "complete.obs")
    lm_fit <- lm(actual ~ metric_score, data = df)
    pval <- summary(lm_fit)$coefficients["metric_score", "Pr(>|t|)"]
    rmse_val <- sqrt(mean((df$actual - df$metric_score)^2, na.rm = TRUE))
    
    tibble(
        cor = r,
        p_value = pval,
        rmse = rmse_val,
        avg_metric = mean(df$metric_score, na.rm = TRUE),
        n = nrow(df)
    )
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
    
    lm_model <- train(reformulate(colnames(X), response = outcome_var), data = df_model_scaled, method = "lm", trControl = cv_ctrl)
    pred_lm <- lm_model$pred %>% arrange(rowIndex) %>% pull(pred)
    resid_lm <- y - pred_lm
    
    rf_model <- train(reformulate(colnames(X), response = outcome_var), data = df_model_scaled, method = "rf", importance = TRUE, trControl = cv_ctrl)
    pred_rf <- rf_model$pred %>% arrange(rowIndex) %>% pull(pred)
    resid_rf <- y - pred_rf
    
    rf_importance <- varImp(rf_model)$importance
    rf_weights <- rf_importance$Overall
    names(rf_weights) <- rownames(rf_importance)
    rf_weights <- rf_weights / sum(rf_weights, na.rm = TRUE)
    common_vars <- intersect(names(rf_weights), colnames(X))
    baseline_score <- if (length(common_vars) > 0) {
        rowSums(t(t(X[, common_vars, drop = FALSE]) * rf_weights[common_vars]))
    } else {
        rep(NA_real_, nrow(X))
    }
    
    xgb_grid <- expand.grid(nrounds = 100, max_depth = 3, eta = 0.1, gamma = 0,
                            colsample_bytree = 1, min_child_weight = 1, subsample = 1)
    xgb_model <- train(reformulate(colnames(X), response = outcome_var), data = df_model_scaled, method = "xgbTree",
                       tuneGrid = xgb_grid, trControl = cv_ctrl, verbose = 0)
    pred_xgb <- xgb_model$pred %>% arrange(rowIndex) %>% pull(pred)
    resid_xgb <- y - pred_xgb
    
    rescale_signed <- function(x) {
        max_abs <- max(abs(x), na.rm = TRUE)
        if (max_abs == 0) return(rep(0, length(x)))
        return(x / max_abs)
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
    
    write.csv(profiles_extended, file.path(output_dir, "serve_quality_all_models.csv"), row.names = FALSE)
    return(list(profiles = profiles_extended))
}

evaluate_welo_baseline <- function(df_train, df_test, serve_numbers, model_name) {
    train <- df_train %>% filter(ServeNumber %in% serve_numbers)
    test <- df_test %>% filter(ServeNumber %in% serve_numbers)
    
    train_welo_df <- train %>%
        mutate(welo = if_else(ServeIndicator == 1, player1_avg_welo, player2_avg_welo)) %>%
        group_by(ServerName) %>%
        summarise(avg_welo = mean(welo, na.rm = TRUE), .groups = "drop")
    
    test_performance_df <- test %>%
        group_by(ServerName) %>%
        summarise(
            win_rate = mean(PointWinner == ServeIndicator, na.rm = TRUE),
            serve_efficiency = mean((PointWinner == ServeIndicator) & (RallyCount <= 3), na.rm = TRUE),
            .groups = "drop"
        )
    
    df <- inner_join(train_welo_df, test_performance_df, by = "ServerName") %>%
        drop_na()
    
    df <- df %>%
        mutate(
            avg_welo = scale(avg_welo)[, 1],
            win_rate = scale(win_rate)[, 1],
            serve_efficiency = scale(serve_efficiency)[, 1]
        )
    
    r <- cor(df[[outcome_var]], df$avg_welo)
    lm_fit <- lm(df[[outcome_var]] ~ avg_welo, data = df)
    pval <- summary(lm_fit)$coefficients["avg_welo", "Pr(>|t|)"]
    rmse_val <- sqrt(mean((df[[outcome_var]] - predict(lm_fit))^2))
    
    tibble(
        cor = r,
        p_value = pval,
        rmse = rmse_val,
        avg_metric = mean(df$avg_welo, na.rm = TRUE),
        n = nrow(df),
        Model = "avg_welo",
        Data = model_name
    )
}

save_evals <- function(name, model_df, df_test_clean, df_train_clean, serve_numbers) {
    metrics <- c("overperf_lm", "overperf_rf", "overperf_xgb", "rf_weighted_baseline")
    out <- lapply(metrics, function(m) evaluate_model_metric(df_test_clean, model_df, m) %>% mutate(Model = m))
    result <- bind_rows(out) %>% mutate(Data = name)
    
    # Add Welo baseline
    welo_result <- evaluate_welo_baseline(df_train_clean, df_test_clean, serve_numbers, name)
    result <- bind_rows(result, welo_result)
    
    write.csv(result, paste0("../data/results/server_quality_models/comparison/test_eval_", name, ".csv"), row.names = FALSE)
    return(result)
}

# --- Load Data ---
df_train <- fread("../data/processed/scaled/wimbledon_subset_m_training.csv")
df_test <- fread("../data/processed/scaled/wimbledon_subset_m_testing.csv")

# --- Preprocess ---
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

df_test_clean <- df_test %>%
    filter(!is.na(ServeWidth), !is.na(ServeDepth), ServeWidth != "", ServeDepth != "") %>%
    filter(ServeNumber %in% c(1, 2)) %>%
    mutate(
        modal_location = paste0("W", ServeWidth, "_D", ServeDepth),
        ServerName = tolower(if_else(ServeIndicator == 1, player1, player2)),
        win_rate = PointWinner == ServeIndicator,
        serve_efficiency = (PointWinner == ServeIndicator) & (RallyCount <= 3)
    )

# --- Run Models and Save Results ---
first_results    <- run_pipeline_models(df_clean, 1, "../data/results/server_quality_models/first_serve")
second_results   <- run_pipeline_models(df_clean, 2, "../data/results/server_quality_models/second_serve")
combined_results <- run_pipeline_models(df_clean, c(1, 2), "../data/results/server_quality_models/combined")

eval_first    <- save_evals("first", first_results$profiles, df_test_clean, df_clean, 1)
eval_second   <- save_evals("second", second_results$profiles, df_test_clean, df_clean, 2)
eval_combined <- save_evals("combined", combined_results$profiles, df_test_clean, df_clean, c(1, 2))


# --- Scatterplot Function for All Models Including avg_welo ---
plot_scatter_models <- function(df_test_clean, model_df, df_train_clean, serve_numbers, label) {
    outcome_label <- ifelse(outcome_var == "serve_efficiency", "Serve Efficiency", "Win Percentage")
    
    # Get avg_welo for serving players from training data
    train_welo_df <- df_train_clean %>%
        filter(ServeNumber %in% serve_numbers) %>%
        mutate(welo = if_else(ServeIndicator == 1, player1_avg_welo, player2_avg_welo)) %>%
        group_by(ServerName) %>%
        summarise(avg_welo = mean(welo, na.rm = TRUE), .groups = "drop")
    
    # Get actual test-time performance
    actual_df <- df_test_clean %>%
        filter(ServeNumber %in% serve_numbers) %>%
        group_by(ServerName) %>%
        summarise(
            actual = mean(!!sym(outcome_var), na.rm = TRUE),
            .groups = "drop"
        )
    
    # Combine all metrics
    all_df <- actual_df %>%
        inner_join(model_df %>% select(ServerName, overperf_lm, overperf_rf, overperf_xgb, rf_weighted_baseline), by = "ServerName") %>%
        inner_join(train_welo_df, by = "ServerName")
    
    # Standardize for fair plotting
    all_df_std <- all_df %>%
        mutate(across(c(actual, overperf_lm, overperf_rf, overperf_xgb, rf_weighted_baseline, avg_welo), scale)) %>%
        rename(
            LM = overperf_lm,
            RF = overperf_rf,
            XGB = overperf_xgb,
            Baseline = rf_weighted_baseline,
            Welo = avg_welo
        )
    
    metric_names <- c("LM", "RF", "XGB", "Baseline", "Welo")
    
    for (m in metric_names) {
        df_m <- all_df_std %>%
            select(ServerName, actual, metric_score = !!sym(m)) %>%
            mutate(Model = m)
        
        lm_fit <- lm(actual ~ metric_score, data = df_m)
        df_m <- df_m %>%
            mutate(predicted = predict(lm_fit),
                   residual = actual - predicted,
                   above_line = residual > 0)
        
        p <- ggplot(df_m, aes(x = metric_score, y = actual)) +
            geom_point(aes(color = above_line, alpha = abs(residual)), size = 2.5) +
            geom_line(aes(y = predicted), linetype = "dotted", color = "black") +
            scale_color_manual(values = c("red", "darkgreen")) +
            geom_text_repel(aes(label = ifelse(abs(residual) > 0.5, ServerName, "")),
                            size = 3, max.overlaps = Inf) +
            theme_minimal(base_size = 12) +
            labs(
                title = paste(label, "-", m, "Model"),
                x = paste("Server Quality (", m, ")"),
                y = paste("Actual", outcome_label)
            )
        
        ggsave(
            paste0("../data/results/server_quality_models/comparison/scatter_", tolower(label), "_", tolower(m), ".png"),
            p, width = 9, height = 6, bg = "white"
        )
    }
}

plot_scatter_models(df_test_clean, first_results$profiles, df_clean, 1, "First Serve")
plot_scatter_models(df_test_clean, second_results$profiles, df_clean, 2, "Second Serve")
plot_scatter_models(df_test_clean, combined_results$profiles, df_clean, c(1, 2), "Combined")


### everything below this was kept the same!
#--------------------------------------------------

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

