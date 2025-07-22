# --- Setup ---
rm(list = ls())
library(tidyverse)
library(data.table)
library(cluster)
library(factoextra)
library(recipes)
library(pheatmap)
library(randomForest)
library(scales)
library(ggrepel)
library(gridExtra)
library(ggplot2)

# --- Helper functions ---
compute_entropy <- function(x) {
    p <- prop.table(table(x))
    -sum(p * log2(p))
}
get_mode <- function(x) {
    ux <- unique(x)
    ux[which.max(tabulate(match(x, ux)))]
}

get_serve_profiles <- function(df, serve_numbers) {
    df %>%
        filter(ServeNumber %in% serve_numbers) %>%
        group_by(ServerName) %>%
        summarise(
            avg_speed = mean(Speed_MPH, na.rm = TRUE),
            sd_speed = sd(Speed_MPH, na.rm = TRUE),
            win_rate = mean(PointWinner == ServeIndicator, na.rm = TRUE),
            ace_pct = mean(is_ace, na.rm = TRUE),
            location_entropy = compute_entropy(location_bin),
            n_serves = n(),
            .groups = "drop"
        ) %>%
        filter(n_serves > 20)
}

run_pipeline <- function(df_clean, serve_label, output_dir) {
    set.seed(42)
    profiles <- get_serve_profiles(df_clean, serve_label)
    
    # Create and scale model data
    model_data <- profiles %>% select(-win_rate, -n_serves)
    recipe_obj <- recipe(~ ., data = model_data) %>%
        update_role(ServerName, new_role = "id") %>%
        step_center(all_numeric_predictors()) %>%
        step_scale(all_numeric_predictors()) %>%
        prep()
    
    df_encoded <- bake(recipe_obj, new_data = NULL) %>%
        select(-ServerName) %>%
        bind_cols(win_rate = profiles$win_rate,
                  ServerName = profiles$ServerName,
                  n_serves = profiles$n_serves)
    
    df_model <- df_encoded %>%
        column_to_rownames("ServerName") %>%
        select(-n_serves)
    
    # Random forest
    rf <- randomForest(win_rate ~ ., data = df_model, importance = TRUE)
    imp <- importance(rf, type = 1)
    imp_sorted <- sort(imp[, 1], decreasing = TRUE)
    rf_weights <- imp_sorted / sum(imp_sorted)
    serve_score_rf <- predict(rf, newdata = df_model)
    
    features <- names(rf_weights)
    df_model_scaled <- df_model %>%
        select(-win_rate) %>%
        mutate(across(all_of(features), ~ rescale(.x, to = c(0, 1)), .names = "rescaled_{.col}"))
    
    weighted_score <- df_model_scaled %>%
        rowwise() %>%
        mutate(serve_score_rf_weighted = sum(c_across(starts_with("rescaled_")) * rf_weights[features])) %>%
        ungroup() %>%
        mutate(ServerName = rownames(df_model),
               serve_score_rf = serve_score_rf[rownames(df_model)]) %>%
        select(ServerName, serve_score_rf, serve_score_rf_weighted)
    
    # Linear model and overperformance
    lm_model <- lm(win_rate ~ ace_pct + avg_speed + sd_speed + location_entropy, data = profiles)
    profiles$expected_win_rate <- predict(lm_model, newdata = profiles)
    profiles$overperformance <- profiles$win_rate - profiles$expected_win_rate
    
    # Output full combined dataset
    output_df <- bind_cols(
        weighted_score,
        profiles %>% select(-win_rate, -n_serves),
        win_rate = profiles$win_rate,
        expected_win_rate = profiles$expected_win_rate,
        overperformance = profiles$overperformance,
        n_serves = profiles$n_serves
    )
    
    write.csv(output_df, paste0(output_dir, "/server_quality.csv"), row.names = FALSE)
    
    # Clustering
    set.seed(42)
    kfit <- kmeans(df_model %>% select(-win_rate), centers = 3, nstart = 25)
    clustered <- df_model %>%
        select(-win_rate) %>%
        mutate(cluster = factor(kfit$cluster),
               ServerName = rownames(df_model)) %>%
        pivot_longer(cols = -c(cluster, ServerName), names_to = "Feature", values_to = "Value")
    
    p_feat <- ggplot(clustered, aes(x = cluster, y = Value, fill = cluster)) +
        geom_boxplot(outlier.size = 0.5) +
        facet_wrap(~ Feature, scales = "free", ncol = 3) +
        theme_minimal(base_size = 12) +
        theme(legend.position = "none") +
        labs(title = "Feature Distributions by Cluster", x = "Cluster", y = "Standardized Value")
    ggsave(paste0(output_dir, "/feature_distributions_by_cluster.png"), p_feat, width = 10, height = 8, bg = "white")
    
    # RF importance plot
    imp_df <- data.frame(Variable = rownames(imp), Importance = imp[, 1])
    p_imp <- ggplot(imp_df, aes(x = reorder(Variable, Importance), y = Importance)) +
        geom_col(fill = "steelblue") +
        coord_flip() +
        theme_minimal(base_size = 12) +
        labs(title = "Random Forest Variable Importance", x = "Feature", y = "Importance")
    ggsave(paste0(output_dir, "/rf_variable_importance.png"), p_imp, width = 7, height = 5, bg = "white")
    
    # Overperformance plot
    p_over <- ggplot(profiles, aes(x = expected_win_rate, y = win_rate)) +
        geom_point(aes(color = overperformance), size = 2.5) +
        geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray") +
        geom_text_repel(data = subset(profiles, abs(overperformance) > 0.05),
                        aes(label = ServerName), size = 3) +
        scale_color_gradient2(low = "red", high = "green", mid = "gray90", midpoint = 0) +
        theme_minimal(base_size = 12) +
        labs(title = "Serve Overperformance vs Expected Win Rate",
             x = "Expected Win Rate", y = "Actual Win Rate", color = "Overperformance")
    ggsave(paste0(output_dir, "/serve_overperformance_plot.png"), p_over, width = 8, height = 6, bg = "white")
    
    return(list(serve_quality = output_df %>% rownames_to_column("ServerName")))
}

# --- Run pipelines ---
df_train <- fread("../data/processed/scaled/wimbledon_subset_m_training.csv")
df_clean <- df_train %>%
    filter(!is.na(ServeWidth), !is.na(ServeDepth), ServeWidth != "", ServeDepth != "") %>%
    filter(ServeNumber %in% c(1, 2)) %>%
    mutate(
        location_bin = paste0("W", ServeWidth, "_D", ServeDepth),
        ServerName = tolower(if_else(ServeIndicator == 1, player1, player2)),
        is_ace = if_else(ServeIndicator == 1, P1Ace, P2Ace)
    )

first_results <- run_pipeline(df_clean, 1, "../data/results/server_quality_models/first_serve")
second_results <- run_pipeline(df_clean, 2, "../data/results/server_quality_models/second_serve")
combined_results <- run_pipeline(df_clean, c(1, 2), "../data/results/server_quality_models/combined")

# --- Load and clean testing data ---
df_test <- fread("../data/processed/scaled/wimbledon_subset_m_testing.csv")
df_test_clean <- df_test %>%
    filter(!is.na(ServeWidth), !is.na(ServeDepth), ServeWidth != "", ServeDepth != "") %>%
    filter(ServeNumber %in% c(1, 2)) %>%
    mutate(
        ServerName = tolower(if_else(ServeIndicator == 1, player1, player2)),
        won_point = PointWinner == ServeIndicator
    )

# --- Evaluation ---
evaluate_on_test <- function(test_df, metric_df, metric_col) {
    test_df <- test_df %>%
        mutate(ServerName = tolower(trimws(ServerName)))
    
    df <- test_df %>%
        inner_join(metric_df %>% select(ServerName, !!sym(metric_col)), by = "ServerName")
    
    if (nrow(df) == 0) {
        return(tibble(cor_metric_vs_winrate = NA, avg_actual_win_rate = NA, avg_metric_score = NA, n_players = 0))
    }
    
    df %>%
        group_by(ServerName) %>%
        summarise(actual_win_rate = mean(won_point), metric_score = first(!!sym(metric_col)), .groups = "drop") %>%
        summarise(
            cor_metric_vs_winrate = cor(actual_win_rate, metric_score, use = "complete.obs"),
            avg_actual_win_rate = mean(actual_win_rate),
            avg_metric_score = mean(metric_score),
            n_players = n()
        )
}

first_results_serve_quality <- first_results$serve_quality %>% 
    select(-ServerName) %>% 
    rename(ServerName = `ServerName...1`,
           expected_win_rate = `expected_win_rate...9`,
           overperformance = `overperformance...10`) %>% 
    select(ServerName, serve_score_rf, serve_score_rf_weighted, avg_speed, sd_speed,
           ace_pct, location_entropy, expected_win_rate, overperformance, win_rate, n_serves)

second_results_serve_quality <- second_results$serve_quality %>% 
    select(-ServerName) %>% 
    rename(ServerName = `ServerName...1`,
           expected_win_rate = `expected_win_rate...9`,
           overperformance = `overperformance...10`) %>% 
    select(ServerName, serve_score_rf, serve_score_rf_weighted, avg_speed, sd_speed,
           ace_pct, location_entropy, expected_win_rate, overperformance, win_rate, n_serves)

combined_results_serve_quality <- combined_results$serve_quality %>% 
    select(-ServerName) %>% 
    rename(ServerName = `ServerName...1`,
           expected_win_rate = `expected_win_rate...9`,
           overperformance = `overperformance...10`) %>% 
    select(ServerName, serve_score_rf, serve_score_rf_weighted, avg_speed, sd_speed,
           ace_pct, location_entropy, expected_win_rate, overperformance, win_rate, n_serves)


first_eval <- evaluate_on_test(df_test_clean, first_results_serve_quality, "overperformance")
second_eval <- evaluate_on_test(df_test_clean, second_results_serve_quality, "overperformance")
combined_eval <- evaluate_on_test(df_test_clean, combined_results_serve_quality, "overperformance")

comparison <- bind_rows(
    first_eval %>% mutate(Method = "First Serve"),
    second_eval %>% mutate(Method = "Second Serve"),
    combined_eval %>% mutate(Method = "Combined")
)

write.csv(comparison, "../data/results/server_quality_models/comparison/compare_all_overlap_test_performance.csv", row.names = FALSE)
print(comparison)

# --- Scatterplot ---
get_scatter_data <- function(test_df, metric_df, metric_col, label) {
    test_df <- test_df %>%
        mutate(ServerName = tolower(trimws(ServerName)))
    
    test_df %>%
        inner_join(metric_df %>% select(ServerName, !!sym(metric_col)), by = "ServerName") %>%
        group_by(ServerName) %>%
        summarise(win_rate = mean(won_point), metric_score = first(!!sym(metric_col)), .groups = "drop") %>%
        mutate(Model = label)
}

df_first <- get_scatter_data(df_test_clean, first_results_serve_quality, "overperformance", "First Serve")
df_second <- get_scatter_data(df_test_clean, second_results_serve_quality, "overperformance", "Second Serve")
df_combined <- get_scatter_data(df_test_clean, combined_results_serve_quality, "overperformance", "Combined")

scatter_data <- bind_rows(df_first, df_second, df_combined)

p_scatter <- ggplot(scatter_data, aes(x = metric_score, y = win_rate)) +
    geom_point(color = "steelblue", size = 2) +
    geom_smooth(method = "lm", se = FALSE, color = "darkred", linetype = "dashed") +
    # geom_text_repel(data = subset(scatter_data, abs(win_rate - metric_score) > 0.1),
    #                 aes(label = ServerName), size = 3) +
    facet_wrap(~ Model) +
    theme_minimal(base_size = 12) +
    labs(
        title = "Overperformance Metric vs Actual Win Rate (Test Set)",
        x = "Overperformance (Train)",
        y = "Actual Win Rate"
    )

ggsave("../data/results/server_quality_models/comparison/server_overperformance_vs_winrate.png",
       p_scatter, width = 10, height = 6, bg = "white")


#---------------------------------------------
# same testing but using rf weighted server quality instead of based on overperformance from model
#---------------------------------------------

first_eval <- evaluate_on_test(df_test_clean, first_results_serve_quality, "serve_score_rf_weighted")
second_eval <- evaluate_on_test(df_test_clean, second_results_serve_quality, "serve_score_rf_weighted")
combined_eval <- evaluate_on_test(df_test_clean, combined_results_serve_quality, "serve_score_rf_weighted")

comparison <- bind_rows(
    first_eval %>% mutate(Method = "First Serve"),
    second_eval %>% mutate(Method = "Second Serve"),
    combined_eval %>% mutate(Method = "Combined")
)

write.csv(comparison, "../data/results/server_quality_models/comparison/compare_all_overlap_test_rf_weighted.csv", row.names = FALSE)
print(comparison)

# --- Scatterplot ---
df_first <- get_scatter_data(df_test_clean, first_results_serve_quality, "serve_score_rf_weighted", "First Serve")
df_second <- get_scatter_data(df_test_clean, second_results_serve_quality, "serve_score_rf_weighted", "Second Serve")
df_combined <- get_scatter_data(df_test_clean, combined_results_serve_quality, "serve_score_rf_weighted", "Combined")

scatter_data <- bind_rows(df_first, df_second, df_combined)

p_scatter <- ggplot(scatter_data, aes(x = metric_score, y = win_rate)) +
    geom_point(color = "steelblue", size = 2) +
    geom_smooth(method = "lm", se = FALSE, color = "darkred", linetype = "dashed") +
    # geom_text_repel(data = subset(scatter_data, abs(win_rate - metric_score) > 0.1),
    #                 aes(label = ServerName), size = 3) +
    facet_wrap(~ Model) +
    theme_minimal(base_size = 12) +
    labs(
        title = "Weighted Server Quality (weights from RF) vs Actual Win Rate (Test Set)",
        x = "Weighted Server Quality",
        y = "Actual Win Rate"
    )

ggsave("../data/results/server_quality_models/comparison/server_rf_weighted_vs_winrate.png",
       p_scatter, width = 10, height = 6, bg = "white")
