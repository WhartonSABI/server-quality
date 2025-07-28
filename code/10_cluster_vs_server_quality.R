# --- Setup ---
rm(list = ls())
library(tidyverse)
library(data.table)
library(ggplot2)
library(patchwork) # for combining plots

# --- Config ---
tournament_gender_tag <- "wimbledon_males"  # wimbledon_males, wimbledon_females, usopen_males, usopen_females
outcome_type <- "serve_efficiency_outcome"  # serve_efficiency_outcome, win_pct_outcome

# --- Paths ---
base_dir_quality  <- file.path("../data/results/server_quality_models", outcome_type, tournament_gender_tag, "combined")
base_dir_cluster  <- file.path("../data/results/clustering", tournament_gender_tag)
output_dir        <- file.path("../data/results/cluster_vs_quality", outcome_type, tournament_gender_tag)
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

# --- Load clustering + quality metrics ---
profiles_quality <- fread(file.path(base_dir_quality, "serve_quality_all_models.csv"))
profiles_clusters <- fread(file.path(base_dir_cluster, "combined_serves_kmeans_cluster_assignments.csv"))

profiles_clusters <- profiles_clusters %>%
    select(ServerName, cluster) %>% 
    mutate(ServerName = tolower(ServerName))

# --- Merge clusters and quality ---
df_merged <- profiles_quality %>%
    inner_join(profiles_clusters, by = "ServerName") %>%
    mutate(cluster = as.factor(cluster))

# --- Define overperformance categories ---
model_cols <- c("overperf_lm", "overperf_rf", "overperf_xgb")

overperf_long <- df_merged %>%
    select(ServerName, cluster, all_of(model_cols), rf_weighted_baseline) %>%
    pivot_longer(cols = all_of(model_cols), names_to = "model", values_to = "residual") %>%
    mutate(performance = case_when(
        residual > 0  ~ "Overperformer",
        residual < 0  ~ "Underperformer",
        TRUE          ~ "Neutral"
    ))

# --- Plot: Proportion of Over/Under/Neutral in each cluster ---
plot_performance_distribution <- ggplot(overperf_long, aes(x = cluster, fill = performance)) +
    geom_bar(position = "fill") +
    facet_wrap(~ model) +
    scale_y_continuous(labels = scales::percent_format()) +
    scale_fill_manual(values = c("Overperformer" = "darkgreen", "Underperformer" = "red", "Neutral" = "gray")) +
    labs(title = "Performance Distribution by Cluster and Model",
         y = "Proportion", x = "Cluster") +
    theme_minimal()

ggsave(file.path(output_dir, "performance_distribution_by_cluster.png"),
       plot_performance_distribution, width = 10, height = 6, bg = "white")

# --- Summary: average server quality score by cluster ---
score_cols <- c("overperf_lm", "overperf_rf", "overperf_xgb", "rf_weighted_baseline")

cluster_summary <- df_merged %>%
    group_by(cluster) %>%
    summarise(across(all_of(score_cols), mean, .names = "avg_{.col}"), .groups = "drop")

write.csv(cluster_summary, file.path(output_dir, "average_quality_by_cluster.csv"), row.names = FALSE)


# --- Compute common y-axis range ---
combined_y <- c(df_merged$overperf_lm, df_merged$overperf_rf, df_merged$overperf_xgb, df_merged$rf_weighted_baseline)
y_limits <- range(combined_y, na.rm = TRUE)

# --- Individual overperformance plots ---
plot_overperf_lm <- ggplot(df_merged, aes(x = cluster, y = overperf_lm, fill = cluster)) +
    geom_boxplot(outlier.shape = NA) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    coord_cartesian(ylim = y_limits) +
    labs(title = "Linear Model Overperformance by Cluster",
         x = "Cluster", y = "Residual") +
    theme_minimal()

plot_overperf_rf <- ggplot(df_merged, aes(x = cluster, y = overperf_rf, fill = cluster)) +
    geom_boxplot(outlier.shape = NA) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    coord_cartesian(ylim = y_limits) +
    labs(title = "Random Forest Overperformance by Cluster",
         x = "Cluster", y = "Residual") +
    theme_minimal()

plot_overperf_xgb <- ggplot(df_merged, aes(x = cluster, y = overperf_xgb, fill = cluster)) +
    geom_boxplot(outlier.shape = NA) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    coord_cartesian(ylim = y_limits) +
    labs(title = "XGBoost Overperformance by Cluster",
         x = "Cluster", y = "Residual") +
    theme_minimal()

# --- Boxplot of rf_weighted_baseline by cluster ---
plot_baseline <- ggplot(df_merged, aes(x = cluster, y = rf_weighted_baseline, fill = cluster)) +
    geom_boxplot(outlier.shape = NA) +
    coord_cartesian(ylim = y_limits) +
    labs(title = "Baseline Server Quality by Cluster",
         x = "Cluster", y = "RF Weighted Baseline") +
    theme_minimal()

# --- Combine plots in 2x2 grid ---
combined_plot <- (plot_overperf_lm | plot_overperf_rf) /
    (plot_overperf_xgb | plot_baseline)

ggsave(file.path(output_dir, "combined_boxplots_by_cluster_grid.png"),
       combined_plot, width = 14, height = 12, bg = "white")

