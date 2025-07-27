# --- Setup ---
rm(list = ls())
library(tidyverse)
library(data.table)
library(ggplot2)

# --- Config ---
tournament_gender_tag <- "wimbledon_males"  # wimbledon_males, wimbledon_females, usopen_males, usopen_females
outcome_type <- "win_pct_outcome"  # serve_efficiency_outcome, win_pct_outcome

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

# --- Boxplot of overperformance by cluster ---
boxplot_overperf <- ggplot(overperf_long, aes(x = cluster, y = residual, fill = cluster)) +
    geom_boxplot(outlier.shape = NA) +
    facet_wrap(~ model) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    labs(title = "Overperformance Residuals by Cluster",
         x = "Cluster", y = "Residual") +
    theme_minimal()

ggsave(file.path(output_dir, "residuals_by_cluster_boxplot.png"),
       boxplot_overperf, width = 10, height = 6, bg = "white")

# --- Boxplot of rf_weighted_baseline by cluster ---
boxplot_baseline <- ggplot(df_merged, aes(x = cluster, y = rf_weighted_baseline, fill = cluster)) +
    geom_boxplot(outlier.shape = NA) +
    labs(title = "Baseline Server Quality by Cluster",
         x = "Cluster", y = "RF Weighted Baseline") +
    theme_minimal()

ggsave(file.path(output_dir, "baseline_by_cluster_boxplot.png"),
       boxplot_baseline, width = 8, height = 6, bg = "white")
