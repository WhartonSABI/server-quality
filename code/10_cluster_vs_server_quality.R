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
output_dir        <- file.path("../data/results/cluster_vs_quality", outcome_type, tournament_gender_tag)
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

# --- Load clustering + quality metrics ---
profiles_quality <- fread(file.path(base_dir_quality, "serve_quality_all_models.csv"))

profiles_quality <- profiles_quality %>% 
    mutate(cluster = as.factor(cluster))

# --- Define overperformance categories ---
model_cols <- c("performance_lm", "performance_rf", "performance_xgb", 
                "weighted_avg", "avg_welo_rescaled")

overperf_long <- profiles_quality %>%
    select(ServerName, cluster, all_of(model_cols)) %>%
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
cluster_summary <- profiles_quality %>%
    group_by(cluster) %>%
    summarise(across(all_of(model_cols), mean, .names = "avg_{.col}"), .groups = "drop")

write.csv(cluster_summary, file.path(output_dir, "average_quality_by_cluster.csv"), row.names = FALSE)


# --- Compute common y-axis range ---
combined_y <- c(profiles_quality$performance_lm, profiles_quality$performance_rf, profiles_quality$performance_xgb, 
                profiles_quality$weighted_avg, profiles_quality$vg_welo_rescaled)
y_limits <- range(combined_y, na.rm = TRUE)

# --- Individual overperformance plots ---
plot_perf_lm <- ggplot(profiles_quality, aes(x = cluster, y = performance_lm, fill = cluster)) +
    geom_boxplot(outlier.shape = NA) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    coord_cartesian(ylim = y_limits) +
    labs(title = "Linear Model Performance by Cluster",
         x = "Cluster", y = "Server Metric") +
    theme_minimal()

plot_perf_rf <- ggplot(profiles_quality, aes(x = cluster, y = performance_lm, fill = cluster)) +
    geom_boxplot(outlier.shape = NA) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    coord_cartesian(ylim = y_limits) +
    labs(title = "Random Forest Performance by Cluster",
         x = "Cluster", y = "Server Metric") +
    theme_minimal()

plot_perf_xgb <- ggplot(profiles_quality, aes(x = cluster, y = performance_xgb, fill = cluster)) +
    geom_boxplot(outlier.shape = NA) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    coord_cartesian(ylim = y_limits) +
    labs(title = "XGBoost Performance by Cluster",
         x = "Cluster", y = "Server Metric") +
    theme_minimal()

# --- Boxplot of weighted_avg by cluster ---
plot_baseline <- ggplot(profiles_quality, aes(x = cluster, y = weighted_avg, fill = cluster)) +
    geom_boxplot(outlier.shape = NA) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    coord_cartesian(ylim = y_limits) +
    labs(title = "Weighted Average Statistics by Cluster",
         x = "Cluster", y = "Weighted Average") +
    theme_minimal()

# --- Boxplot of avg_welo_rescaled by cluster ---
plot_welo <- ggplot(profiles_quality, aes(x = cluster, y = avg_welo_rescaled, fill = cluster)) +
    geom_boxplot(outlier.shape = NA) +
    coord_cartesian(ylim = y_limits) +
    labs(title = "Weighted Elo (Scaled) by Cluster",
         x = "Cluster", y = "Weighted Elo (scaled)") +
    theme_minimal()

# --- Combine plots in a grid ---
combined_plot <- (plot_perf_lm | plot_perf_rf) /
    (plot_perf_xgb | plot_baseline) /
    (plot_spacer() | plot_welo | plot_spacer())

ggsave(file.path(output_dir, "combined_boxplots_by_cluster_grid.png"),
       combined_plot, width = 14, height = 12, bg = "white")

