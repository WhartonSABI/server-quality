# --- Setup ---
rm(list = ls())
library(tidyverse)
library(data.table)
library(ggplot2)

# --- Config ---
outcome_type <- "win_pct_outcome"  # serve_efficiency_outcome or win_pct_outcome
tournament_gender_tag <- "wimbledon_males"  # wimbledon_males, wimbledon_females, usopen_males, usopen_females
column_name <- "overperf_xgb"  # serve_efficiency, win_rate, overperf_lm, overperf_rf, overperf_xgb

# --- Path ---
input_path <- file.path("../data/results/server_quality_models", outcome_type, tournament_gender_tag, "combined", "serve_quality_all_models.csv")

# --- Load Data ---
df <- fread(input_path)

# --- Check if column exists ---
if (!column_name %in% names(df)) {
    stop(paste("Column", column_name, "not found in the data."))
}

# --- Optional: Add index if no clear x-axis variable ---
df$resid_index <- 1:nrow(df)

# --- Compute median ---
metric_median <- median(df[[column_name]], na.rm = TRUE)

# --- Plot Histogram with Median Line ---
plot <- ggplot(df, aes_string(x = column_name)) +
    geom_histogram(color = "white", fill = "steelblue", bins = 25, alpha = 0.8) +
    geom_vline(xintercept = metric_median, linetype = "dashed", color = "red", linewidth = 1) +
    labs(
        title = paste("Distribution of", column_name),
        x = column_name,
        y = "Frequency"
    ) +
    scale_x_continuous(limits = c(-1, 1), expand = expansion(mult = c(0, 0.01))) +
    theme_minimal(base_size = 14)

# --- Save Plot ---
output_dir <- file.path("../data/results/distribution_plots", outcome_type, tournament_gender_tag)
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
ggsave(filename = file.path(output_dir, paste0("histogram_", column_name, ".png")), 
       plot = plot, width = 8, height = 6, bg = "white")

# --- Print Plot to Viewer ---
print(plot)


# --- Plot Residual Scatterplot ---
resid_plot <- ggplot(df, aes(x = resid_index, y = .data[[column_name]])) +
    geom_hline(yintercept = 0, linetype = "dotted", color = "gray40", linewidth = 1) +
    geom_point(color = "steelblue", alpha = 0.8, size = 2) +
    labs(
        title = paste("Residual Plot of", column_name),
        x = "Player Index",
        y = column_name
    ) +
    scale_y_continuous(limits = c(-1, 1), expand = expansion(mult = c(0.05, 0.05))) +
    theme_minimal(base_size = 14)

# --- Save Plot ---
output_dir <- file.path("../data/results/distribution_plots", outcome_type, tournament_gender_tag)
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
ggsave(filename = file.path(output_dir, paste0("residual_plot_", column_name, ".png")),
       plot = resid_plot, width = 8, height = 6, bg = "white")

# --- Print Plot ---
print(resid_plot)
