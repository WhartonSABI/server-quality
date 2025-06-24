# --- Load libraries ---
rm(list=ls())
library(tidyverse)
library(data.table)
library(ggplot2)
library(car)
library(splines)

# --- Load US Open data ---
subset_m <- fread("../data/usopen_subset_m.csv")
subset_f <- fread("../data/usopen_subset_f.csv")

# --- Helper function to plot spline model + empirical ---
plot_spline_model <- function(df, model, speed_col, title, save_path) {
  coefs <- coef(model)
  speed_vals <- seq(min(df[[speed_col]], na.rm = TRUE),
                    max(df[[speed_col]], na.rm = TRUE),
                    length.out = 200)
  
  spline_basis <- as.data.frame(bs(speed_vals, degree = 3, df = 5))
  colnames(spline_basis) <- paste0("bs", 1:5)
  spline_coefs <- coefs[grep(paste0("bs\\(", speed_col), names(coefs))]
  spline_lp <- as.matrix(spline_basis[, 1:5]) %*% spline_coefs
  spline_prob <- plogis(spline_lp)
  
  spline_df <- data.frame(
    Speed = speed_vals,
    Probability = spline_prob,
    Source = "Spline Prediction"
  )
  
  optimal_point <- spline_df[which.max(spline_df$Probability), ]
  
  df <- df %>%
    mutate(server = if_else(PointServer == 1, player1_name, player2_name))
  player_point_counts <- df %>% count(server, name = "n_points")
  df <- left_join(df, player_point_counts, by = "server") %>%
    mutate(weight = 1 / n_points)
  
  empirical_df <- df %>%
    filter(!is.na(.data[[speed_col]])) %>%
    mutate(speed_bin = cut(.data[[speed_col]],
                           breaks = seq(floor(min(.data[[speed_col]], na.rm = TRUE)),
                                        ceiling(max(.data[[speed_col]], na.rm = TRUE)),
                                        by = ifelse(speed_col == "Speed_MPH", 5, 0.025)))) %>%
    group_by(speed_bin) %>%
    summarise(
      Speed = weighted.mean(.data[[speed_col]], w = weight, na.rm = TRUE),
      Probability = weighted.mean(serving_player_won == 1, w = weight, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(Source = "Empirical Win Rate (Weighted)")
  
  plot_df <- bind_rows(empirical_df, spline_df)
  
  ggplot(plot_df, aes(x = Speed, y = Probability, color = Source)) +
    geom_line(size = 1.2) +
    geom_point(data = optimal_point, aes(x = Speed, y = Probability),
               color = "blue", shape = 17, size = 3) +
    geom_text(data = optimal_point, aes(x = Speed, y = Probability,
                                        label = paste0("Max: ", round(Speed, 2))),
              vjust = -1.2, color = "blue", fontface = "bold") +
    labs(
      title = title,
      x = ifelse(speed_col == "Speed_MPH", "Serve Speed (MPH)", "Speed Ratio"),
      y = "Probability Server Wins",
      color = "Source"
    ) +
    theme_minimal() +
    scale_color_manual(values = c("Empirical Win Rate (Weighted)" = "black",
                                  "Spline Prediction" = "red"))
  
  ggsave(save_path, bg = "white", width = 8, height = 6, units = "in")
}

# --- Split subsets ---
m_first <- subset_m[ServeNumber == 1]
m_second <- subset_m[ServeNumber == 2]
f_first <- subset_f[ServeNumber == 1]
f_second <- subset_f[ServeNumber == 2]

# --- Function to fit spline models and plot ---
run_spline_group <- function(df, group_id, group_name) {
  # Fit spline model with Speed_MPH
  model_speed <- glm(serving_player_won ~ p_server_beats_returner + ElapsedSeconds_fixed + importance +
                       bs(Speed_MPH, degree = 3, df = 5) + factor(ServeWidth) + factor(ServeDepth),
                     data = df, family = "binomial")
  
  assign(paste0("us_", group_id, "_spline_speed"), model_speed, envir = .GlobalEnv)
  
  plot_spline_model(df, model_speed, "Speed_MPH",
                    title = paste("US Open — Spline vs. Empirical (Speed MPH) —", group_name),
                    save_path = paste0("../images/us_", group_id, "_spline_speed.png"))
  
  # Fit spline model with speed_ratio
  model_ratio <- glm(serving_player_won ~ p_server_beats_returner + ElapsedSeconds_fixed + importance +
                       bs(speed_ratio, degree = 3, df = 5) + factor(ServeWidth) + factor(ServeDepth),
                     data = df, family = "binomial")
  
  assign(paste0("us_", group_id, "_spline_ratio"), model_ratio, envir = .GlobalEnv)
  
  plot_spline_model(df, model_ratio, "speed_ratio",
                    title = paste("US Open — Spline vs. Empirical (Speed Ratio) —", group_name),
                    save_path = paste0("../images/us_", group_id, "_spline_ratio.png"))
}

# --- Run all 4 groups ---
run_spline_group(m_first, "m_first", "Males First Serve")
run_spline_group(m_second, "m_second", "Males Second Serve")
run_spline_group(f_first, "f_first", "Females First Serve")
run_spline_group(f_second, "f_second", "Females Second Serve")

# --- model summaries ---

us_model_names <- c(
  "us_m_first_spline_speed",
  "us_m_first_spline_ratio",
  "us_m_second_spline_speed",
  "us_m_second_spline_ratio",
  "us_f_first_spline_speed",
  "us_f_first_spline_ratio",
  "us_f_second_spline_speed",
  "us_f_second_spline_ratio"
)

# Loop through and print summaries
for (model_name in us_model_names) {
  cat("\n==============================\n")
  cat("Model Summary:", model_name, "\n")
  cat("==============================\n")
  print(summary(get(model_name)))
}

# --- Helper function to plot histograms of serve speed or ratio ---
plot_histogram <- function(df, group_id, group_name, var) {
  df <- df %>%
    mutate(server = if_else(PointServer == 1, player1_name, player2_name))
  
  player_point_counts <- df %>% count(server, name = "n_points")
  df <- left_join(df, player_point_counts, by = "server") %>%
    mutate(weight = 1 / n_points)
  
  ggplot(df, aes_string(x = var)) +
    geom_histogram(aes(weight = weight), bins = 30, fill = "skyblue", color = "black") +
    labs(
      title = paste("Distribution of", var, "—", group_name),
      x = ifelse(var == "Speed_MPH", "Serve Speed (MPH)", "Serve Speed Ratio"),
      y = "Weighted Count"
    ) +
    theme_minimal()
  
  ggsave(paste0("../images/us_", group_id, "_hist_", tolower(var), ".png"),
         width = 8, height = 6, units = "in", bg = "white")
}

# --- Create histograms for all 4 subsets ---
plot_histogram(m_first, "m_first", "Males First Serve", "Speed_MPH")
plot_histogram(m_first, "m_first", "Males First Serve", "speed_ratio")

plot_histogram(m_second, "m_second", "Males Second Serve", "Speed_MPH")
plot_histogram(m_second, "m_second", "Males Second Serve", "speed_ratio")

plot_histogram(f_first, "f_first", "Females First Serve", "Speed_MPH")
plot_histogram(f_first, "f_first", "Females First Serve", "speed_ratio")

plot_histogram(f_second, "f_second", "Females Second Serve", "Speed_MPH")
plot_histogram(f_second, "f_second", "Females Second Serve", "speed_ratio")

# --- Filtered second-serve subsets where speed_ratio <= 1 ---
m_second_leq1 <- m_second %>% filter(speed_ratio <= 1)
f_second_leq1 <- f_second %>% filter(speed_ratio <= 1)

# --- Refit and plot models for filtered male second serves ---
model_m_second_ratio_leq1 <- glm(serving_player_won ~ p_server_beats_returner + ElapsedSeconds_fixed + importance +
                                   bs(speed_ratio, degree = 3, df = 5) + factor(ServeWidth) + factor(ServeDepth),
                                 data = m_second_leq1, family = "binomial")
assign("m_second_spline_ratio_leq1", model_m_second_ratio_leq1, envir = .GlobalEnv)

plot_spline_model(m_second_leq1, model_m_second_ratio_leq1, "speed_ratio",
                  title = "Spline vs. Empirical (Speed Ratio ≤ 1) — Males Second Serve",
                  save_path = "../images/us_m_second_spline_ratio_leq1.png")

# --- Refit and plot models for filtered female second serves ---
model_f_second_ratio_leq1 <- glm(serving_player_won ~ p_server_beats_returner + ElapsedSeconds_fixed + importance +
                                   bs(speed_ratio, degree = 3, df = 5) + factor(ServeWidth) + factor(ServeDepth),
                                 data = f_second_leq1, family = "binomial")
assign("f_second_spline_ratio_leq1", model_f_second_ratio_leq1, envir = .GlobalEnv)

plot_spline_model(f_second_leq1, model_f_second_ratio_leq1, "speed_ratio",
                  title = "Spline vs. Empirical (Speed Ratio ≤ 1) — Females Second Serve",
                  save_path = "../images/us_f_second_spline_ratio_leq1.png")

# --- Optionally: print summaries ---
cat("\n==============================\n")
cat("Model Summary: m_second_spline_ratio_leq1\n")
cat("==============================\n")
print(summary(m_second_spline_ratio_leq1))

cat("\n==============================\n")
cat("Model Summary: f_second_spline_ratio_leq1\n")
cat("==============================\n")
print(summary(f_second_spline_ratio_leq1))

# --- Trimmed second serve data: keep only middle 90% of speed_ratio ---
get_middle_90 <- function(df) {
  quantiles <- quantile(df$speed_ratio, probs = c(0.05, 0.95), na.rm = TRUE)
  df %>% filter(speed_ratio >= quantiles[1], speed_ratio <= quantiles[2])
}

m_second_trimmed <- get_middle_90(m_second)
f_second_trimmed <- get_middle_90(f_second)

# --- Refit and plot model for trimmed male second serves ---
model_m_second_ratio_trimmed <- glm(serving_player_won ~ p_server_beats_returner + ElapsedSeconds_fixed + importance +
                                      bs(speed_ratio, degree = 3, df = 5) + factor(ServeWidth) + factor(ServeDepth),
                                    data = m_second_trimmed, family = "binomial")
assign("m_second_spline_ratio_trimmed", model_m_second_ratio_trimmed, envir = .GlobalEnv)

plot_spline_model(m_second_trimmed, model_m_second_ratio_trimmed, "speed_ratio",
                  title = "Spline vs. Empirical (Middle 90% Speed Ratio) — Males Second Serve",
                  save_path = "../images/us_m_second_spline_ratio_trimmed.png")

# --- Refit and plot model for trimmed female second serves ---
model_f_second_ratio_trimmed <- glm(serving_player_won ~ p_server_beats_returner + ElapsedSeconds_fixed + importance +
                                      bs(speed_ratio, degree = 3, df = 5) + factor(ServeWidth) + factor(ServeDepth),
                                    data = f_second_trimmed, family = "binomial")
assign("f_second_spline_ratio_trimmed", model_f_second_ratio_trimmed, envir = .GlobalEnv)

plot_spline_model(f_second_trimmed, model_f_second_ratio_trimmed, "speed_ratio",
                  title = "Spline vs. Empirical (Middle 90% Speed Ratio) — Females Second Serve",
                  save_path = "../images/us_f_second_spline_ratio_trimmed.png")

# --- Optionally: print summaries ---
cat("\n==============================\n")
cat("Model Summary: m_second_spline_ratio_trimmed\n")
cat("==============================\n")
print(summary(m_second_spline_ratio_trimmed))

cat("\n==============================\n")
cat("Model Summary: f_second_spline_ratio_trimmed\n")
cat("==============================\n")
print(summary(f_second_spline_ratio_trimmed))
