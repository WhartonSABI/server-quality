# --- Load libraries ---
rm(list=ls())
library(tidyverse)
library(data.table)
library(ggplot2)
library(car)
library(splines)
library(dplyr)

# --- Load data ---
subset_m <- fread("../data/wimbledon_subset_m.csv")
subset_f <- fread("../data/wimbledon_subset_f.csv")

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
  
  # Save to global env with name
  assign(paste0(group_id, "_spline_speed"), model_speed, envir = .GlobalEnv)
  
  plot_spline_model(df, model_speed, "Speed_MPH",
                    title = paste("Spline vs. Empirical (Speed MPH) —", group_name),
                    save_path = paste0("../images/", group_id, "_spline_speed.png"))
  
  # Fit spline model with speed_ratio
  model_ratio <- glm(serving_player_won ~ p_server_beats_returner + ElapsedSeconds_fixed + importance +
                       bs(speed_ratio, degree = 3, df = 5) + factor(ServeWidth) + factor(ServeDepth),
                     data = df, family = "binomial")
  
  assign(paste0(group_id, "_spline_ratio"), model_ratio, envir = .GlobalEnv)
  
  plot_spline_model(df, model_ratio, "speed_ratio",
                    title = paste("Spline vs. Empirical (Speed Ratio) —", group_name),
                    save_path = paste0("../images/", group_id, "_spline_ratio.png"))
}

# --- Run all 4 groups ---
run_spline_group(m_first, "m_first", "Males First Serve")
run_spline_group(m_second, "m_second", "Males Second Serve")
run_spline_group(f_first, "f_first", "Females First Serve")
run_spline_group(f_second, "f_second", "Females Second Serve")

# --- Print all Wimbledon model summaries ---
wimbledon_model_names <- c(
  "m_first_spline_speed",
  "m_first_spline_ratio",
  "m_second_spline_speed",
  "m_second_spline_ratio",
  "f_first_spline_speed",
  "f_first_spline_ratio",
  "f_second_spline_speed",
  "f_second_spline_ratio"
)

for (model_name in wimbledon_model_names) {
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
  
  ggsave(paste0("../images/", group_id, "_hist_", tolower(var), ".png"),
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
                  save_path = "../images/m_second_spline_ratio_leq1.png")

# --- Refit and plot models for filtered female second serves ---
model_f_second_ratio_leq1 <- glm(serving_player_won ~ p_server_beats_returner + ElapsedSeconds_fixed + importance +
                                   bs(speed_ratio, degree = 3, df = 5) + factor(ServeWidth) + factor(ServeDepth),
                                 data = f_second_leq1, family = "binomial")
assign("f_second_spline_ratio_leq1", model_f_second_ratio_leq1, envir = .GlobalEnv)

plot_spline_model(f_second_leq1, model_f_second_ratio_leq1, "speed_ratio",
                  title = "Spline vs. Empirical (Speed Ratio ≤ 1) — Females Second Serve",
                  save_path = "../images/f_second_spline_ratio_leq1.png")

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
  quantiles <- quantile(df$Speed_MPH, probs = c(0.05, 0.95), na.rm = TRUE)
  df %>% filter(Speed_MPH >= quantiles[1], Speed_MPH <= quantiles[2])
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
                  save_path = "../images/m_second_spline_ratio_trimmed.png")

# --- Refit and plot model for trimmed female second serves ---
model_f_second_ratio_trimmed <- glm(serving_player_won ~ p_server_beats_returner + ElapsedSeconds_fixed + importance +
                                      bs(speed_ratio, degree = 3, df = 5) + factor(ServeWidth) + factor(ServeDepth),
                                    data = f_second_trimmed, family = "binomial")
assign("f_second_spline_ratio_trimmed", model_f_second_ratio_trimmed, envir = .GlobalEnv)

plot_spline_model(f_second_trimmed, model_f_second_ratio_trimmed, "speed_ratio",
                  title = "Spline vs. Empirical (Middle 90% Speed Ratio) — Females Second Serve",
                  save_path = "../images/f_second_spline_ratio_trimmed.png")

# --- Optionally: print summaries ---
cat("\n==============================\n")
cat("Model Summary: m_second_spline_ratio_trimmed\n")
cat("==============================\n")
print(summary(m_second_spline_ratio_trimmed))

cat("\n==============================\n")
cat("Model Summary: f_second_spline_ratio_trimmed\n")
cat("==============================\n")
print(summary(f_second_spline_ratio_trimmed))

# --- More-detailed analysis of second serves (weighted) -----------------

plot_win_prob_simple <- function(df,
                                 title_text = "P(win) vs. Second-Serve Speed Ratio",
                                 save_path  = NULL) {
  
  # 1  Add weights & filter ----------------------------------------------------
  df <- df %>% 
    filter(ServeNumber == 2, !is.na(serving_player_won), !is.na(speed_ratio)) %>% 
    mutate(server = if_else(PointServer == 1, player1_name, player2_name)) %>% 
    add_count(server, name = "n_points") %>% 
    mutate(weight = 1 / n_points)
  
  # 2  Weighted spline fit -----------------------------------------------------
  spline_mod <- glm(serving_player_won ~ bs(speed_ratio, df = 5),
                    data    = df,
                    family  = "binomial",
                    weights = weight)
  
  speed_grid <- seq(min(df$speed_ratio), max(df$speed_ratio), length.out = 200)
  design_mat <- model.matrix(~ bs(speed_ratio, df = 5),
                             data = data.frame(speed_ratio = speed_grid))
  pred_probs <- plogis(design_mat %*% coef(spline_mod))
  
  spline_df <- tibble(Speed = speed_grid,
                      Probability = pred_probs,
                      Source = "Spline Prediction")
  max_pt <- slice_max(spline_df, Probability, n = 1)
  
  # 3  Weighted empirical bins -------------------------------------------------
  empirical_df <- df %>% 
    mutate(speed_bin = cut(speed_ratio,
                           breaks = seq(floor(min(speed_ratio)),
                                        ceiling(max(speed_ratio)), by = 0.025))) %>% 
    group_by(speed_bin) %>% 
    summarise(
      Speed       = weighted.mean(speed_ratio, w = weight, na.rm = TRUE),
      Probability = weighted.mean(serving_player_won == 1, w = weight, na.rm = TRUE),
      .groups     = "drop") %>% 
    mutate(Source = "Empirical Win Rate (Weighted)")
  
  # 4  Plot --------------------------------------------------------------------
  plot_df <- bind_rows(spline_df, empirical_df)
  
  p <- ggplot(plot_df, aes(Speed, Probability, colour = Source)) +
    geom_line(data = subset(plot_df, Source == "Spline Prediction"), size = 1.2) +
    geom_point(data = subset(plot_df, Source == "Empirical Win Rate (Weighted)"), size = 2) +
    geom_point(data = max_pt, colour = "blue", shape = 17, size = 3, show.legend = FALSE) +
    geom_text(data = max_pt, aes(label = paste0("Max: ", round(Speed, 2))),
              vjust = -1.2, colour = "blue", fontface = "bold", show.legend = FALSE) +
    labs(title = title_text,
         x     = "Second-Serve Speed Ratio",
         y     = "P(Server Wins Point)",
         colour = NULL) +
    scale_colour_manual(values = c("Empirical Win Rate (Weighted)" = "black",
                                   "Spline Prediction"             = "red")) +
    theme_minimal()
  
  if (!is.null(save_path))
    ggsave(save_path, p, width = 8, height = 6, units = "in", bg = "white")
  
  invisible(p)
}

# Viewer only
plot_win_prob_simple(subset_m,
                     "Male Second Serves: P(win) vs Speed Ratio")

# Viewer + save
plot_win_prob_simple(subset_f,
                     "Female Second Serves: P(win) vs Speed Ratio",
                     save_path = "../images/wimbledon_female_second_speed_ratio.png")

plot_win_prob_simple(subset_m,
                     "Male Second Serves: P(win) vs Speed Ratio",
                     save_path = "../images/wimbledon_male_second_speed_ratio.png")

## middle 90% of data
m_second_trimmed <- get_middle_90(m_second)
f_second_trimmed <- get_middle_90(f_second)

plot_win_prob_simple(m_second_trimmed,
                     "Male Second Serves: P(win) vs Speed Ratio (Middle 90%)",
                     save_path = "../images/wimbledon_male_second_ratio_middle.png")

plot_win_prob_simple(f_second_trimmed,
                     "Female Second Serves: P(win) vs Speed Ratio (Middle 90%)",
                     save_path = "../images/wimbledon_female_second_ratio_middle.png")

##### speed MPH

plot_win_prob_speed <- function(df,
                                title_text = "P(win) vs. Second-Serve Speed",
                                save_path  = NULL) {
  
  # 1  Add weights & filter ----------------------------------------------------
  df <- df %>% 
    filter(ServeNumber == 2, !is.na(serving_player_won), !is.na(Speed_MPH)) %>% 
    mutate(server = if_else(PointServer == 1, player1_name, player2_name)) %>% 
    add_count(server, name = "n_points") %>% 
    mutate(weight = 1 / n_points)
  
  # 2  Weighted spline fit -----------------------------------------------------
  spline_mod <- glm(serving_player_won ~ bs(Speed_MPH, df = 5),
                    data    = df,
                    family  = "binomial",
                    weights = weight)
  
  speed_grid <- seq(min(df$Speed_MPH), max(df$Speed_MPH), length.out = 200)
  design_mat <- model.matrix(~ bs(Speed_MPH, df = 5),
                             data = data.frame(Speed_MPH = speed_grid))
  pred_probs <- plogis(design_mat %*% coef(spline_mod))
  
  spline_df <- tibble(Speed = speed_grid,
                      Probability = pred_probs,
                      Source = "Spline Prediction")
  max_pt <- slice_max(spline_df, Probability, n = 1)
  
  # 3  Weighted empirical bins -------------------------------------------------
  empirical_df <- df %>% 
    mutate(speed_bin = cut(Speed_MPH,
                           breaks = seq(floor(min(Speed_MPH)),
                                        ceiling(max(Speed_MPH)), by = 5))) %>% 
    group_by(speed_bin) %>% 
    summarise(
      Speed       = weighted.mean(Speed_MPH, w = weight, na.rm = TRUE),
      Probability = weighted.mean(serving_player_won == 1, w = weight, na.rm = TRUE),
      .groups     = "drop") %>% 
    mutate(Source = "Empirical Win Rate (Weighted)")
  
  # 4  Plot --------------------------------------------------------------------
  plot_df <- bind_rows(spline_df, empirical_df)
  
  p <- ggplot(plot_df, aes(Speed, Probability, colour = Source)) +
    geom_line(data = subset(plot_df, Source == "Spline Prediction"), size = 1.2) +
    geom_point(data = subset(plot_df, Source == "Empirical Win Rate (Weighted)"), size = 2) +
    geom_point(data = max_pt, colour = "blue", shape = 17, size = 3, show.legend = FALSE) +
    geom_text(data = max_pt, aes(label = paste0("Max: ", round(Speed, 2))),
              vjust = -1.2, colour = "blue", fontface = "bold", show.legend = FALSE) +
    labs(title = title_text,
         x     = "Second-Serve Speed (MPH)",
         y     = "P(Server Wins Point)",
         colour = NULL) +
    scale_colour_manual(values = c("Empirical Win Rate (Weighted)" = "black",
                                   "Spline Prediction"             = "red")) +
    theme_minimal()
  
  if (!is.null(save_path))
    ggsave(save_path, p, width = 8, height = 6, units = "in", bg = "white")
  
  invisible(p)
}

# Viewer only
plot_win_prob_speed(subset_m,
                    "Male Second Serves: P(win) vs Speed (MPH)")

# Viewer + save
plot_win_prob_speed(subset_f,
                    "Female Second Serves: P(win) vs Speed (MPH)",
                    save_path = "../images/wimbledon_female_second_speed_mph.png")

plot_win_prob_speed(subset_m,
                    "Male Second Serves: P(win) vs Speed (MPH)",
                    save_path = "../images/wimbledon_male_second_speed_mph.png")

## middle 90% of data
m_second_trimmed <- get_middle_90(m_second)
f_second_trimmed <- get_middle_90(f_second)

plot_win_prob_speed(m_second_trimmed,
                    "Male Second Serves: P(win) vs Speed (MPH) (Middle 90%)",
                    save_path = "../images/wimbledon_male_second_mph_middle.png")

plot_win_prob_speed(f_second_trimmed,
                    "Female Second Serves: P(win) vs Speed (MPH) (Middle 90%)",
                    save_path = "../images/wimbledon_female_second_mph_middle.png")

