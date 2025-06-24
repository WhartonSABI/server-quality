# --- Load libraries ---
rm(list=ls())
library(tidyverse)
library(data.table)
library(ggplot2)
library(car)
library(splines)

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
