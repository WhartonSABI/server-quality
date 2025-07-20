rm(list=ls())
# install.packages("welo")
library(welo)
library(tidyverse)
library(data.table)
library(ggplot2)
# install.packages("car")
library(car)

#-----------------------------------------------------------------------------------------------------

subset_m <- as.data.table(read.csv("../data/usopen_subset_m.csv"))
subset_f <- as.data.table(read.csv("../data/usopen_subset_f.csv"))

names(subset_m)

#-----------------------------------------------------------------------------------------------------

# --- More-detailed analysis of second serves (weighted) -----------------

m_second_in <- subset_m %>% 
  filter(
    ServeNumber == 2,                        # it was a 2nd serve
    (PointServer == 1 & P1DoubleFault == 0) | 
      (PointServer == 2 & P2DoubleFault == 0)   # …and it wasn’t a double fault
  )

# remove duplicate p_server_beats_returner in subset_f
setDT(subset_f)                       # no effect if it's already a data.table
subset_f <- subset_f[, .SD, .SDcols = !duplicated(names(subset_f))]

f_second_in <- subset_f %>% 
  filter(
    ServeNumber == 2,                        # it was a 2nd serve
    (PointServer == 1 & P1DoubleFault == 0) | 
      (PointServer == 2 & P2DoubleFault == 0)   # …and it wasn’t a double fault
  )


# --- Trimmed second serve data: keep only middle 90% of speed_ratio ---
get_middle_90 <- function(df) {
  quantiles <- quantile(df$speed_ratio, probs = c(0.05, 0.95), na.rm = TRUE)
  df %>% filter(speed_ratio >= quantiles[1], speed_ratio <= quantiles[2])
  quantiles <- quantile(df$Speed_MPH, probs = c(0.05, 0.95), na.rm = TRUE)
  df %>% filter(Speed_MPH >= quantiles[1], Speed_MPH <= quantiles[2])
}

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
plot_win_prob_simple(m_second_in,
                     "Male Second Serves: P(win | in) vs Speed Ratio")

# Viewer + save
plot_win_prob_simple(f_second_in,
                     "Female Second Serves: P(win | in) vs Speed Ratio",
                     save_path = "../images/in_usopen_female_second_speed_ratio.png")

plot_win_prob_simple(m_second_in,
                     "Male Second Serves: P(win) vs Speed Ratio",
                     save_path = "../images/in_usopen_male_second_speed_ratio.png")

## middle 90% of data
m_second_trimmed <- get_middle_90(m_second_in)
f_second_trimmed <- get_middle_90(f_second_in)

plot_win_prob_simple(m_second_trimmed,
                     "Male Second Serves: P(win | in) vs Speed Ratio (Middle 90%)",
                     save_path = "../images/in_usopen_male_second_ratio_middle.png")

plot_win_prob_simple(f_second_trimmed,
                     "Female Second Serves: P(win | in) vs Speed Ratio (Middle 90%)",
                     save_path = "../images/in_usopen_female_second_ratio_middle.png")

##### speed MPH

plot_win_prob_speed <- function(df,
                                title_text = "P(win | in) vs. Second-Serve Speed",
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
plot_win_prob_speed(m_second_in,
                    "Male Second Serves: P(win |in) vs Speed (MPH)")

# Viewer + save
plot_win_prob_speed(f_second_in,
                    "Female Second Serves: P(win | in) vs Speed (MPH)",
                    save_path = "../images/in_usopen_female_second_speed_mph.png")

plot_win_prob_speed(m_second_in,
                    "Male Second Serves: P(win | in) vs Speed (MPH)",
                    save_path = "../images/in_usopen_male_second_speed_mph.png")

## middle 90% of data
m_second_trimmed <- get_middle_90(m_second_in)
f_second_trimmed <- get_middle_90(f_second_in)

plot_win_prob_speed(m_second_trimmed,
                    "Male Second Serves: P(win | in) vs Speed (MPH) (Middle 90%)",
                    save_path = "../images/in_usopen_male_second_mph_middle.png")

plot_win_prob_speed(f_second_trimmed,
                    "Female Second Serves: P(win | in) vs Speed (MPH) (Middle 90%)",
                    save_path = "../images/in_usopen_female_second_mph_middle.png")

