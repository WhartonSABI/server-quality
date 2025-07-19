# --- Clear Environment & Load Libraries ---
rm(list = ls())
library(tidyverse)
library(data.table)

# --- Load Data ---
subset_m <- fread("out_data/scaled/wimbledon_subset_m_training.csv")
names(subset_m)

analyze_first_serve_strategy <- function(
    df,
    speed_var = "Speed_MPH",  # or "speed_ratio"
    p_first_in_num = NULL     # optional override; will compute if NULL
) {
  
  # --- Step 1: Filter relevant rows and compute first serve in ---
  df <- df %>%
    filter(ServeNumber %in% c(1, 2)) %>%
    mutate(first_serve_in = ifelse(ServeNumber == 1, 1, 0))
  
  # --- Step 2: First serve stats per match per server ---
  match_player_stats <- df %>%
    group_by(match_id, ServerName) %>%
    summarise(
      n_points = n(),
      n_first_in = sum(first_serve_in),
      .groups = "drop"
    ) %>%
    mutate(first_in_rate = n_first_in / n_points)
  
  # --- Step 3: Per-player averages across matches ---
  player_summary <- match_player_stats %>%
    group_by(ServerName) %>%
    summarise(
      n_matches = n(),
      avg_first_in_rate = mean(first_in_rate),
      .groups = "drop"
    )
  
  # --- Step 4: Compute weighted average ---
  if (is.null(p_first_in_num)) {
    p_first_in_num <- player_summary %>%
      mutate(weight = n_matches / sum(n_matches)) %>%
      summarise(prob_first_serve_in = sum(avg_first_in_rate * weight)) %>%
      pull(prob_first_serve_in)
  }
  cat("Weighted average P(first serve in):", round(p_first_in_num, 4), "\n\n")
  
  # --- Modes for categorical predictors ---
  mode_width <- df %>% count(ServeWidth) %>% arrange(desc(n)) %>% slice(1) %>% pull(ServeWidth)
  mode_depth <- df %>% count(ServeDepth) %>% arrange(desc(n)) %>% slice(1) %>% pull(ServeDepth)
  
  # --- Modeling datasets ---
  first_serve_df <- df %>% filter(ServeNumber == 1)
  second_serve_df <- df %>% filter(ServeNumber == 2)
  
  # --- Fit logistic models ---
  predictors <- c(speed_var, "ServeWidth", "ServeDepth", "importance_z", "df_pct_server_z", "p_server_beats_returner_z")
  formula <- as.formula(paste("serving_player_won ~", paste(predictors, collapse = " + ")))
  
  model_first <- glm(formula, data = first_serve_df, family = "binomial")
  model_second <- glm(formula, data = second_serve_df, family = "binomial")
  
  # --- Median z-values ---
  median_vals <- df %>%
    summarise(across(c("importance_z", "df_pct_server_z", "p_server_beats_returner_z"), median, na.rm = TRUE))
  
  # --- Define speed sequence and z-scores ---
  speed_seq <- if (speed_var == "Speed_MPH") {
    seq(80, 145, by = 1)
  } else {
    seq(0.55, 1.25, by = 0.01)
  }
  
  speed_mean <- mean(df[[speed_var]], na.rm = TRUE)
  speed_sd <- sd(df[[speed_var]], na.rm = TRUE)
  speed_z <- (speed_seq - speed_mean) / speed_sd
  
  # --- Prepare grid data for predictions ---
  grid_data <- tibble(
    !!speed_var := speed_seq,
    importance_z = median_vals$importance_z,
    df_pct_server_z = median_vals$df_pct_server_z,
    p_server_beats_returner_z = median_vals$p_server_beats_returner_z,
    ServeWidth = mode_width,
    ServeDepth = mode_depth
  )
  
  # --- Predict full P(win) with modeled second serve ---
  grid_data <- grid_data %>%
    mutate(
      p_win_first = predict(model_first, newdata = ., type = "response"),
      p_win_second = predict(model_second, newdata = ., type = "response"),
      p_win_total = p_win_first * p_first_in_num + p_win_second * (1 - p_first_in_num)
    )
  
  optimal_row <- grid_data %>% filter(p_win_total == max(p_win_total)) %>% slice(1)
  
  p1 <- ggplot(grid_data, aes_string(x = speed_var, y = "p_win_total")) +
    geom_line(size = 1.2, color = "blue") +
    geom_point(data = optimal_row, aes_string(x = speed_var, y = "p_win_total"), color = "red", size = 3) +
    geom_text(
      data = optimal_row,
      aes_string(label = sprintf("paste('Optimal:', round(%s), '%s')", speed_var, ifelse(speed_var == "Speed_MPH", " MPH", ""))),
      vjust = -1.2, size = 4, color = "red"
    ) +
    labs(
      title = "P(win) vs. Serve Speed (Full Model)",
      x = ifelse(speed_var == "Speed_MPH", "Serve Speed (MPH)", "Serve Speed Ratio"),
      y = "P(win)"
    ) +
    theme_minimal()
  
  print(p1)
  
  # --- Predict with constant P(win | second serve) ---
  p_win_second_constant <- mean(predict(model_second, type = "response"), na.rm = TRUE)
  
  grid_data <- tibble(
    !!speed_var := speed_seq,
    importance_z = median_vals$importance_z,
    df_pct_server_z = median_vals$df_pct_server_z,
    p_server_beats_returner_z = median_vals$p_server_beats_returner_z,
    ServeWidth = mode_width,
    ServeDepth = mode_depth
  )
  
  grid_data <- grid_data %>%
    mutate(
      p_win_first = predict(model_first, newdata = ., type = "response"),
      p_win_second = p_win_second_constant,
      p_win_total = p_win_first * p_first_in_num + p_win_second * (1 - p_first_in_num)
    )
  
  optimal_row <- grid_data %>% filter(p_win_total == max(p_win_total)) %>% slice(1)
  
  p2 <- ggplot(grid_data, aes_string(x = speed_var, y = "p_win_total")) +
    geom_line(size = 1.2, color = "blue") +
    geom_point(data = optimal_row, aes_string(x = speed_var, y = "p_win_total"), color = "red", size = 3) +
    geom_text(
      data = optimal_row,
      aes_string(label = sprintf("paste('Optimal:', round(%s), '%s')", speed_var, ifelse(speed_var == "Speed_MPH", " MPH", ""))),
      vjust = -1.2, size = 4, color = "red"
    ) +
    labs(
      title = "P(win) vs. Serve Speed (Constant 2nd Serve Win Rate)",
      x = ifelse(speed_var == "Speed_MPH", "Serve Speed (MPH)", "Serve Speed Ratio"),
      y = "P(win)"
    ) +
    theme_minimal()
  
  print(p2)
  
  invisible(NULL)
}

analyze_first_serve_strategy(subset_m, speed_var = "speed_ratio")

