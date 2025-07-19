# --- Clear Environment & Load Libraries ---
rm(list = ls())
library(tidyverse)
library(data.table)

# --- Load Data ---
subset_m <- fread("out_data/scaled/wimbledon_subset_m_training.csv")
names(subset_m)

# --- Step 1: Filter relevant rows ---
# ServeNumber == 1 means first serve was in, ServeNumber == 2 means it was out
# We only care about first serve attempts
df <- subset_m %>%
  filter(ServeNumber %in% c(1, 2)) %>%
  mutate(first_serve_in = ifelse(ServeNumber == 1, 1, 0))

# --- Step 2: Compute first serve stats per match per server ---
match_player_stats <- df %>%
  group_by(match_id, ServerName) %>%
  summarise(
    n_points = n(),
    n_first_in = sum(first_serve_in),
    .groups = "drop"
  ) %>%
  mutate(first_in_rate = n_first_in / n_points)

# --- Step 3: Compute per-player averages across matches ---
player_summary <- match_player_stats %>%
  group_by(ServerName) %>%
  summarise(
    n_matches = n(),
    avg_first_in_rate = mean(first_in_rate),
    .groups = "drop"
  )

# --- Step 4: Compute weighted average across all players ---
overall_weighted_avg <- player_summary %>%
  mutate(weight = n_matches / sum(n_matches)) %>%
  summarise(prob_first_serve_in = sum(avg_first_in_rate * weight)) %>%
  pull(prob_first_serve_in)

overall_weighted_avg





# --- Most common ServeWidth and ServeDepth ---
mode_width <- df %>% count(ServeWidth) %>% arrange(desc(n)) %>% slice(1) %>% pull(ServeWidth)
mode_depth <- df %>% count(ServeDepth) %>% arrange(desc(n)) %>% slice(1) %>% pull(ServeDepth)

# --- Modeling datasets ---
first_serve_df <- df %>% filter(ServeNumber == 1)
second_serve_df <- df %>% filter(ServeNumber == 2)

# --- Fit logistic models ---
predictors <- c("Speed_MPH", "ServeWidth", "ServeDepth", "importance_z", "df_pct_server_z", "p_server_beats_returner_z")
formula <- as.formula(paste("serving_player_won ~", paste(predictors, collapse = " + ")))

model_first <- glm(formula, data = first_serve_df, family = "binomial")
model_second <- glm(formula, data = second_serve_df, family = "binomial")

# --- Fixed values for other predictors (use median z-values) ---
median_vals <- df %>%
  summarise(across(all_of(predictors[4:6]), median, na.rm = TRUE))  # skip speed_ratio_z for now

# --- Create speed range and compute z-scores ---
speed_seq <- seq(80, 145, by = 1)
speed_mean <- mean(df$Speed_MPH, na.rm = TRUE)
speed_sd <- sd(df$Speed_MPH, na.rm = TRUE)
speed_z <- (speed_seq - speed_mean) / speed_sd

# --- Construct data for prediction ---
grid_data <- tibble(
  Speed_MPH = speed_seq,
  speed_ratio_z = (speed_seq / mean(df$Speed_MPH, na.rm = TRUE) - 1),  # use ratio to player average
  importance_z = median_vals$importance_z,
  df_pct_server_z = median_vals$df_pct_server_z,
  p_server_beats_returner_z = median_vals$p_server_beats_returner_z,
  ServeWidth = mode_width,
  ServeDepth = mode_depth
)

# --- Predict win probabilities ---
p_first_in <- 0.6481901

grid_data <- grid_data %>%
  mutate(
    p_win_first = predict(model_first, newdata = ., type = "response"),
    p_win_second = predict(model_second, newdata = ., type = "response"),
    p_win_total = p_win_first * p_first_in + p_win_second * (1 - p_first_in)
  )

# --- Find optimal serve speed ---
optimal_row <- grid_data %>% filter(p_win_total == max(p_win_total)) %>% slice(1)

# --- Plot ---
library(ggplot2)

ggplot(grid_data, aes(x = Speed_MPH, y = p_win_total)) +
  geom_line(size = 1.2, color = "blue") +
  geom_point(data = optimal_row, aes(x = Speed_MPH, y = p_win_total), color = "red", size = 3) +
  geom_text(
    data = optimal_row,
    aes(x = Speed_MPH, y = p_win_total, label = sprintf("Optimal: %.0f MPH", Speed_MPH)),
    vjust = -1.2, hjust = 0.5, size = 4, color = "red"
  ) +
  labs(
    title = "Serve Speed vs. Overall Point Win Probability",
    x = "Serve Speed (MPH)",
    y = "P(win)",
    caption = "Holding serve placement, importance, df_pct, and opponent quality constant"
  ) +
  theme_minimal()
