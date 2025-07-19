# --- Clear environment and load libraries ---
rm(list = ls())
library(tidyverse)
library(data.table)

# --- Load data ---
df <- fread("out_data/scaled/wimbledon_subset_m_training.csv")

names(df)

# --- Filter to only first serve attempts ---
df <- df %>%
  filter(ServeNumber %in% c(1, 2)) %>%
  mutate(first_serve_in = ifelse(ServeNumber == 1, 1, 0))

# --- Subset first and second serves ---
first_serve_df <- df %>% filter(ServeNumber == 1)
second_serve_df <- df %>% filter(ServeNumber == 2)

# --- Step 1: Compute player-level summaries ---
# First serve win %, in %, and average speed
first_summary <- first_serve_df %>%
  group_by(ServerName) %>%
  summarise(
    n_first = n(),
    avg_speed_mph = mean(Speed_MPH, na.rm = TRUE),
    first_win_pct = mean(serving_player_won, na.rm = TRUE),
    .groups = "drop"
  )

# First serve in rate (based on both ServeNumber == 1 and 2)
player_attempts <- df %>%
  group_by(ServerName) %>%
  summarise(
    n_total = n(),
    n_in = sum(first_serve_in),
    first_in_pct = n_in / n_total,
    .groups = "drop"
  )

# Second serve win rate
second_summary <- second_serve_df %>%
  group_by(ServerName) %>%
  summarise(
    second_win_pct = mean(serving_player_won, na.rm = TRUE),
    .groups = "drop"
  )

# Combine all summaries
player_level <- first_summary %>%
  inner_join(player_attempts, by = "ServerName") %>%
  inner_join(second_summary, by = "ServerName") %>%
  mutate(
    est_overall_win = first_in_pct * first_win_pct + (1 - first_in_pct) * second_win_pct
  )

# --- Step 2: Plot relationships ---
# First serve speed vs in-rate
p1 <- ggplot(player_level, aes(x = avg_speed_mph, y = first_in_pct)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "loess", se = FALSE, color = "blue") +
  labs(
    title = "First Serve Speed vs. First Serve In Percentage",
    x = "Average 1st Serve Speed (MPH)",
    y = "First Serve In %"
  ) +
  theme_minimal()

# First serve speed vs win rate (when in)
p2 <- ggplot(player_level, aes(x = avg_speed_mph, y = first_win_pct)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "loess", se = FALSE, color = "darkgreen") +
  labs(
    title = "First Serve Speed vs. Win % (on 1st Serve In)",
    x = "Average 1st Serve Speed (MPH)",
    y = "Win % on 1st Serve In"
  ) +
  theme_minimal()

# First serve speed vs estimated overall win %
p3 <- ggplot(player_level, aes(x = avg_speed_mph, y = est_overall_win)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "loess", se = FALSE, color = "purple") +
  labs(
    title = "First Serve Speed vs. Estimated Overall Win %",
    x = "Average 1st Serve Speed (MPH)",
    y = "Estimated Win %"
  ) +
  theme_minimal()

# --- Show plots ---
print(p1)
print(p2)
print(p3)

# --- Optional: Identify speed that maximizes estimated overall win ---
opt_row <- player_level %>%
  filter(est_overall_win == max(est_overall_win)) %>%
  slice(1)

cat("\nPlayer with optimal estimated overall win rate:\n")
print(opt_row %>% select(ServerName, avg_speed_mph, est_overall_win))
