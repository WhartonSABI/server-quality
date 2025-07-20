rm(list=ls())
library(tidyverse)
library(data.table)
library(hms)

#-----------------------------------------------------------------------------------------------------

# change names of datasets if wanted
subset_m <- as.data.table(read.csv("../data/processed/usopen_subset_m_training.csv"))
subset_f <- as.data.table(read.csv("../data/processed/usopen_subset_f_training.csv"))

names(subset_m)

# wow no nas
colSums(is.na(subset_m))
colSums(is.na(subset_f))

#-----------------------------------------------------------------------------------------------------

# fix large gaps in elapsed time (male)

subset_m <- subset_m %>%
  arrange(match_id, PointNumber) %>%
  group_by(match_id) %>%
  mutate(
    lag_elapsed = lag(ElapsedSeconds),
    time_diff = ElapsedSeconds - lag_elapsed
  )

# --- Step 2: Calculate average time_diff per match (excluding big jumps) ---
avg_diffs <- subset_m %>%
  filter(!is.na(time_diff) & time_diff <= 1200) %>%
  group_by(match_id) %>%
  summarise(avg_time_diff = mean(time_diff, na.rm = TRUE), .groups = "drop")

# --- Step 3: Join avg diffs and create corrected column ---
subset_m <- subset_m %>%
  left_join(avg_diffs, by = "match_id") %>%
  arrange(match_id, PointNumber) %>%
  group_by(match_id) %>%
  mutate(
    ElapsedSeconds_fixed = ElapsedSeconds,
    flagged = time_diff > 1200
  )

# --- Step 4: Correct values where time_diff > 20 minutes (1200 seconds) ---
for (i in 2:nrow(subset_m)) {
  if (subset_m$match_id[i] == subset_m$match_id[i - 1] &&
      (subset_m$flagged[i] || subset_m$ElapsedSeconds_fixed[i] - subset_m$ElapsedSeconds_fixed[i - 1] > 1200)) {
    subset_m$ElapsedSeconds_fixed[i] <- subset_m$ElapsedSeconds_fixed[i - 1] + subset_m$avg_time_diff[i]
    subset_m$flagged[i] <- TRUE
  }
}

# --- Step 5: Cleanup ---
subset_m <- subset_m %>%
  ungroup() %>%
  select(-lag_elapsed, -time_diff, -avg_time_diff, -flagged) %>% 
  filter(!(ElapsedSeconds_fixed < 0))

# --- Optional: Check for remaining NAs ---
colSums(is.na(subset_m))

# --- Optional: Plot ---
ggplot(subset_m, aes(x = ElapsedSeconds_fixed)) +
  geom_histogram(color = "black", fill = "steelblue", na.rm = TRUE)

# --- Save cleaned dataset ---
write.csv(subset_m, "../data/processed/usopen_subset_m_training.csv", row.names = FALSE)

#-----------------------------------------------------------------------------------------------------

# fix large gaps in elapsed time (female)

# --- Step 1: Arrange and calculate time difference between points ---
subset_f <- subset_f %>%
  arrange(match_id, PointNumber) %>%
  group_by(match_id) %>%
  mutate(
    lag_elapsed = lag(ElapsedSeconds),
    time_diff = ElapsedSeconds - lag_elapsed
  )

# --- Step 2: Calculate average time_diff per match (excluding big jumps) ---
avg_diffs <- subset_f %>%
  filter(!is.na(time_diff) & time_diff <= 1200) %>%
  group_by(match_id) %>%
  summarise(avg_time_diff = mean(time_diff, na.rm = TRUE), .groups = "drop")

# --- Step 3: Join avg diffs and create corrected column ---
subset_f <- subset_f %>%
  left_join(avg_diffs, by = "match_id") %>%
  arrange(match_id, PointNumber) %>%
  group_by(match_id) %>%
  mutate(
    ElapsedSeconds_fixed = ElapsedSeconds,
    flagged = time_diff > 1200
  )

# --- Step 4: Correct values where time_diff > 20 minutes (1200 seconds) ---
for (i in 2:nrow(subset_f)) {
  if (subset_f$match_id[i] == subset_f$match_id[i - 1] &&
      (subset_f$flagged[i] || subset_f$ElapsedSeconds_fixed[i] - subset_f$ElapsedSeconds_fixed[i - 1] > 1200)) {
    subset_f$ElapsedSeconds_fixed[i] <- subset_f$ElapsedSeconds_fixed[i - 1] + subset_f$avg_time_diff[i]
    subset_f$flagged[i] <- TRUE
  }
}

# --- Step 5: Cleanup ---
subset_f <- subset_f %>%
  ungroup() %>%
  select(-lag_elapsed, -time_diff, -avg_time_diff, -flagged) %>%
  filter(!(ElapsedSeconds_fixed < 0))

# --- Optional: Check for remaining NAs ---
colSums(is.na(subset_f))

# --- Optional: Plot ---
ggplot(subset_f, aes(x = ElapsedSeconds_fixed)) +
  geom_histogram(color = "black", fill = "steelblue", na.rm = TRUE)

# --- Save cleaned dataset ---
write.csv(subset_f, "../data/processed/usopen_subset_f_training.csv", row.names = FALSE)

# ------------------- Standardize Selected Columns -------------------
# Define the columns to standardize
cols_to_standardize <- c(
  "Speed_MPH",
  "speed_ratio",
  "ElapsedSeconds_fixed",
  "p_server_beats_returner",
  "importance"
)

# Standardize and add new _z columns for males
subset_m <- subset_m %>% 
  mutate(
    across(
      all_of(cols_to_standardize),
      ~ as.numeric(scale(.x, center = TRUE, scale = TRUE)),
      .names = "{.col}_z"
    )
  )

# Standardize and add new _z columns for females
subset_f <- subset_f %>% 
  mutate(
    across(
      all_of(cols_to_standardize),
      ~ as.numeric(scale(.x, center = TRUE, scale = TRUE)),
      .names = "{.col}_z"
    )
  )

# Save scaled versions
write.csv(subset_m, "../data/processed/scaled/usopen_subset_m_training.csv", row.names = FALSE)
write.csv(subset_f, "../data/processed/scaled/usopen_subset_f_training.csv", row.names = FALSE)
