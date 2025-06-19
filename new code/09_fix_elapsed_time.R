rm(list=ls())
# install.packages("fastDummies")
library(welo)
library(tidyverse)
library(data.table)
library(ggplot2)
library(hms)

#-----------------------------------------------------------------------------------------------------

subset_m <- as.data.table(read.csv("../data/wimbledon_subset_m.csv"))
subset_f <- as.data.table(read.csv("../data/wimbledon_subset_f.csv"))

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

# avg difference in between points before the large jump (if any)
avg_diffs <- subset_m %>%
  filter(!is.na(time_diff) & ElapsedSeconds < 28800) %>%
  group_by(match_id) %>%
  summarise(avg_time_diff = mean(time_diff, na.rm = TRUE), .groups = "drop")

subset_m <- subset_m %>%
  left_join(avg_diffs, by = "match_id") %>%
  arrange(match_id, PointNumber) %>%
  group_by(match_id) %>%
  mutate(
    ElapsedSeconds_fixed = ElapsedSeconds,
    flagged = ElapsedSeconds > 28800
  )

for (i in 3:nrow(subset_m)) {
  if (subset_m$flagged[i] || subset_m$ElapsedSeconds_fixed[i] > 28800) {
    subset_m$ElapsedSeconds_fixed[i] <- subset_m$ElapsedSeconds_fixed[i - 1] + subset_m$avg_time_diff[i]
    subset_m$flagged[i] <- TRUE
  }
}

subset_m <- subset_m %>%
  ungroup() %>%
  select(-lag_elapsed, -time_diff, -avg_time_diff, -flagged)
colSums(is.na(subset_m))

write.csv(subset_m, "../data/wimbledon_subset_m.csv", row.names = FALSE)

#-----------------------------------------------------------------------------------------------------

# fix large gaps in elapsed time (female)
subset_f <- subset_f %>%
  arrange(match_id, PointNumber) %>%
  group_by(match_id) %>%
  mutate(
    lag_elapsed = lag(ElapsedSeconds),
    time_diff = ElapsedSeconds - lag_elapsed
  )

# avg difference in between points before the large jump (if any)
avg_diffs <- subset_f %>%
  filter(!is.na(time_diff) & ElapsedSeconds < 28800) %>%
  group_by(match_id) %>%
  summarise(avg_time_diff = mean(time_diff, na.rm = TRUE), .groups = "drop")

subset_f <- subset_f %>%
  left_join(avg_diffs, by = "match_id") %>%
  arrange(match_id, PointNumber) %>%
  group_by(match_id) %>%
  mutate(
    ElapsedSeconds_fixed = ElapsedSeconds,
    flagged = ElapsedSeconds > 28800
  )

for (i in 3:nrow(subset_f)) {
  if (subset_f$flagged[i] || subset_f$ElapsedSeconds_fixed[i] > 28800) {
    subset_f$ElapsedSeconds_fixed[i] <- subset_f$ElapsedSeconds_fixed[i - 1] + subset_f$avg_time_diff[i]
    subset_f$flagged[i] <- TRUE
  }
}

subset_f <- subset_f %>%
  ungroup() %>%
  select(-lag_elapsed, -time_diff, -avg_time_diff, -flagged)
colSums(is.na(subset_f))

write.csv(subset_f, "../data/wimbledon_subset_f.csv", row.names = FALSE)
