rm(list=ls())
# install.packages("fastDummies")
library(welo)
library(tidyverse)
library(data.table)
library(ggplot2)
library(hms)

#-----------------------------------------------------------------------------------------------------

subset_m <- as.data.table(read.csv("out_data/wimbledon_subset_m.csv"))
subset_f <- as.data.table(read.csv("out_data/wimbledon_subset_f.csv"))

names(subset_m)

# wow no nas
colSums(is.na(subset_m))
colSums(is.na(subset_f))

#-----------------------------------------------------------------------------------------------------

# Convert ELO to logistic (Bradley-Terry scale)
subset_m[, welo_p1_bt := 0.0057565 * player1_avg_welo]
subset_m[, welo_p2_bt := 0.0057565 * player2_avg_welo]

## male
subset_m <- subset_m %>%
  mutate(p_server_beats_returner <- ifelse(PointServer == 1,
                                           1 / (1 + exp(welo_p2_bt - welo_p1_bt)),
                                           1 / (1 + exp(welo_p1_bt - welo_p2_bt))))

# rename column in subset_m
setnames(subset_m, old = c("... <- NULL"),
         new = c("p_server_beats_returner"))

write.csv(subset_m, "out_data/wimbledon_subset_m.csv", row.names = FALSE)

#-----------------------------------------------------------------------------------------------------

# Convert ELO to logistic (Bradley-Terry scale)
subset_f[, welo_p1_bt := 0.0057565 * player1_avg_welo]
subset_f[, welo_p2_bt := 0.0057565 * player2_avg_welo]

## female
subset_f <- subset_f %>%
  mutate(p_server_beats_returner <- ifelse(PointServer == 1,
                                           1 / (1 + exp(welo_p1_bt - welo_p2_bt)),
                                           1 / (1 + exp(welo_p2_bt - welo_p1_bt))))

# rename column in subset_f
setnames(subset_f, old = c("... <- NULL"),
         new = c("p_server_beats_returner"))

write.csv(subset_f, "out_data/wimbledon_subset_f.csv", row.names = FALSE)


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

# graph histogram of ElapsedSeconds_fixed in subset_m
# ggplot(subset_m, aes(x = ElapsedSeconds_fixed)) +
#   geom_histogram(color = "black", fill = "steelblue", na.rm = TRUE)

write.csv(subset_m, "out_data/wimbledon_subset_m.csv", row.names = FALSE)

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

# remove all rows from subset_f that have NAs in ElapsedSeconds_fixed column
subset_f <- subset_f[!is.na(subset_f$ElapsedSeconds_fixed), ]

write.csv(subset_f, "out_data/wimbledon_subset_f.csv", row.names = FALSE)

#-----------------------------------------------------------------------------------------------------

subset_m <- as.data.table(read.csv("out_data/wimbledon_subset_m.csv"))
subset_f <- as.data.table(read.csv("out_data/wimbledon_subset_f.csv"))

## pct double faults per player per match

# 1) get every player who ever served
servers <- subset_m %>%
  filter(ServeNumber %in% c(1,2)) %>%
  mutate(ServerName = if_else(PointServer == 1, player1, player2)) %>%
  pull(ServerName) %>%
  unique()

# 2) function to compute df_pct for one server
compute_df_for <- function(name) {
  subset_m %>%
    filter(
      (PointServer == 1 & player1 == name) |
        (PointServer == 2 & player2 == name)
    ) %>%
    mutate(
      DF_flag = if_else(PointServer == 1, P1DoubleFault, P2DoubleFault)
    ) %>%
    group_by(match_id) %>%
    summarise(
      ServerName    = name,
      total_serves  = n(),
      double_faults = sum(DF_flag, na.rm = TRUE),
      df_pct_server        = double_faults / total_serves,
      .groups       = "drop"
    )
}

# 3) apply it to every server and bind the results
all_rates <- map_dfr(servers, compute_df_for)

print(all_rates)

## merge all_rates with subset_m based on match_id and ServerName
subset_m <- subset_m %>% 
  # identify who is serving this point
  mutate(ServerName = if_else(PointServer == 1, player1, player2)) %>% 
  
  # now the key columns exist in both tables
  left_join(all_rates, by = c("match_id", "ServerName"))

colSums(is.na(subset_m))

# 1) get every player who ever served
servers <- subset_f %>%
  filter(ServeNumber %in% c(1,2)) %>%
  mutate(ServerName = if_else(PointServer == 1, player1, player2)) %>%
  pull(ServerName) %>%
  unique()

# 2) function to compute df_pct for one server
compute_df_for <- function(name) {
  subset_f %>%
    filter(
      (PointServer == 1 & player1 == name) |
        (PointServer == 2 & player2 == name)
    ) %>%
    mutate(
      DF_flag = if_else(PointServer == 1, P1DoubleFault, P2DoubleFault)
    ) %>%
    group_by(match_id) %>%
    summarise(
      ServerName    = name,
      total_serves  = n(),
      double_faults = sum(DF_flag, na.rm = TRUE),
      df_pct_server        = double_faults / total_serves,
      .groups       = "drop"
    )
}

# 3) apply it to every server and bind the results
all_rates <- map_dfr(servers, compute_df_for)

print(all_rates)

## merge all_rates with subset_m based on match_id and ServerName
subset_f <- subset_f %>% 
  # identify who is serving this point
  mutate(ServerName = if_else(PointServer == 1, player1, player2)) %>% 
  
  # now the key columns exist in both tables
  left_join(all_rates, by = c("match_id", "ServerName"))

colSums(is.na(subset_f))

write.csv(subset_m, "out_data/wimbledon_subset_m.csv", row.names = FALSE)
write.csv(subset_f, "out_data/wimbledon_subset_f.csv", row.names = FALSE)

#-----------------------------------------------------------------------------------------------------

# standardize numeric cols

cols_to_standardize <- c(
  "Speed_MPH",
  "speed_ratio",
  "ElapsedSeconds_fixed",
  "df_pct_server",
  "p_server_beats_returner",
  "importance"
)

subset_m <- subset_m %>% 
  mutate(
    across(
      all_of(cols_to_standardize),
      ~ as.numeric(scale(.x, center = TRUE, scale = TRUE)),   # mean 0, sd 1
      .names = "{.col}_z"
    )
  )
subset_f <- subset_f %>%
  mutate(
    across(
      all_of(cols_to_standardize),
      ~ as.numeric(scale(.x, center = TRUE, scale = TRUE)),   # mean 0, sd 1
      .names = "{.col}_z"
    )
  )

names(subset_f)

# write the standardized data to csv
write.csv(subset_m, "out_data/scaled/wimbledon_subset_m_training.csv", row.names = FALSE)
write.csv(subset_f, "out_data/scaled/wimbledon_subset_f_training.csv", row.names = FALSE)
