rm(list=ls())
setwd("C:/UPenn/research/WSABI summer 2025/Research/brainstorming")

library(rio)
library(data.table)
library(ggplot2)
library(dplyr)
library(tidyverse)

#-----------------------------------------------------------------------------------------------------

wimbledon_2021_matches <- as.data.table(read.csv("2021-wimbledon-matches.csv"))
names(wimbledon_2021_matches)

colSums(is.na(wimbledon_2021_matches))

wimbledon_2021_points <- as.data.table(read.csv("2021-wimbledon-points.csv"))
names(wimbledon_2021_points)

head(wimbledon_2021_points)
colSums(is.na(wimbledon_2021_points))

# remove all cols with NAs
wimbledon_2021_matches <- wimbledon_2021_matches %>%
  select(where(~ all(!is.na(.))))
names(wimbledon_2021_matches)

wimbledon_2021_points <- wimbledon_2021_points %>%
  select(where(~ all(!is.na(.))))
names(wimbledon_2021_points)

wimbledon_2021 <- left_join(wimbledon_2021_points, wimbledon_2021_matches, by = "match_id")
names(wimbledon_2021)
length(unique(c(wimbledon_2021$player1, wimbledon_2021$player2))) # 256 unique players for wimbledon 2024

write.csv(wimbledon_2021, "wimbledon_2021_combined.csv", row.names = FALSE)


#-----------------------------------------------------------------------------------------------------

wimbledon_2023_matches <- as.data.table(read.csv("2023-wimbledon-matches.csv"))
names(wimbledon_2023_matches)

colSums(is.na(wimbledon_2023_matches))

wimbledon_2023_points <- as.data.table(read.csv("2023-wimbledon-points.csv"))
names(wimbledon_2023_points)

head(wimbledon_2023_points)
colSums(is.na(wimbledon_2023_points))

# remove all cols with NAs
wimbledon_2023_matches <- wimbledon_2023_matches %>%
  select(where(~ all(!is.na(.))))
names(wimbledon_2023_matches)

wimbledon_2023_points <- wimbledon_2023_points %>%
  select(where(~ all(!is.na(.))))
names(wimbledon_2023_points)

wimbledon_2023 <- left_join(wimbledon_2023_points, wimbledon_2023_matches, by = "match_id")
names(wimbledon_2023)
length(unique(c(wimbledon_2023$player1, wimbledon_2023$player2))) # 256 unique players for wimbledon 2024

write.csv(wimbledon_2023, "wimbledon_2023_combined.csv", row.names = FALSE)

#-----------------------------------------------------------------------------------------------------

wimbledon_2024_matches <- as.data.table(read.csv("2024-wimbledon-matches.csv"))
names(wimbledon_2024_matches)

colSums(is.na(wimbledon_2024_matches))

wimbledon_2024_points <- as.data.table(read.csv("2024-wimbledon-points.csv"))
names(wimbledon_2024_points)

head(wimbledon_2024_points)
colSums(is.na(wimbledon_2024_points))

# remove all cols with NAs
wimbledon_2024_matches <- wimbledon_2024_matches %>%
  select(where(~ all(!is.na(.))))
names(wimbledon_2024_matches)

wimbledon_2024_points <- wimbledon_2024_points %>%
  select(where(~ all(!is.na(.))))
names(wimbledon_2024_points)

# i think serve number refers to first or second serve (makes sense when comparing it to speed_mph). 
# however, data dictionary says serve indicator is the one to use, which has slightly diff. entries.

wimbledon_2024 <- left_join(wimbledon_2024_points, wimbledon_2024_matches, by = "match_id")
names(wimbledon_2024)
length(unique(c(wimbledon_2024$player1, wimbledon_2024$player2))) # 256 unique players for wimbledon 2024

write.csv(wimbledon_2024, "wimbledon_2024_combined.csv", row.names = FALSE)

#-----------------------------------------------------------------------------------------------------
# EDA on serve speeds

wimbledon_2024 <- as.data.table(read.csv("wimbledon_2024_combined.csv"))
names(wimbledon_2024)

table(wimbledon_2024$ServeNumber) # around 30,000 first serves, 15,700 second serves, and 2,300 non-serves
wimbledon_2024 <- wimbledon_2024[wimbledon_2024$Speed_MPH != 0, ]

# wow these are both pretty nice normal-ish distn's
ggplot(wimbledon_2024, aes(x = Speed_MPH)) +
  geom_histogram(binwidth = 2, fill = "steelblue", color = "white") +
  facet_wrap(~ ServeNumber) +
  labs(title = "Distribution of Serve Speeds",
       x = "Speed (MPH)",
       y = "Count") +
  theme_minimal()

# are kmh and mph same?
speeds <- wimbledon_2024 %>% 
  select(Speed_KMH, Speed_MPH) %>% 
  mutate(new_speed_kmh = Speed_MPH * 1.609344)
# yes looks like they're the same

# if we use serve indicator, the histograms are pretty much the same so serve number might be correct?

subset <- wimbledon_2024 %>% 
  select(match_id, slam, year, player1, player2, Speed_MPH, ServeNumber, PointServer, PointWinner, ServeWidth, ServeDepth, RallyCount, GameNo, P1DistanceRun,
         P2DistanceRun, RallyCount)
# write.csv(subset, "wimbledon_2024_subset.csv", row.names = FALSE)
sapply(subset, typeof)
#-----------------------------------------------------------------------------------------------------

# seeing if there's any difference with 2022 data
wimbledon_2022_matches <- as.data.table(read.csv("2022-wimbledon-matches.csv"))
names(wimbledon_2022_matches)
colSums(is.na(wimbledon_2022_matches))
# same info as 2024

wimbledon_2022_points <- as.data.table(read.csv("2022-wimbledon-points.csv"))
names(wimbledon_2022_points)
colSums(is.na(wimbledon_2022_points))
# i think this is basically the same as 2024

# remove all cols with NAs
wimbledon_2022_matches <- wimbledon_2022_matches %>%
  select(where(~ all(!is.na(.))))
names(wimbledon_2022_matches)

wimbledon_2022_points <- wimbledon_2022_points %>%
  select(where(~ all(!is.na(.))))
names(wimbledon_2022_points)

wimbledon_2022 <- left_join(wimbledon_2022_points, wimbledon_2022_matches, by = "match_id")
names(wimbledon_2022)
length(unique(c(wimbledon_2022$player1, wimbledon_2022$player2))) # 256 unique players for wimbledon 2022, same as 2024

write.csv(wimbledon_2022, "wimbledon_2022_combined.csv", row.names = FALSE)

#-----------------------------------------------------------------------------------------------------

# add another column in wimbledon_2022 that indicates if the serving player won or lost
# filter out rows where PointServer is 0 and/or Speed_KMH is 0 and/or Speed_MPH is 0

wimbledon_2022 <- as.data.table(read.csv("wimbledon_2022_combined.csv"))
names(wimbledon_2022)

# binary variable: whether serving player won or lost
wimbledon_2022 <- wimbledon_2022 %>%
  filter(PointServer != 0, Speed_KMH != 0, Speed_MPH != 0) %>% 
  mutate(serving_player_won = ifelse((ServeNumber == 1 & PointWinner == 1) | (ServeNumber == 2 & PointWinner == 2), 1, 0))

ggplot(wimbledon_2022, aes(x = Speed_MPH, y = serving_player_won)) +
  geom_density2d_filled(alpha = 0.8) +
  labs(title = "Serve Speed vs. Win Outcome") +
  theme_minimal()

ggplot(wimbledon_2022, aes(x = Speed_MPH, y = serving_player_won)) +
  geom_point() +
  labs(title = "Serve Speed vs. Win Outcome") +
  theme_minimal()

# split between 1st and 2nd serves
first_serves <- wimbledon_2022 %>% 
  filter(ServeNumber == 1)

ggplot(first_serves, aes(x = Speed_MPH, y = serving_player_won)) +
  geom_point() +
  labs(title = "Serve Speed vs. Win Outcome for First Serves") +
  theme_minimal()

ggplot(first_serves, aes(x = Speed_MPH, y = serving_player_won)) +
  geom_density2d_filled(alpha = 0.8) +
  labs(title = "Serve Speed vs. Win Outcome for First Serves") +
  theme_minimal()

second_serves <- wimbledon_2022 %>% 
  filter(ServeNumber == 2)

ggplot(second_serves, aes(x = Speed_MPH, y = serving_player_won)) +
  geom_point() +
  labs(title = "Serve Speed vs. Win Outcome for Second Serves") +
  theme_minimal()

ggplot(second_serves, aes(x = Speed_MPH, y = serving_player_won)) +
  geom_density2d_filled(alpha = 0.8) +
  labs(title = "Serve Speed vs. Win Outcome for Second Serves") +
  theme_minimal()

#-----------------------------------------------------------------------------------------------------

# how about ratio of second serve speed vs. first serve speed?
# for each match, then divide further for each player, compare the ratio of (average first serve speed) to (average second serve speed) vs. how many points they won on first serve vs. how many they won on second serve
# this will be a bit tricky since we have to make sure we only compare serves from the same match and player

# actually not sure if we can do this if we want to consider position of each serve :(

# first, calculate the average speeds for first and second serves for each player in each match
wimbledon_2022_avg_speeds <- wimbledon_2022 %>% 
  group_by(match_id, player1, player2, ServeNumber) %>%
  summarise(avg_speed = mean(Speed_MPH, na.rm = TRUE), .groups = 'drop') %>%
  pivot_wider(names_from = ServeNumber, values_from = avg_speed, names_prefix = "avg_speed_") %>%
  rename(avg_speed_1st = avg_speed_1, avg_speed_2nd = avg_speed_2)

# now, calculate the ratio of first to second serve speeds for each player in each match
wimbledon_2022_avg_speeds <- wimbledon_2022_avg_speeds %>%
  mutate(speed_ratio = ifelse(avg_speed_2nd > 0, avg_speed_2nd / avg_speed_1st, NA))

# filter out any matches where we don't have both first and second serve speeds
wimbledon_2022_avg_speeds <- wimbledon_2022_avg_speeds %>%
  filter(!is.na(speed_ratio), !is.infinite(speed_ratio))

# now we can merge this back into the main dataset to analyze the serve speed ratios
wimbledon_2022_new <- left_join(wimbledon_2022, wimbledon_2022_avg_speeds, by = c("match_id", "player1", "player2"))

# check the new dataset
names(wimbledon_2022_new)

# let's also calculate the win rates for first and second serves
wimbledon_2022_new <- wimbledon_2022_new %>%
  group_by(match_id, player1, player2) %>%
  summarise(first_serve_wins = sum(serving_player_won[ServeNumber == 1], na.rm = TRUE),
            second_serve_wins = sum(serving_player_won[ServeNumber == 2], na.rm = TRUE),
            total_first_serves = sum(ServeNumber == 1),
            total_second_serves = sum(ServeNumber == 2),
            first_serve_win_rate = first_serve_wins / total_first_serves,
            second_serve_win_rate = second_serve_wins / total_second_serves,
            .groups = 'drop') %>%
  left_join(wimbledon_2022_new, by = c("match_id", "player1", "player2"))

names(wimbledon_2022_new)

#todo: divide wimbledon_2022_avg_speeds into more rows, one row per unique player.
# then, add new columns in wimbledon_2022 for player 1 avg serve speeds (1st and 2nd serves) and same with player 2.
