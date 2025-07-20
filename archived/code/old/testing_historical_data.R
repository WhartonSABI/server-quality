# rm(list=ls())
# install.packages("welo")
library(welo)
library(tidyverse)
library(data.table)

#-----------------------------------------------------------------------------------------------------

## 2024 data
wimbledon_2024 <- as.data.table(read.csv("../data/wimbledon_2024_combined.csv"))

#-----------------------------------------------------------------------------------------------------

## 2021 data
wimbledon_2021 <- as.data.table(read.csv("../data/wimbledon_2021_combined.csv"))
## 2019 data
wimbledon_2019 <- as.data.table(read.csv("../data/wimbledon_2019_combined.csv"))

#-----------------------------------------------------------------------------------------------------

### define a function to add a new column for “speed_ratio = Speed_MPH / player_avg_first_speed”

add_speed_ratio_column <- function(data) {
  # calculate average first serve speed for each player, for each match.
  avg_first_speed <- data %>%
    filter(ServeNumber == 1) %>%
    group_by(match_id) %>%
    summarise(
      avg_player1_first_speed = mean(Speed_MPH[PointServer == 1], na.rm = TRUE),
      avg_player2_first_speed = mean(Speed_MPH[PointServer == 2], na.rm = TRUE),
      .groups = 'drop'
    )
  # Join the average first speed back to the original data
  data <- data %>%
    left_join(avg_first_speed, by = "match_id") %>%
    mutate(speed_ratio = Speed_MPH / ifelse(PointServer == 1, avg_player1_first_speed, avg_player2_first_speed))
  
  return(data)
}

#-----------------------------------------------------------------------------------------------------

# 2021: divide male & female
wimbledon_2021_male <- wimbledon_2021[1:14328,]
wimbledon_2021_female <- wimbledon_2021[14329:nrow(wimbledon_2021),]

wimbledon_2021_male <- add_speed_ratio_column(wimbledon_2021_male)
wimbledon_2021_female <- add_speed_ratio_column(wimbledon_2021_female)

subset_2021_m <- wimbledon_2021_male %>% # to make it easier for us to look through relevant columns
  select(match_id, slam, year, ElapsedTime, PointNumber, player1, player2, Speed_MPH, ServeNumber, PointServer, PointWinner, ServeWidth, ServeDepth, RallyCount, GameNo, P1DistanceRun,
         P2DistanceRun, RallyCount, serving_player_won, speed_ratio)

subset_2021_f <- wimbledon_2021_female %>% # to make it easier for us to look through relevant columns
  select(match_id, slam, year, ElapsedTime, PointNumber, player1, player2, Speed_MPH, ServeNumber, PointServer, PointWinner, ServeWidth, ServeDepth, RallyCount, GameNo, P1DistanceRun,
         P2DistanceRun, RallyCount, serving_player_won, speed_ratio)

#-----------------------------------------------------------------------------------------------------

# 2019: divide male & female
wimbledon_2019_male <- wimbledon_2019[1:15678,]
wimbledon_2019_female <- wimbledon_2019[15679:nrow(wimbledon_2019),]

wimbledon_2019_male <- add_speed_ratio_column(wimbledon_2019_male)
wimbledon_2019_female <- add_speed_ratio_column(wimbledon_2019_female)

subset_2019_m <- wimbledon_2019_male %>% # to make it easier for us to look through relevant columns
  select(match_id, slam, year, ElapsedTime, PointNumber, player1, player2, Speed_MPH, ServeNumber, PointServer, PointWinner, ServeWidth, ServeDepth, RallyCount, GameNo, P1DistanceRun,
         P2DistanceRun, RallyCount, serving_player_won, speed_ratio)

subset_2019_f <- wimbledon_2019_female %>% # to make it easier for us to look through relevant columns
  select(match_id, slam, year, ElapsedTime, PointNumber, player1, player2, Speed_MPH, ServeNumber, PointServer, PointWinner, ServeWidth, ServeDepth, RallyCount, GameNo, P1DistanceRun,
         P2DistanceRun, RallyCount, serving_player_won, speed_ratio)

#-----------------------------------------------------------------------------------------------------

# check to see if all the player names in 2021 data (player1 and player2) are also in 2019 data
# first, get vector of all unique entries in 2021 player names 
unique_player_names_2021_m <- sort(unique(c(subset_2021_m$player1, subset_2021_m$player2)))
unique_player_names_2019_m <- sort(unique(c(subset_2019_m$player1, subset_2019_m$player2)))

# then, loop through all the 2021 names and save the ones that are not in the 2019 names into a new vector
missing_player_names_2021_m <- setdiff(unique_player_names_2021_m, unique_player_names_2019_m)

#-----------------------------------------------------------------------------------------------------

# 2022 data
wimbledon_2022 <- as.data.table(read.csv("../data/wimbledon_2022_combined.csv"))

# 2022: divide male & female
wimbledon_2022_male <- wimbledon_2022[1:14897,]
wimbledon_2022_female <- wimbledon_2022[14898:nrow(wimbledon_2022),]

wimbledon_2022_male <- add_speed_ratio_column(wimbledon_2022_male)
wimbledon_2022_female <- add_speed_ratio_column(wimbledon_2022_female)

subset_2022_m <- wimbledon_2022_male %>% # to make it easier for us to look through relevant columns
  select(match_id, slam, year, ElapsedTime, PointNumber, player1, player2, Speed_MPH, ServeNumber, PointServer, PointWinner, ServeWidth, ServeDepth, RallyCount, GameNo, P1DistanceRun,
         P2DistanceRun, RallyCount, serving_player_won, speed_ratio)

subset_2022_f <- wimbledon_2022_female %>% # to make it easier for us to look through relevant columns
  select(match_id, slam, year, ElapsedTime, PointNumber, player1, player2, Speed_MPH, ServeNumber, PointServer, PointWinner, ServeWidth, ServeDepth, RallyCount, GameNo, P1DistanceRun,
         P2DistanceRun, RallyCount, serving_player_won, speed_ratio)

## 2022 vs 2021
unique_player_names_2022_m <- sort(unique(c(subset_2022_m$player1, subset_2022_m$player2)))

# then, loop through all the 2021 names and save the ones that are not in the 2019 names into a new vector
missing_player_names_2022_m <- setdiff(unique_player_names_2022_m, unique_player_names_2021_m)

missing_player_names_2022_2019_m <- setdiff(unique_player_names_2022_m, unique_player_names_2019_m)

