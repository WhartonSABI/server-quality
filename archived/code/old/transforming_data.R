rm(list=ls()) # clear environment

# install.packages(c("rio", "data.table", "ggplot2","dplyr","tidyverse"))
library(rio)
library(data.table)
library(ggplot2)
library(dplyr)
library(tidyverse)

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

## wimbledon 2024:

wimbledon_2024 <- as.data.table(read.csv("../data/wimbledon_2024_combined.csv"))
names(wimbledon_2024)

# divide male & female
wimbledon_2024_male <- wimbledon_2024[1:14489,]
wimbledon_2024_female <- wimbledon_2024[14490:nrow(wimbledon_2024),]

wimbledon_2024_male <- add_speed_ratio_column(wimbledon_2024_male)
wimbledon_2024_female <- add_speed_ratio_column(wimbledon_2024_female)

subset_2024_m <- wimbledon_2024_male %>% # to make it easier for us to look through relevant columns
  select(match_id, slam, year, PointNumber, player1, player2, Speed_MPH, ServeNumber, PointServer, PointWinner, ServeWidth, ServeDepth, RallyCount, GameNo, P1DistanceRun,
         P2DistanceRun, RallyCount, serving_player_won, speed_ratio)

subset_2024_f <- wimbledon_2024_female %>% # to make it easier for us to look through relevant columns
  select(match_id, slam, year, PointNumber, player1, player2, Speed_MPH, ServeNumber, PointServer, PointWinner, ServeWidth, ServeDepth, RallyCount, GameNo, P1DistanceRun,
         P2DistanceRun, RallyCount, serving_player_won, speed_ratio)



#-----------------------------------------------------------------------------------------------------

## wimbledon 2022:
wimbledon_2022 <- as.data.table(read.csv("../data/wimbledon_2022_combined.csv"))

wimbledon_2022 <- wimbledon_2022 %>%
  filter(PointServer != 0, Speed_KMH != 0, Speed_MPH != 0) %>% 
  mutate(serving_player_won = ifelse((ServeNumber == 1 & PointWinner == 1) | (ServeNumber == 2 & PointWinner == 2), 1, 0))

wimbledon_2022_male <- wimbledon_2022[1:14897,]
wimbledon_2022_female <- wimbledon_2022[14897:nrow(wimbledon_2022),]

wimbledon_2022_male <- add_speed_ratio_column(wimbledon_2022_male)
wimbledon_2022_female <- add_speed_ratio_column(wimbledon_2022_female)

subset_2022_m <- wimbledon_2022_male %>% # to make it easier for us to look through relevant columns
  select(match_id, slam, year, PointNumber, player1, player2, Speed_MPH, ServeNumber, PointServer, PointWinner, ServeWidth, ServeDepth, RallyCount, GameNo, P1DistanceRun,
         P2DistanceRun, RallyCount, serving_player_won, speed_ratio)

subset_2022_f <- wimbledon_2022_female %>% # to make it easier for us to look through relevant columns
  select(match_id, slam, year, PointNumber, player1, player2, Speed_MPH, ServeNumber, PointServer, PointWinner, ServeWidth, ServeDepth, RallyCount, GameNo, P1DistanceRun,
         P2DistanceRun, RallyCount, serving_player_won, speed_ratio)
