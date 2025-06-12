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

# binary variable for whether the point was won by the server
wimbledon_2024 <- as.data.table(read.csv("../data/wimbledon_2024_combined.csv"))

wimbledon_2024 <- wimbledon_2024 %>%
  filter(PointServer != 0, Speed_KMH != 0, Speed_MPH != 0) %>% 
  mutate(serving_player_won = ifelse((ServeNumber == 1 & PointWinner == 1) | (ServeNumber == 2 & PointWinner == 2), 1, 0))

# divide male & female
wimbledon_2024_male <- wimbledon_2024[1:14489,]
wimbledon_2024_female <- wimbledon_2024[14490:nrow(wimbledon_2024),]

wimbledon_2024_male <- add_speed_ratio_column(wimbledon_2024_male)
wimbledon_2024_female <- add_speed_ratio_column(wimbledon_2024_female)

subset_2024_m <- wimbledon_2024_male %>% # to make it easier for us to look through relevant columns
  select(match_id, slam, year, player1, player2, Speed_MPH, ServeNumber, PointServer, PointWinner, ServeWidth, ServeDepth, RallyCount, GameNo, P1DistanceRun,
         P2DistanceRun, RallyCount, serving_player_won, speed_ratio)

subset_2024_f <- wimbledon_2024_female %>% # to make it easier for us to look through relevant columns
  select(match_id, slam, year, player1, player2, Speed_MPH, ServeNumber, PointServer, PointWinner, ServeWidth, ServeDepth, RallyCount, GameNo, P1DistanceRun,
         P2DistanceRun, RallyCount, serving_player_won, speed_ratio)

#-----------------------------------------------------------------------------------------------------

## ELO rankings
elo_rankings_2021 <- as.data.table(read.csv("../Rankings (1).csv"))
names(elo_rankings_2021)
elo_rankings_2021 <- elo_rankings_2021 %>% 
  select(rank, name, points)

# merge rankings_2024_m with subset_2024_m to get the rank of player1 and player2
subset_2024_m <- subset_2024_m %>%
  left_join(elo_rankings_2021, by = c("player1" = "name")) %>%
  rename(player1_rank = rank, player1_pts = points) %>%
  left_join(elo_rankings_2021, by = c("player2" = "name")) %>%
  rename(player2_rank = rank, player2_pts = points)

colSums(is.na(subset_2024_m))
# make new dataset that contains rows where there are NAs
subset_2024_m_na <- subset_2024_m %>%
  filter(is.na(player1_rank) | is.na(player2_rank))

#-----------------------------------------------------------------------------------------------------

## ATP player rankings 2024
rankings_2024_m <- as.data.table(read.csv("../male_2024_rankings.csv"))
names(rankings_2024_m)

# remove first column
rankings_2024_m <- rankings_2024_m[, -1, with = FALSE]
# rename columns
setnames(rankings_2024_m, old = c("X.1", "Players", "Pts"), new = c("rank", "player", "pts"))
# make player lowercase
rankings_2024_m$player <- tolower(rankings_2024_m$player)

# in subset_2024_m, make a new column for player1_name and player2_name that takes
# the first initial of player1, followed by a . then a space, then followed by the last name of player 1.
# same thing with player 2.

subset_2024_m <- subset_2024_m %>%
  mutate(player1_name = tolower(paste0(substr(player1, 1, 1), ". ", sub("^[^ ]+ ", "", player1))),
         player2_name = tolower(paste0(substr(player2, 1, 1), ". ", sub("^[^ ]+ ", "", player2))))

# merge rankings_2024_m with subset_2024_m to get the rank of player1 and player2
subset_2024_m <- subset_2024_m %>%
  left_join(rankings_2024_m, by = c("player1_name" = "player")) %>%
  rename(player1_rank = rank, player1_pts = pts) %>%
  left_join(rankings_2024_m, by = c("player2_name" = "player")) %>%
  rename(player2_rank = rank, player2_pts = pts)

# check for 0 or NAs in subset_2024_m
colSums(is.na(subset_2024_m))
# make new dataset that contains rows where there are NAs
subset_2024_m_na <- subset_2024_m %>%
  filter(is.na(player1_rank) | is.na(player2_rank))

# in subset_2024_m, if player1 or player2 is in some vector of names we need to change,
# then change player1_name and/or player2_name to the corresponding element in a second vector
# vector of names to change
names_to_change <- c("francisco comesana", "roberto carballes baena",
                      "felix auger aliassime", "jan lennard struff",
                      "roberto bautista agut")
# corresponding vector of names to change to
names_to_change_to <- c("f. comesaña", "r. carballés",
                         "f. auger-aliassime", "j. struff", "r. bautista")
# change player1_name and player2_name in subset_2024_m
subset_2024_m_test <- subset_2024_m %>%
  mutate(player1_name = ifelse(player1_name %in% names_to_change, 
                               names_to_change_to[match(player1_name, names_to_change)], player1_name),
         player2_name = ifelse(player2_name %in% names_to_change, 
                               names_to_change_to[match(player2_name, names_to_change)], player2_name))
colSums(is.na(subset_2024_m_test))
subset_2024_m_na <- subset_2024_m_test %>%
  filter(is.na(player1_rank) | is.na(player2_rank))

## pedro martinez, liam broady, elias ymer, Facundo Diaz Acosta aren't ranked?
# Botic van De Zandschulp change to B. van de Zandschulp instead of B. van De Zandschulp
# Francisco Comesana change to F. Comesaña instead of F. Comesana
# Roberto Carballes Baena change to R. Carballés instead of R. Carballes Baena
# Felix Auger Aliassime change to F. Auger-Aliassime instead of F. Auger Aliassime
# Alex de Minaur change to A. De Minaur instead of A. de Minaur
# Jan Lennard Struff change to J. Struff instead of J. Lennard Struff
# Roberto Bautista Agut change to R. Bautista instead of R. Bautista Agut

## tldr: change to all lowercase, then go in and manually change everything else


# write.csv(subset_2024_m, "../data/wimbledon_2024_m_rankings.csv", row.names = FALSE)
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
