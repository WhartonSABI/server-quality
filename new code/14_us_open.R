rm(list=ls()) # clear environment

# install.packages(c("rio", "data.table", "ggplot2","dplyr","tidyverse"))
library(rio)
library(data.table)
library(ggplot2)
library(dplyr)
library(tidyverse)

#-----------------------------------------------------------------------------------------------------

## us open 2021

usopen_2021_matches <- as.data.table(read.csv("../data/raw_data/2021-usopen-matches.csv"))
colSums(is.na(usopen_2021_matches))

usopen_2021_points <- as.data.table(read.csv("../data/raw_data/2021-usopen-points.csv"))
colSums(is.na(usopen_2021_points))

# remove all cols with NAs
usopen_2021_matches <- usopen_2021_matches %>%
  select(where(~ all(!is.na(.))))
names(usopen_2021_matches)

usopen_2021_points <- usopen_2021_points %>%
  select(where(~ all(!is.na(.))))
names(usopen_2021_points)

usopen_2021 <- left_join(usopen_2021_points, usopen_2021_matches, by = "match_id")
names(usopen_2021)
colSums(is.na(usopen_2021))

usopen_2021 <- usopen_2021 %>%
  filter(!(P1DoubleFault == 0 & P2DoubleFault == 0 & Speed_MPH == 0)) %>% 
  mutate(serving_player_won = ifelse((PointServer == 1 & PointWinner == 1) | (PointServer == 2 & PointWinner == 2), 1, 0))

# binary variable: whether serving player won or lost
# usopen_2021 <- usopen_2021 %>%
#   filter(PointServer != 0, Speed_KMH != 0, Speed_MPH != 0) %>%
#   mutate(serving_player_won = ifelse((PointServer == 1 & PointWinner == 1) | (PointServer == 2 & PointWinner == 2), 1, 0))

# usopen_2021 <- usopen_2021 %>%
#   filter(PointServer != 0) %>%
#   mutate(serving_player_won = ifelse((PointServer == 1 & PointWinner == 1) | (PointServer == 2 & PointWinner == 2), 1, 0))

write.csv(usopen_2021, "out_data/usopen_2021_combined.csv", row.names = FALSE)

#-----------------------------------------------------------------------------------------------------

## us open 2022

usopen_2022_matches <- as.data.table(read.csv("../data/raw_data/2022-usopen-matches.csv"))
colSums(is.na(usopen_2022_matches))

usopen_2022_points <- as.data.table(read.csv("../data/raw_data/2022-usopen-points.csv"))
colSums(is.na(usopen_2022_points))

# remove all cols with NAs
usopen_2022_matches <- usopen_2022_matches %>%
  select(where(~ all(!is.na(.))))
names(usopen_2022_matches)

usopen_2022_points <- usopen_2022_points %>%
  select(where(~ all(!is.na(.))))
names(usopen_2022_points)

usopen_2022 <- left_join(usopen_2022_points, usopen_2022_matches, by = "match_id")
names(usopen_2022)
colSums(is.na(usopen_2022))

# binary variable: whether serving player won or lost
# usopen_2022 <- usopen_2022 %>%
#   filter(PointServer != 0, Speed_KMH != 0, Speed_MPH != 0) %>%
#   mutate(serving_player_won = ifelse((PointServer == 1 & PointWinner == 1) | (PointServer == 2 & PointWinner == 2), 1, 0))

# usopen_2022 <- usopen_2022 %>%
#   filter(PointServer != 0) %>%
#   mutate(serving_player_won = ifelse((PointServer == 1 & PointWinner == 1) | (PointServer == 2 & PointWinner == 2), 1, 0))

usopen_2022 <- usopen_2022 %>%
  filter(!(P1DoubleFault == 0 & P2DoubleFault == 0 & Speed_MPH == 0)) %>% 
  mutate(serving_player_won = ifelse((PointServer == 1 & PointWinner == 1) | (PointServer == 2 & PointWinner == 2), 1, 0))


write.csv(usopen_2022, "out_data/usopen_2022_combined.csv", row.names = FALSE)

#-----------------------------------------------------------------------------------------------------

## us open 2023

usopen_2023_matches <- as.data.table(read.csv("../data/raw_data/2023-usopen-matches.csv"))
colSums(is.na(usopen_2023_matches))

usopen_2023_points <- as.data.table(read.csv("../data/raw_data/2023-usopen-points.csv"))
colSums(is.na(usopen_2023_points))

# remove all cols with NAs
usopen_2023_matches <- usopen_2023_matches %>%
  select(where(~ all(!is.na(.))))
names(usopen_2023_matches)

usopen_2023_points <- usopen_2023_points %>%
  select(where(~ all(!is.na(.))))
names(usopen_2023_points)

usopen_2023 <- left_join(usopen_2023_points, usopen_2023_matches, by = "match_id")
names(usopen_2023)
colSums(is.na(usopen_2023))

# binary variable: whether serving player won or lost
# usopen_2023 <- usopen_2023 %>%
#   filter(PointServer != 0, Speed_KMH != 0, Speed_MPH != 0) %>%
#   mutate(serving_player_won = ifelse((PointServer == 1 & PointWinner == 1) | (PointServer == 2 & PointWinner == 2), 1, 0))

# usopen_2023 <- usopen_2023 %>%
#   filter(PointServer != 0) %>%
#   mutate(serving_player_won = ifelse((PointServer == 1 & PointWinner == 1) | (PointServer == 2 & PointWinner == 2), 1, 0))

usopen_2023 <- usopen_2023 %>%
  filter(!(P1DoubleFault == 0 & P2DoubleFault == 0 & Speed_MPH == 0)) %>% 
  mutate(serving_player_won = ifelse((PointServer == 1 & PointWinner == 1) | (PointServer == 2 & PointWinner == 2), 1, 0))


write.csv(usopen_2023, "out_data/usopen_2023_combined.csv", row.names = FALSE)

#-----------------------------------------------------------------------------------------------------

## us open 2024

usopen_2024_matches <- as.data.table(read.csv("../data/raw_data/2024-usopen-matches.csv"))
colSums(is.na(usopen_2024_matches))

usopen_2024_points <- as.data.table(read.csv("../data/raw_data/2024-usopen-points.csv"))
colSums(is.na(usopen_2024_points))

# remove all cols with NAs
usopen_2024_matches <- usopen_2024_matches %>%
  select(where(~ all(!is.na(.))))
names(usopen_2024_matches)

usopen_2024_points <- usopen_2024_points %>%
  select(where(~ all(!is.na(.))))
names(usopen_2024_points)

usopen_2024 <- left_join(usopen_2024_points, usopen_2024_matches, by = "match_id")
names(usopen_2024)
colSums(is.na(usopen_2024))

# binary variable: whether serving player won or lost
# usopen_2024 <- usopen_2024 %>%
#   filter(PointServer != 0, Speed_KMH != 0, Speed_MPH != 0) %>%
#   mutate(serving_player_won = ifelse((PointServer == 1 & PointWinner == 1) | (PointServer == 2 & PointWinner == 2), 1, 0))

# usopen_2024 <- usopen_2024 %>%
#   filter(PointServer != 0) %>%
#   mutate(serving_player_won = ifelse((PointServer == 1 & PointWinner == 1) | (PointServer == 2 & PointWinner == 2), 1, 0))

usopen_2024 <- usopen_2024 %>%
  filter(!(P1DoubleFault == 0 & P2DoubleFault == 0 & Speed_MPH == 0)) %>% 
  mutate(serving_player_won = ifelse((PointServer == 1 & PointWinner == 1) | (PointServer == 2 & PointWinner == 2), 1, 0))


write.csv(usopen_2024, "out_data/usopen_2024_combined.csv", row.names = FALSE)

#-----------------------------------------------------------------------------------------------------

### define a function to add a new column for “speed_ratio = Speed_MPH / player_avg_first_speed”

add_speed_ratio_column <- function(data) {
  # calculate average first serve speed for each player, for each match.
  avg_first_speed <- data %>%
    filter(ServeNumber == 1) %>%
    group_by(match_id) %>%
    summarise(
      avg_player1_first_speed = mean(Speed_MPH[PointServer == 1 & Speed_MPH > 0], na.rm = TRUE),
      avg_player2_first_speed = mean(Speed_MPH[PointServer == 2 & Speed_MPH > 0], na.rm = TRUE),
      .groups = 'drop'
    )
  # Join the average first speed back to the original data
  data <- data %>%
    left_join(avg_first_speed, by = "match_id") %>%
    mutate(speed_ratio = Speed_MPH / ifelse(PointServer == 1, avg_player1_first_speed, avg_player2_first_speed))
  
  return(data)
}

#-----------------------------------------------------------------------------------------------------

## transform 2024 data

usopen_2024 <- usopen_2024 %>% 
  mutate(P1Score = as.character(P1Score),
         P2Score = as.character(P2Score),
         PointServer = as.integer(PointServer),
         PointWinner = as.integer(PointWinner),
         GameWinner = as.integer(GameWinner),
         server_score = if_else(PointServer == 1, P1Score, P2Score),
         returner_score = if_else(PointServer == 1, P2Score, P1Score),
         state = paste(server_score, returner_score, sep = "-")) %>% 
  filter(state %in% c("0-0", "15-0", "30-0", "40-0",
                      "0-15", "0-30", "0-40",
                      "15-15", "30-15", "40-15",
                      "15-30", "30-30", "40-30",
                      "15-40", "30-40", "40-40",
                      "40-AD", "AD-40"))

unique_states <- unique(c(usopen_2024$state)) 
length(unique_states) 

score_importance_dtmc <- as.data.table(read.csv("../data/score_importance_dtmc.csv"))

usopen_2024 <- left_join(usopen_2024, score_importance_dtmc, by = "state")
colSums(is.na(usopen_2024))

# divide male & female
usopen_2024_male <- usopen_2024[1:26610,]
usopen_2024_female <- usopen_2024[26611:nrow(usopen_2024),]

usopen_2024_male <- add_speed_ratio_column(usopen_2024_male)
usopen_2024_female <- add_speed_ratio_column(usopen_2024_female)

subset_2024_m <- usopen_2024_male %>% # to make it easier for us to look through relevant columns
  select(match_id, slam, year, ElapsedTime, SetNo, GameNo, PointNumber, player1, player2, Speed_MPH, ServeNumber, PointServer, PointWinner, 
         ServeWidth, ServeDepth, RallyCount, P1DistanceRun, P2DistanceRun, P1Score, P2Score, state, PointWinner, GameWinner,
         RallyCount, serving_player_won, speed_ratio, P1BreakPoint, P2BreakPoint, P1GamesWon, P2GamesWon, importance,
         P1Ace, P2Ace, P1DoubleFault, P2DoubleFault)

subset_2024_f <- usopen_2024_female %>% # to make it easier for us to look through relevant columns
  select(match_id, slam, year, ElapsedTime, SetNo, GameNo, PointNumber, player1, player2, Speed_MPH, ServeNumber, PointServer, PointWinner, 
         ServeWidth, ServeDepth, RallyCount, P1DistanceRun, P2DistanceRun, P1Score, P2Score, state, PointWinner, GameWinner,
         RallyCount, serving_player_won, speed_ratio, P1BreakPoint, P2BreakPoint, P1GamesWon, P2GamesWon, importance,
         P1Ace, P2Ace, P1DoubleFault, P2DoubleFault)

# add new column for player1_name being last name (which includes every word in player1 except for 
# the first word) followed by their first initial, then a period.

subset_2024_m <- subset_2024_m %>%
  rowwise() %>%
  mutate(
    player1_name = tolower(paste(
      paste(tail(strsplit(player1, " ")[[1]], -1), collapse = " "),
      paste0(substr(player1, 1, 1), "."))
    ),
    player2_name = tolower(paste(
      paste(tail(strsplit(player2, " ")[[1]], -1), collapse = " "),
      paste0(substr(player2, 1, 1), "."))
    )
  ) %>%
  ungroup()

subset_2024_f <- subset_2024_f %>%
  rowwise() %>%
  mutate(
    player1_name = tolower(paste(
      paste(tail(strsplit(player1, " ")[[1]], -1), collapse = " "),
      paste0(substr(player1, 1, 1), "."))
    ),
    player2_name = tolower(paste(
      paste(tail(strsplit(player2, " ")[[1]], -1), collapse = " "),
      paste0(substr(player2, 1, 1), "."))
    )
  ) %>%
  ungroup()

## male welo 2024
library(welo)
## males welo
atp_2024 <- tennis_data("2024", "ATP") # ATP = men's, WTA = women's
atp_2024_clean <- clean(atp_2024)

atp_2024_welo <- atp_2024_clean %>%
  filter(Series == "Grand Slam", Tournament == "US Open", Surface == "Hard") 

atp_2024_welo <- welofit(atp_2024_welo)$results

# select only the P_i, P_j, WElo_i_before_match, and WElo_j_before_match columns. Then, 
# make into only two columns by putting the P_j and corresponding WElo_j_before_match in rows below the 
# P_i nd WElo_i_before_match columns.
atp_2024_welo_subset <- atp_2024_welo %>%
  select(P_i, P_j, WElo_i_before_match, WElo_j_before_match) %>%
  pivot_longer(cols = c(P_i, P_j, WElo_i_before_match, WElo_j_before_match), 
               names_to = c(".value", "player"), 
               names_pattern = "(.*)_(i|j)") %>%
  mutate(player = ifelse(player == "i", "player1", "player2")) %>%
  select(-player)

# find average welo for each player in atp_2021_welo_subset
atp_2024_welo_avg <- atp_2024_welo_subset %>%
  group_by(P) %>%
  summarise(
    avg_welo_before_match = mean(WElo, na.rm = TRUE),
    .groups = 'drop'
  )

# find number of unique entries in player1 and player2 for subset_2024_m
unique_players_2024_m <- unique(c(subset_2024_m$player1_name, subset_2024_m$player2_name)) 
length(unique_players_2024_m)  # 128

# make P column in atp_2024_welo_avg all lowercase
atp_2024_welo_avg <- atp_2024_welo_avg %>%
  mutate(P = tolower(P))

# merge atp_2024_welo_avg with subset_2024_m to get the average welo for player1 and player2
subset_2024_m <- subset_2024_m %>%
  left_join(atp_2024_welo_avg, by = c("player1_name" = "P")) %>%
  rename(player1_avg_welo = avg_welo_before_match) %>%
  left_join(atp_2024_welo_avg, by = c("player2_name" = "P")) %>%
  rename(player2_avg_welo = avg_welo_before_match)

colSums(is.na(subset_2024_m))
# make new dataset that contains rows where there are NAs
subset_2024_m_na <- subset_2024_m %>%
  filter(is.na(player1_avg_welo) | is.na(player2_avg_welo))

# remove rows in subset_2024_m with NAs
subset_2024_m <- subset_2024_m %>%
  filter(!is.na(player1_avg_welo) & !is.na(player2_avg_welo))

# write.csv(subset_2024_m, "../data/wimbledon_subset_2024_m.csv", row.names = FALSE)

### females welo
wta_2024 <- tennis_data("2024", "WTA")
wta_2024_clean <- clean(wta_2024)

wta_2024_welo <- wta_2024_clean %>%
  filter(Tournament == "US Open", Surface == "Hard") 

wta_2024_welo <- welofit(wta_2024_welo)$results

# select only the P_i, P_j, WElo_i_before_match, and WElo_j_before_match columns. Then, 
# make into only two columns by putting the P_j and corresponding WElo_j_before_match in rows below the 
# P_i nd WElo_i_before_match columns.
wta_2024_welo_subset <- wta_2024_welo %>%
  select(P_i, P_j, WElo_i_before_match, WElo_j_before_match) %>%
  pivot_longer(cols = c(P_i, P_j, WElo_i_before_match, WElo_j_before_match), 
               names_to = c(".value", "player"), 
               names_pattern = "(.*)_(i|j)") %>%
  mutate(player = ifelse(player == "i", "player1", "player2")) %>%
  select(-player)

# find average welo for each player in atp_2021_welo_subset
wta_2024_welo_avg <- wta_2024_welo_subset %>%
  group_by(P) %>%
  summarise(
    avg_welo_before_match = mean(WElo, na.rm = TRUE),
    .groups = 'drop'
  )

wta_2024_welo_avg <- wta_2024_welo_avg %>%
  mutate(P = tolower(P))

# find number of unique entries in player1 and player2 for subset_2024_m
unique_players_2024_f <- unique(c(subset_2024_f$player1_name, subset_2024_f$player2_name)) 
length(unique_players_2024_f) # 127

# make P column in atp_2021_welo_avg all lowercase
wta_2024_welo_avg <- wta_2024_welo_avg %>%
  mutate(P = tolower(P))

# merge atp_2021_welo_avg with subset_2024_m to get the average welo for player1 and player2
subset_2024_f <- subset_2024_f %>%
  left_join(wta_2024_welo_avg, by = c("player1_name" = "P")) %>%
  rename(player1_avg_welo = avg_welo_before_match) %>%
  left_join(wta_2024_welo_avg, by = c("player2_name" = "P")) %>%
  rename(player2_avg_welo = avg_welo_before_match)

colSums(is.na(subset_2024_f))
# make new dataset that contains rows where there are NAs
subset_2024_f_na <- subset_2024_f %>%
  filter(is.na(player1_avg_welo) | is.na(player2_avg_welo))

# remove rows in subset_2024_m with NAs
subset_2024_f <- subset_2024_f %>%
  filter(!is.na(player1_avg_welo) & !is.na(player2_avg_welo))

# write.csv(subset_2024_f, "../data/wimbledon_subset_2024_f.csv", row.names = FALSE)

# make new column in subset_2024_m and subset_2024_f which transforms ElapsedTime into number of seconds elapsed

subset_2024_m <- subset_2024_m %>%
  separate(ElapsedTime, into = c("h", "m", "s"), sep = ":", convert = TRUE) %>%
  mutate(ElapsedSeconds = h * 3600 + m * 60 + s) %>%
  select(-h, -m, -s)

subset_2024_f <- subset_2024_f %>%
  separate(ElapsedTime, into = c("h", "m", "s"), sep = ":", convert = TRUE) %>%
  mutate(ElapsedSeconds = h * 3600 + m * 60 + s) %>%
  select(-h, -m, -s)

# subset_2024_m <- subset_2024_m %>%
#   mutate(ElapsedSeconds = as.numeric(
#     as.difftime(ElapsedTime, format = "%H:%M:%S", units = "secs")
#   ))
# 
# subset_2024_f <- subset_2024_f %>%
#   mutate(ElapsedSeconds = as.numeric(
#     as.difftime(ElapsedTime, format = "%H:%M:%S", units = "secs")
#   ))

write.csv(subset_2024_m, "out_data/usopen_subset_2024_m.csv", row.names = FALSE)
write.csv(subset_2024_f, "out_data/usopen_subset_2024_f.csv", row.names = FALSE)

#-----------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------

## transform 2023 data

usopen_2023 <- usopen_2023 %>% 
  mutate(P1Score = as.character(P1Score),
         P2Score = as.character(P2Score),
         PointServer = as.integer(PointServer),
         PointWinner = as.integer(PointWinner),
         GameWinner = as.integer(GameWinner),
         server_score = if_else(PointServer == 1, P1Score, P2Score),
         returner_score = if_else(PointServer == 1, P2Score, P1Score),
         state = paste(server_score, returner_score, sep = "-")) %>% 
  filter(state %in% c("0-0", "15-0", "30-0", "40-0",
                      "0-15", "0-30", "0-40",
                      "15-15", "30-15", "40-15",
                      "15-30", "30-30", "40-30",
                      "15-40", "30-40", "40-40",
                      "40-AD", "AD-40"))

unique_states <- unique(c(usopen_2023$state)) 
length(unique_states) 

score_importance_dtmc <- as.data.table(read.csv("../data/score_importance_dtmc.csv"))

usopen_2023 <- left_join(usopen_2023, score_importance_dtmc, by = "state")
colSums(is.na(usopen_2023))

# divide male & female
usopen_2023_male <- usopen_2023[1:25923,]
usopen_2023_female <- usopen_2023[25924:nrow(usopen_2023),]

usopen_2023_male <- add_speed_ratio_column(usopen_2023_male)
usopen_2023_female <- add_speed_ratio_column(usopen_2023_female)

subset_2023_m <- usopen_2023_male %>% # to make it easier for us to look through relevant columns
  select(match_id, slam, year, ElapsedTime, SetNo, GameNo, PointNumber, player1, player2, Speed_MPH, ServeNumber, PointServer, PointWinner, 
         ServeWidth, ServeDepth, RallyCount, P1DistanceRun, P2DistanceRun, P1Score, P2Score, state, PointWinner, GameWinner,
         RallyCount, serving_player_won, speed_ratio, P1BreakPoint, P2BreakPoint, P1GamesWon, P2GamesWon, importance,
         P1Ace, P2Ace, P1DoubleFault, P2DoubleFault)

subset_2023_f <- usopen_2023_female %>% # to make it easier for us to look through relevant columns
  select(match_id, slam, year, ElapsedTime, SetNo, GameNo, PointNumber, player1, player2, Speed_MPH, ServeNumber, PointServer, PointWinner, 
         ServeWidth, ServeDepth, RallyCount, P1DistanceRun, P2DistanceRun, P1Score, P2Score, state, PointWinner, GameWinner,
         RallyCount, serving_player_won, speed_ratio, P1BreakPoint, P2BreakPoint, P1GamesWon, P2GamesWon, importance,
         P1Ace, P2Ace, P1DoubleFault, P2DoubleFault)

# add new column for player1_name being last name (which includes every word in player1 except for 
# the first word) followed by their first initial, then a period.

subset_2023_m <- subset_2023_m %>%
  rowwise() %>%
  mutate(
    player1_name = tolower(paste(
      paste(tail(strsplit(player1, " ")[[1]], -1), collapse = " "),
      paste0(substr(player1, 1, 1), "."))
    ),
    player2_name = tolower(paste(
      paste(tail(strsplit(player2, " ")[[1]], -1), collapse = " "),
      paste0(substr(player2, 1, 1), "."))
    )
  ) %>%
  ungroup()

subset_2023_f <- subset_2023_f %>%
  rowwise() %>%
  mutate(
    player1_name = tolower(paste(
      paste(tail(strsplit(player1, " ")[[1]], -1), collapse = " "),
      paste0(substr(player1, 1, 1), "."))
    ),
    player2_name = tolower(paste(
      paste(tail(strsplit(player2, " ")[[1]], -1), collapse = " "),
      paste0(substr(player2, 1, 1), "."))
    )
  ) %>%
  ungroup()

## male welo 2023
library(welo)
## males welo
atp_2023 <- tennis_data("2023", "ATP") # ATP = men's, WTA = women's
atp_2023_clean <- clean(atp_2023)

atp_2023_welo <- atp_2023_clean %>%
  filter(Series == "Grand Slam", Tournament == "US Open", Surface == "Hard") 

atp_2023_welo <- welofit(atp_2023_welo)$results

# select only the P_i, P_j, WElo_i_before_match, and WElo_j_before_match columns. Then, 
# make into only two columns by putting the P_j and corresponding WElo_j_before_match in rows below the 
# P_i nd WElo_i_before_match columns.
atp_2023_welo_subset <- atp_2023_welo %>%
  select(P_i, P_j, WElo_i_before_match, WElo_j_before_match) %>%
  pivot_longer(cols = c(P_i, P_j, WElo_i_before_match, WElo_j_before_match), 
               names_to = c(".value", "player"), 
               names_pattern = "(.*)_(i|j)") %>%
  mutate(player = ifelse(player == "i", "player1", "player2")) %>%
  select(-player)

# find average welo for each player in atp_2021_welo_subset
atp_2023_welo_avg <- atp_2023_welo_subset %>%
  group_by(P) %>%
  summarise(
    avg_welo_before_match = mean(WElo, na.rm = TRUE),
    .groups = 'drop'
  )

# find number of unique entries in player1 and player2 for subset_2024_m
unique_players_2023_m <- unique(c(subset_2023_m$player1_name, subset_2023_m$player2_name)) 
length(unique_players_2023_m)  # 127

# make P column in atp_2024_welo_avg all lowercase
atp_2023_welo_avg <- atp_2023_welo_avg %>%
  mutate(P = tolower(P))

# merge atp_2024_welo_avg with subset_2024_m to get the average welo for player1 and player2
subset_2023_m <- subset_2023_m %>%
  left_join(atp_2024_welo_avg, by = c("player1_name" = "P")) %>%
  rename(player1_avg_welo = avg_welo_before_match) %>%
  left_join(atp_2024_welo_avg, by = c("player2_name" = "P")) %>%
  rename(player2_avg_welo = avg_welo_before_match)

colSums(is.na(subset_2023_m))
# make new dataset that contains rows where there are NAs
subset_2023_m_na <- subset_2023_m %>%
  filter(is.na(player1_avg_welo) | is.na(player2_avg_welo))

# remove rows in subset_2024_m with NAs
subset_2023_m <- subset_2023_m %>%
  filter(!is.na(player1_avg_welo) & !is.na(player2_avg_welo))

# write.csv(subset_2024_m, "../data/wimbledon_subset_2024_m.csv", row.names = FALSE)

### females welo
wta_2023 <- tennis_data("2023", "WTA")
wta_2023_clean <- clean(wta_2023)

wta_2023_welo <- wta_2023_clean %>%
  filter(Tournament == "US Open", Surface == "Hard") 

wta_2023_welo <- welofit(wta_2023_welo)$results

# select only the P_i, P_j, WElo_i_before_match, and WElo_j_before_match columns. Then, 
# make into only two columns by putting the P_j and corresponding WElo_j_before_match in rows below the 
# P_i nd WElo_i_before_match columns.
wta_2023_welo_subset <- wta_2023_welo %>%
  select(P_i, P_j, WElo_i_before_match, WElo_j_before_match) %>%
  pivot_longer(cols = c(P_i, P_j, WElo_i_before_match, WElo_j_before_match), 
               names_to = c(".value", "player"), 
               names_pattern = "(.*)_(i|j)") %>%
  mutate(player = ifelse(player == "i", "player1", "player2")) %>%
  select(-player)

# find average welo for each player in atp_2021_welo_subset
wta_2023_welo_avg <- wta_2023_welo_subset %>%
  group_by(P) %>%
  summarise(
    avg_welo_before_match = mean(WElo, na.rm = TRUE),
    .groups = 'drop'
  )

wta_2023_welo_avg <- wta_2023_welo_avg %>%
  mutate(P = tolower(P))

# find number of unique entries in player1 and player2 for subset_2024_m
unique_players_2023_f <- unique(c(subset_2023_f$player1_name, subset_2023_f$player2_name)) 
length(unique_players_2023_f) # 126

# make P column in atp_2021_welo_avg all lowercase
wta_2023_welo_avg <- wta_2023_welo_avg %>%
  mutate(P = tolower(P))

# merge atp_2021_welo_avg with subset_2024_m to get the average welo for player1 and player2
subset_2023_f <- subset_2023_f %>%
  left_join(wta_2024_welo_avg, by = c("player1_name" = "P")) %>%
  rename(player1_avg_welo = avg_welo_before_match) %>%
  left_join(wta_2024_welo_avg, by = c("player2_name" = "P")) %>%
  rename(player2_avg_welo = avg_welo_before_match)

colSums(is.na(subset_2023_f))
# make new dataset that contains rows where there are NAs
subset_2023_f_na <- subset_2023_f %>%
  filter(is.na(player1_avg_welo) | is.na(player2_avg_welo))

# remove rows in subset_2024_m with NAs
subset_2023_f <- subset_2023_f %>%
  filter(!is.na(player1_avg_welo) & !is.na(player2_avg_welo))

# write.csv(subset_2024_f, "../data/wimbledon_subset_2024_f.csv", row.names = FALSE)

# make new column in subset_2024_m and subset_2024_f which transforms ElapsedTime into number of seconds elapsed

subset_2023_m <- subset_2023_m %>%
  separate(ElapsedTime, into = c("h", "m", "s"), sep = ":", convert = TRUE) %>%
  mutate(ElapsedSeconds = h * 3600 + m * 60 + s) %>%
  select(-h, -m, -s)

subset_2023_f <- subset_2023_f %>%
  separate(ElapsedTime, into = c("h", "m", "s"), sep = ":", convert = TRUE) %>%
  mutate(ElapsedSeconds = h * 3600 + m * 60 + s) %>%
  select(-h, -m, -s)

# subset_2024_m <- subset_2024_m %>%
#   mutate(ElapsedSeconds = as.numeric(
#     as.difftime(ElapsedTime, format = "%H:%M:%S", units = "secs")
#   ))
# 
# subset_2024_f <- subset_2024_f %>%
#   mutate(ElapsedSeconds = as.numeric(
#     as.difftime(ElapsedTime, format = "%H:%M:%S", units = "secs")
#   ))

write.csv(subset_2023_m, "out_data/usopen_subset_2023_m.csv", row.names = FALSE)
write.csv(subset_2023_f, "out_data/usopen_subset_2023_f.csv", row.names = FALSE)

#-----------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------

## transform 2022 data

usopen_2022 <- usopen_2022 %>% 
  mutate(P1Score = as.character(P1Score),
         P2Score = as.character(P2Score),
         PointServer = as.integer(PointServer),
         PointWinner = as.integer(PointWinner),
         GameWinner = as.integer(GameWinner),
         server_score = if_else(PointServer == 1, P1Score, P2Score),
         returner_score = if_else(PointServer == 1, P2Score, P1Score),
         state = paste(server_score, returner_score, sep = "-")) %>% 
  filter(state %in% c("0-0", "15-0", "30-0", "40-0",
                      "0-15", "0-30", "0-40",
                      "15-15", "30-15", "40-15",
                      "15-30", "30-30", "40-30",
                      "15-40", "30-40", "40-40",
                      "40-AD", "AD-40"))

unique_states <- unique(c(usopen_2022$state)) 
length(unique_states) 

score_importance_dtmc <- as.data.table(read.csv("../data/score_importance_dtmc.csv"))

usopen_2022 <- left_join(usopen_2022, score_importance_dtmc, by = "state")
colSums(is.na(usopen_2022))

# divide male & female
usopen_2022_male <- usopen_2022[1:26863,]
usopen_2022_female <- usopen_2022[26864:nrow(usopen_2022),]

usopen_2022_male <- add_speed_ratio_column(usopen_2022_male)
usopen_2022_female <- add_speed_ratio_column(usopen_2022_female)

subset_2022_m <- usopen_2022_male %>% # to make it easier for us to look through relevant columns
  select(match_id, slam, year, ElapsedTime, SetNo, GameNo, PointNumber, player1, player2, Speed_MPH, ServeNumber, PointServer, PointWinner, 
         ServeWidth, ServeDepth, RallyCount, P1DistanceRun, P2DistanceRun, P1Score, P2Score, state, PointWinner, GameWinner,
         RallyCount, serving_player_won, speed_ratio, P1BreakPoint, P2BreakPoint, P1GamesWon, P2GamesWon, importance,
         P1Ace, P2Ace, P1DoubleFault, P2DoubleFault)

subset_2022_f <- usopen_2022_female %>% # to make it easier for us to look through relevant columns
  select(match_id, slam, year, ElapsedTime, SetNo, GameNo, PointNumber, player1, player2, Speed_MPH, ServeNumber, PointServer, PointWinner, 
         ServeWidth, ServeDepth, RallyCount, P1DistanceRun, P2DistanceRun, P1Score, P2Score, state, PointWinner, GameWinner,
         RallyCount, serving_player_won, speed_ratio, P1BreakPoint, P2BreakPoint, P1GamesWon, P2GamesWon, importance,
         P1Ace, P2Ace, P1DoubleFault, P2DoubleFault)

# add new column for player1_name being last name (which includes every word in player1 except for 
# the first word) followed by their first initial, then a period.

subset_2022_m <- subset_2022_m %>%
  rowwise() %>%
  mutate(
    player1_name = tolower(paste(
      paste(tail(strsplit(player1, " ")[[1]], -1), collapse = " "),
      paste0(substr(player1, 1, 1), "."))
    ),
    player2_name = tolower(paste(
      paste(tail(strsplit(player2, " ")[[1]], -1), collapse = " "),
      paste0(substr(player2, 1, 1), "."))
    )
  ) %>%
  ungroup()

subset_2022_f <- subset_2022_f %>%
  rowwise() %>%
  mutate(
    player1_name = tolower(paste(
      paste(tail(strsplit(player1, " ")[[1]], -1), collapse = " "),
      paste0(substr(player1, 1, 1), "."))
    ),
    player2_name = tolower(paste(
      paste(tail(strsplit(player2, " ")[[1]], -1), collapse = " "),
      paste0(substr(player2, 1, 1), "."))
    )
  ) %>%
  ungroup()

## male welo 2022
library(welo)
## males welo
atp_2022 <- tennis_data("2022", "ATP") # ATP = men's, WTA = women's
atp_2022_clean <- clean(atp_2022)

atp_2022_welo <- atp_2022_clean %>%
  filter(Series == "Grand Slam", Tournament == "US Open", Surface == "Hard") 

atp_2022_welo <- welofit(atp_2022_welo)$results

# select only the P_i, P_j, WElo_i_before_match, and WElo_j_before_match columns. Then, 
# make into only two columns by putting the P_j and corresponding WElo_j_before_match in rows below the 
# P_i nd WElo_i_before_match columns.
atp_2022_welo_subset <- atp_2022_welo %>%
  select(P_i, P_j, WElo_i_before_match, WElo_j_before_match) %>%
  pivot_longer(cols = c(P_i, P_j, WElo_i_before_match, WElo_j_before_match), 
               names_to = c(".value", "player"), 
               names_pattern = "(.*)_(i|j)") %>%
  mutate(player = ifelse(player == "i", "player1", "player2")) %>%
  select(-player)

# find average welo for each player in atp_2021_welo_subset
atp_2022_welo_avg <- atp_2022_welo_subset %>%
  group_by(P) %>%
  summarise(
    avg_welo_before_match = mean(WElo, na.rm = TRUE),
    .groups = 'drop'
  )

# find number of unique entries in player1 and player2 for subset_2024_m
unique_players_2022_m <- unique(c(subset_2022_m$player1_name, subset_2022_m$player2_name)) 
length(unique_players_2022_m)  # 128

# make P column in atp_2024_welo_avg all lowercase
atp_2022_welo_avg <- atp_2022_welo_avg %>%
  mutate(P = tolower(P))

# merge atp_2024_welo_avg with subset_2024_m to get the average welo for player1 and player2
subset_2022_m <- subset_2022_m %>%
  left_join(atp_2024_welo_avg, by = c("player1_name" = "P")) %>%
  rename(player1_avg_welo = avg_welo_before_match) %>%
  left_join(atp_2024_welo_avg, by = c("player2_name" = "P")) %>%
  rename(player2_avg_welo = avg_welo_before_match)

colSums(is.na(subset_2022_m))
# make new dataset that contains rows where there are NAs
subset_2022_m_na <- subset_2022_m %>%
  filter(is.na(player1_avg_welo) | is.na(player2_avg_welo))

# remove rows in subset_2024_m with NAs
subset_2022_m <- subset_2022_m %>%
  filter(!is.na(player1_avg_welo) & !is.na(player2_avg_welo))

# write.csv(subset_2024_m, "../data/wimbledon_subset_2024_m.csv", row.names = FALSE)

### females welo
wta_2022 <- tennis_data("2022", "WTA")
wta_2022_clean <- clean(wta_2022)

wta_2022_welo <- wta_2022_clean %>%
  filter(Tournament == "US Open", Surface == "Hard") 

wta_2022_welo <- welofit(wta_2022_welo)$results

# select only the P_i, P_j, WElo_i_before_match, and WElo_j_before_match columns. Then, 
# make into only two columns by putting the P_j and corresponding WElo_j_before_match in rows below the 
# P_i nd WElo_i_before_match columns.
wta_2022_welo_subset <- wta_2022_welo %>%
  select(P_i, P_j, WElo_i_before_match, WElo_j_before_match) %>%
  pivot_longer(cols = c(P_i, P_j, WElo_i_before_match, WElo_j_before_match), 
               names_to = c(".value", "player"), 
               names_pattern = "(.*)_(i|j)") %>%
  mutate(player = ifelse(player == "i", "player1", "player2")) %>%
  select(-player)

# find average welo for each player in atp_2021_welo_subset
wta_2022_welo_avg <- wta_2022_welo_subset %>%
  group_by(P) %>%
  summarise(
    avg_welo_before_match = mean(WElo, na.rm = TRUE),
    .groups = 'drop'
  )

wta_2022_welo_avg <- wta_2022_welo_avg %>%
  mutate(P = tolower(P))

# find number of unique entries in player1 and player2 for subset_2024_m
unique_players_2022_f <- unique(c(subset_2022_f$player1_name, subset_2022_f$player2_name)) 
length(unique_players_2022_f) # 127

# make P column in atp_2021_welo_avg all lowercase
wta_2022_welo_avg <- wta_2022_welo_avg %>%
  mutate(P = tolower(P))

# merge atp_2021_welo_avg with subset_2024_m to get the average welo for player1 and player2
subset_2022_f <- subset_2022_f %>%
  left_join(wta_2024_welo_avg, by = c("player1_name" = "P")) %>%
  rename(player1_avg_welo = avg_welo_before_match) %>%
  left_join(wta_2024_welo_avg, by = c("player2_name" = "P")) %>%
  rename(player2_avg_welo = avg_welo_before_match)

colSums(is.na(subset_2022_f))
# make new dataset that contains rows where there are NAs
subset_2022_f_na <- subset_2022_f %>%
  filter(is.na(player1_avg_welo) | is.na(player2_avg_welo))

# remove rows in subset_2024_m with NAs
subset_2022_f <- subset_2022_f %>%
  filter(!is.na(player1_avg_welo) & !is.na(player2_avg_welo))

# write.csv(subset_2024_f, "../data/wimbledon_subset_2024_f.csv", row.names = FALSE)

# make new column in subset_2024_m and subset_2024_f which transforms ElapsedTime into number of seconds elapsed

subset_2022_m <- subset_2022_m %>%
  separate(ElapsedTime, into = c("h", "m", "s"), sep = ":", convert = TRUE) %>%
  mutate(ElapsedSeconds = h * 3600 + m * 60 + s) %>%
  select(-h, -m, -s)

subset_2022_f <- subset_2022_f %>%
  separate(ElapsedTime, into = c("h", "m", "s"), sep = ":", convert = TRUE) %>%
  mutate(ElapsedSeconds = h * 3600 + m * 60 + s) %>%
  select(-h, -m, -s)

# subset_2024_m <- subset_2024_m %>%
#   mutate(ElapsedSeconds = as.numeric(
#     as.difftime(ElapsedTime, format = "%H:%M:%S", units = "secs")
#   ))
# 
# subset_2024_f <- subset_2024_f %>%
#   mutate(ElapsedSeconds = as.numeric(
#     as.difftime(ElapsedTime, format = "%H:%M:%S", units = "secs")
#   ))

write.csv(subset_2022_m, "out_data/usopen_subset_2022_m.csv", row.names = FALSE)
write.csv(subset_2022_f, "out_data/usopen_subset_2022_f.csv", row.names = FALSE)

#-----------------------------------------------------------------------------------------------------

## transform 2021 data

usopen_2021 <- usopen_2021 %>% 
  mutate(P1Score = as.character(P1Score),
         P2Score = as.character(P2Score),
         PointServer = as.integer(PointServer),
         PointWinner = as.integer(PointWinner),
         GameWinner = as.integer(GameWinner),
         server_score = if_else(PointServer == 1, P1Score, P2Score),
         returner_score = if_else(PointServer == 1, P2Score, P1Score),
         state = paste(server_score, returner_score, sep = "-")) %>% 
  filter(state %in% c("0-0", "15-0", "30-0", "40-0",
                      "0-15", "0-30", "0-40",
                      "15-15", "30-15", "40-15",
                      "15-30", "30-30", "40-30",
                      "15-40", "30-40", "40-40",
                      "40-AD", "AD-40"))

unique_states <- unique(c(usopen_2021$state)) 
length(unique_states) 

score_importance_dtmc <- as.data.table(read.csv("../data/score_importance_dtmc.csv"))

usopen_2021 <- left_join(usopen_2021, score_importance_dtmc, by = "state")
colSums(is.na(usopen_2021))

# divide male & female
usopen_2021_male <- usopen_2021[1:28127,]
usopen_2021_female <- usopen_2021[28128:nrow(usopen_2021),]

usopen_2021_male <- add_speed_ratio_column(usopen_2021_male)
usopen_2021_female <- add_speed_ratio_column(usopen_2021_female)

subset_2021_m <- usopen_2021_male %>% # to make it easier for us to look through relevant columns
  select(match_id, slam, year, ElapsedTime, SetNo, GameNo, PointNumber, player1, player2, Speed_MPH, ServeNumber, PointServer, PointWinner, 
         ServeWidth, ServeDepth, RallyCount, P1DistanceRun, P2DistanceRun, P1Score, P2Score, state, PointWinner, GameWinner,
         RallyCount, serving_player_won, speed_ratio, P1BreakPoint, P2BreakPoint, P1GamesWon, P2GamesWon, importance,
         P1Ace, P2Ace, P1DoubleFault, P2DoubleFault)

subset_2021_f <- usopen_2021_female %>% # to make it easier for us to look through relevant columns
  select(match_id, slam, year, ElapsedTime, SetNo, GameNo, PointNumber, player1, player2, Speed_MPH, ServeNumber, PointServer, PointWinner, 
         ServeWidth, ServeDepth, RallyCount, P1DistanceRun, P2DistanceRun, P1Score, P2Score, state, PointWinner, GameWinner,
         RallyCount, serving_player_won, speed_ratio, P1BreakPoint, P2BreakPoint, P1GamesWon, P2GamesWon, importance,
         P1Ace, P2Ace, P1DoubleFault, P2DoubleFault)

# add new column for player1_name being last name (which includes every word in player1 except for 
# the first word) followed by their first initial, then a period.

subset_2021_m <- subset_2021_m %>%
  rowwise() %>%
  mutate(
    player1_name = tolower(paste(
      paste(tail(strsplit(player1, " ")[[1]], -1), collapse = " "),
      paste0(substr(player1, 1, 1), "."))
    ),
    player2_name = tolower(paste(
      paste(tail(strsplit(player2, " ")[[1]], -1), collapse = " "),
      paste0(substr(player2, 1, 1), "."))
    )
  ) %>%
  ungroup()

subset_2021_f <- subset_2021_f %>%
  rowwise() %>%
  mutate(
    player1_name = tolower(paste(
      paste(tail(strsplit(player1, " ")[[1]], -1), collapse = " "),
      paste0(substr(player1, 1, 1), "."))
    ),
    player2_name = tolower(paste(
      paste(tail(strsplit(player2, " ")[[1]], -1), collapse = " "),
      paste0(substr(player2, 1, 1), "."))
    )
  ) %>%
  ungroup()

## male welo 2021
library(welo)
## males welo
atp_2021 <- tennis_data("2021", "ATP") # ATP = men's, WTA = women's
atp_2021_clean <- clean(atp_2021)

atp_2021_welo <- atp_2021_clean %>%
  filter(Series == "Grand Slam", Tournament == "US Open", Surface == "Hard") 

atp_2021_welo <- welofit(atp_2021_welo)$results

# select only the P_i, P_j, WElo_i_before_match, and WElo_j_before_match columns. Then, 
# make into only two columns by putting the P_j and corresponding WElo_j_before_match in rows below the 
# P_i nd WElo_i_before_match columns.
atp_2021_welo_subset <- atp_2021_welo %>%
  select(P_i, P_j, WElo_i_before_match, WElo_j_before_match) %>%
  pivot_longer(cols = c(P_i, P_j, WElo_i_before_match, WElo_j_before_match), 
               names_to = c(".value", "player"), 
               names_pattern = "(.*)_(i|j)") %>%
  mutate(player = ifelse(player == "i", "player1", "player2")) %>%
  select(-player)

# find average welo for each player in atp_2021_welo_subset
atp_2021_welo_avg <- atp_2021_welo_subset %>%
  group_by(P) %>%
  summarise(
    avg_welo_before_match = mean(WElo, na.rm = TRUE),
    .groups = 'drop'
  )

# find number of unique entries in player1 and player2 for subset_2024_m
unique_players_2021_m <- unique(c(subset_2021_m$player1_name, subset_2021_m$player2_name)) 
length(unique_players_2021_m)  # 128

# make P column in atp_2024_welo_avg all lowercase
atp_2021_welo_avg <- atp_2021_welo_avg %>%
  mutate(P = tolower(P))

# merge atp_2024_welo_avg with subset_2024_m to get the average welo for player1 and player2
subset_2021_m <- subset_2021_m %>%
  left_join(atp_2024_welo_avg, by = c("player1_name" = "P")) %>%
  rename(player1_avg_welo = avg_welo_before_match) %>%
  left_join(atp_2024_welo_avg, by = c("player2_name" = "P")) %>%
  rename(player2_avg_welo = avg_welo_before_match)

colSums(is.na(subset_2021_m))
# make new dataset that contains rows where there are NAs
subset_2021_m_na <- subset_2021_m %>%
  filter(is.na(player1_avg_welo) | is.na(player2_avg_welo))

# remove rows in subset_2024_m with NAs
subset_2021_m <- subset_2021_m %>%
  filter(!is.na(player1_avg_welo) & !is.na(player2_avg_welo))

# write.csv(subset_2024_m, "../data/wimbledon_subset_2024_m.csv", row.names = FALSE)

### females welo
wta_2021 <- tennis_data("2021", "WTA")
wta_2021_clean <- clean(wta_2021)

wta_2021_welo <- wta_2021_clean %>%
  filter(Tournament == "US Open", Surface == "Hard") 

wta_2021_welo <- welofit(wta_2021_welo)$results

# select only the P_i, P_j, WElo_i_before_match, and WElo_j_before_match columns. Then, 
# make into only two columns by putting the P_j and corresponding WElo_j_before_match in rows below the 
# P_i nd WElo_i_before_match columns.
wta_2021_welo_subset <- wta_2021_welo %>%
  select(P_i, P_j, WElo_i_before_match, WElo_j_before_match) %>%
  pivot_longer(cols = c(P_i, P_j, WElo_i_before_match, WElo_j_before_match), 
               names_to = c(".value", "player"), 
               names_pattern = "(.*)_(i|j)") %>%
  mutate(player = ifelse(player == "i", "player1", "player2")) %>%
  select(-player)

# find average welo for each player in atp_2021_welo_subset
wta_2021_welo_avg <- wta_2021_welo_subset %>%
  group_by(P) %>%
  summarise(
    avg_welo_before_match = mean(WElo, na.rm = TRUE),
    .groups = 'drop'
  )

wta_2021_welo_avg <- wta_2021_welo_avg %>%
  mutate(P = tolower(P))

# find number of unique entries in player1 and player2 for subset_2024_m
unique_players_2021_f <- unique(c(subset_2021_f$player1_name, subset_2021_f$player2_name)) 
length(unique_players_2021_f) # 127

# make P column in atp_2021_welo_avg all lowercase
wta_2021_welo_avg <- wta_2021_welo_avg %>%
  mutate(P = tolower(P))

# merge atp_2021_welo_avg with subset_2024_m to get the average welo for player1 and player2
subset_2021_f <- subset_2021_f %>%
  left_join(wta_2024_welo_avg, by = c("player1_name" = "P")) %>%
  rename(player1_avg_welo = avg_welo_before_match) %>%
  left_join(wta_2024_welo_avg, by = c("player2_name" = "P")) %>%
  rename(player2_avg_welo = avg_welo_before_match)

colSums(is.na(subset_2021_f))
# make new dataset that contains rows where there are NAs
subset_2021_f_na <- subset_2021_f %>%
  filter(is.na(player1_avg_welo) | is.na(player2_avg_welo))

# remove rows in subset_2024_m with NAs
subset_2021_f <- subset_2021_f %>%
  filter(!is.na(player1_avg_welo) & !is.na(player2_avg_welo))

# write.csv(subset_2024_f, "../data/wimbledon_subset_2024_f.csv", row.names = FALSE)

# make new column in subset_2024_m and subset_2024_f which transforms ElapsedTime into number of seconds elapsed

subset_2021_m <- subset_2021_m %>%
  separate(ElapsedTime, into = c("h", "m", "s"), sep = ":", convert = TRUE) %>%
  mutate(ElapsedSeconds = h * 3600 + m * 60 + s) %>%
  select(-h, -m, -s)

subset_2021_f <- subset_2021_f %>%
  separate(ElapsedTime, into = c("h", "m", "s"), sep = ":", convert = TRUE) %>%
  mutate(ElapsedSeconds = h * 3600 + m * 60 + s) %>%
  select(-h, -m, -s)

# subset_2024_m <- subset_2024_m %>%
#   mutate(ElapsedSeconds = as.numeric(
#     as.difftime(ElapsedTime, format = "%H:%M:%S", units = "secs")
#   ))
# 
# subset_2024_f <- subset_2024_f %>%
#   mutate(ElapsedSeconds = as.numeric(
#     as.difftime(ElapsedTime, format = "%H:%M:%S", units = "secs")
#   ))

write.csv(subset_2021_m, "out_data/usopen_subset_2021_m.csv", row.names = FALSE)
write.csv(subset_2021_f, "out_data/usopen_subset_2021_f.csv", row.names = FALSE)

#-----------------------------------------------------------------------------------------------------

