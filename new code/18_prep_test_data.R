rm(list=ls()) # clear environment

# install.packages(c("rio", "data.table", "ggplot2","dplyr","tidyverse"))
library(rio)
library(data.table)
library(ggplot2)
library(dplyr)
library(tidyverse)

#-----------------------------------------------------------------------------------------------------

## usopen 2018
usopen_2018_matches <- as.data.table(read.csv("../data/raw_data/2018-usopen-matches.csv"))
colSums(is.na(usopen_2018_matches))

usopen_2018_points <- as.data.table(read.csv("../data/raw_data/2018-usopen-points.csv"))
colSums(is.na(usopen_2018_points))

# remove all cols with NAs
usopen_2018_points <- usopen_2018_points %>%
  select(where(~ all(!is.na(.))))
names(usopen_2018_points)

usopen_2018_matches <- usopen_2018_matches %>%
  select(where(~ all(!is.na(.))))
names(usopen_2018_matches)

usopen_2018 <- left_join(usopen_2018_points, usopen_2018_matches, by = "match_id")
names(usopen_2018)
colSums(is.na(usopen_2018))

# get rid of rows where there is no double fault, but Speed_MPH == 0
usopen_2018 <- usopen_2018 %>%
  filter(!(P1DoubleFault == 0 & P2DoubleFault == 0 & Speed_MPH == 0)) %>% 
  mutate(serving_player_won = ifelse((PointServer == 1 & PointWinner == 1) | (PointServer == 2 & PointWinner == 2), 1, 0))


# binary variable: whether serving player won or lost
# usopen_2018 <- usopen_2018 %>%
#   filter(PointServer != 0) %>%
#   mutate(serving_player_won = ifelse((PointServer == 1 & PointWinner == 1) | (PointServer == 2 & PointWinner == 2), 1, 0))


write.csv(usopen_2018, "out_data/usopen_2018_combined.csv", row.names = FALSE)

#-----------------------------------------------------------------------------------------------------

## usopen 2019

usopen_2019_matches <- as.data.table(read.csv("../data/raw_data/2019-usopen-matches.csv"))
colSums(is.na(usopen_2019_matches))

usopen_2019_points <- as.data.table(read.csv("../data/raw_data/2019-usopen-points.csv"))
colSums(is.na(usopen_2019_points))

# remove all cols with NAs
usopen_2019_points <- usopen_2019_points %>%
  select(where(~ all(!is.na(.))))
names(usopen_2019_points)

usopen_2019_matches <- usopen_2019_matches %>%
  select(where(~ all(!is.na(.))))
names(usopen_2019_matches)

usopen_2019 <- left_join(usopen_2019_points, usopen_2019_matches, by = "match_id")
names(usopen_2019)
colSums(is.na(usopen_2019))

# get rid of rows where there is no double fault, but Speed_MPH == 0
usopen_2019 <- usopen_2019 %>%
  filter(!(P1DoubleFault == 0 & P2DoubleFault == 0 & Speed_MPH == 0)) %>% 
  mutate(serving_player_won = ifelse((PointServer == 1 & PointWinner == 1) | (PointServer == 2 & PointWinner == 2), 1, 0))


# # binary variable: whether serving player won or lost
# usopen_2019 <- usopen_2019 %>%
#   filter(PointServer != 0) %>%
#   mutate(serving_player_won = ifelse((PointServer == 1 & PointWinner == 1) | (PointServer == 2 & PointWinner == 2), 1, 0))


write.csv(usopen_2019, "out_data/usopen_2019_combined.csv", row.names = FALSE)

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

## transform 2018 data

usopen_2018 <- usopen_2018 %>% 
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

unique_states <- unique(c(usopen_2018$state)) 
length(unique_states) 

score_importance_dtmc <- as.data.table(read.csv("../data/score_importance_dtmc.csv"))

usopen_2018 <- left_join(usopen_2018, score_importance_dtmc, by = "state")
colSums(is.na(usopen_2018))

# divide male & female
usopen_2018_male <- usopen_2018[1:18060,]
usopen_2018_female <- usopen_2018[18061:nrow(usopen_2018),]

usopen_2018_male <- add_speed_ratio_column(usopen_2018_male)
usopen_2018_female <- add_speed_ratio_column(usopen_2018_female)

subset_2018_m <- usopen_2018_male %>% # to make it easier for us to look through relevant columns
  select(match_id, slam, year, ElapsedTime, SetNo, GameNo, PointNumber, player1, player2, Speed_MPH, ServeNumber, PointServer, PointWinner, 
         ServeWidth, ServeDepth, RallyCount, P1DistanceRun, P2DistanceRun, P1Score, P2Score, state, PointWinner, GameWinner,
         RallyCount, serving_player_won, speed_ratio, P1BreakPoint, P2BreakPoint, P1GamesWon, P2GamesWon, importance,
         P1Ace, P2Ace, P1DoubleFault, P2DoubleFault)

subset_2018_f <- usopen_2018_female %>% # to make it easier for us to look through relevant columns
  select(match_id, slam, year, ElapsedTime, SetNo, GameNo, PointNumber, player1, player2, Speed_MPH, ServeNumber, PointServer, PointWinner, 
         ServeWidth, ServeDepth, RallyCount, P1DistanceRun, P2DistanceRun, P1Score, P2Score, state, PointWinner, GameWinner,
         RallyCount, serving_player_won, speed_ratio, P1BreakPoint, P2BreakPoint, P1GamesWon, P2GamesWon, importance,
         P1Ace, P2Ace, P1DoubleFault, P2DoubleFault)

# add new column for player1_name being last name (which includes every word in player1 except for 
# the first word) followed by their first initial, then a period.

subset_2018_m <- subset_2018_m %>%
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

subset_2018_f <- subset_2018_f %>%
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

## male welo 2018
library(welo)
## males welo
atp_2018 <- tennis_data("2018", "ATP") # ATP = men's, WTA = women's
atp_2018_clean <- clean(atp_2018)

atp_2018_welo <- atp_2018_clean %>%
  filter(Series == "Grand Slam", Tournament == "US Open", Surface == "Hard") 

atp_2018_welo <- welofit(atp_2018_welo)$results

# select only the P_i, P_j, WElo_i_before_match, and WElo_j_before_match columns. Then, 
# make into only two columns by putting the P_j and corresponding WElo_j_before_match in rows below the 
# P_i nd WElo_i_before_match columns.
atp_2018_welo_subset <- atp_2018_welo %>%
  select(P_i, P_j, WElo_i_before_match, WElo_j_before_match) %>%
  pivot_longer(cols = c(P_i, P_j, WElo_i_before_match, WElo_j_before_match), 
               names_to = c(".value", "player"), 
               names_pattern = "(.*)_(i|j)") %>%
  mutate(player = ifelse(player == "i", "player1", "player2")) %>%
  select(-player)

# find average welo for each player in atp_2021_welo_subset
atp_2018_welo_avg <- atp_2018_welo_subset %>%
  group_by(P) %>%
  summarise(
    avg_welo_before_match = mean(WElo, na.rm = TRUE),
    .groups = 'drop'
  )

# find number of unique entries in player1 and player2 for subset_2024_m
unique_players_2018_m <- unique(c(subset_2018_m$player1_name, subset_2018_m$player2_name)) 
length(unique_players_2018_m)  # 88

# make P column in atp_2024_welo_avg all lowercase
atp_2018_welo_avg <- atp_2018_welo_avg %>%
  mutate(P = tolower(P))

# merge atp_2024_welo_avg with subset_2024_m to get the average welo for player1 and player2
subset_2018_m <- subset_2018_m %>%
  left_join(atp_2018_welo_avg, by = c("player1_name" = "P")) %>%
  rename(player1_avg_welo = avg_welo_before_match) %>%
  left_join(atp_2018_welo_avg, by = c("player2_name" = "P")) %>%
  rename(player2_avg_welo = avg_welo_before_match)

colSums(is.na(subset_2018_m))
# make new dataset that contains rows where there are NAs
subset_2018_m_na <- subset_2018_m %>%
  filter(is.na(player1_avg_welo) | is.na(player2_avg_welo))

# remove rows in subset_2024_m with NAs
subset_2018_m <- subset_2018_m %>%
  filter(!is.na(player1_avg_welo) & !is.na(player2_avg_welo))

# write.csv(subset_2024_m, "../data/wimbledon_subset_2024_m.csv", row.names = FALSE)

### females welo
wta_2018 <- tennis_data("2018", "WTA")
wta_2018_clean <- clean(wta_2018)

wta_2018_welo <- wta_2018_clean %>%
  filter(Tournament == "US Open", Surface == "Hard") 

wta_2018_welo <- welofit(wta_2018_welo)$results

# select only the P_i, P_j, WElo_i_before_match, and WElo_j_before_match columns. Then, 
# make into only two columns by putting the P_j and corresponding WElo_j_before_match in rows below the 
# P_i nd WElo_i_before_match columns.
wta_2018_welo_subset <- wta_2018_welo %>%
  select(P_i, P_j, WElo_i_before_match, WElo_j_before_match) %>%
  pivot_longer(cols = c(P_i, P_j, WElo_i_before_match, WElo_j_before_match), 
               names_to = c(".value", "player"), 
               names_pattern = "(.*)_(i|j)") %>%
  mutate(player = ifelse(player == "i", "player1", "player2")) %>%
  select(-player)

# find average welo for each player in atp_2021_welo_subset
wta_2018_welo_avg <- wta_2018_welo_subset %>%
  group_by(P) %>%
  summarise(
    avg_welo_before_match = mean(WElo, na.rm = TRUE),
    .groups = 'drop'
  )

wta_2018_welo_avg <- wta_2018_welo_avg %>%
  mutate(P = tolower(P))

# find number of unique entries in player1 and player2 for subset_2024_m
unique_players_2018_f <- unique(c(subset_2018_f$player1_name, subset_2018_f$player2_name)) 
length(unique_players_2018_f) # 92

# make P column in atp_2021_welo_avg all lowercase
wta_2018_welo_avg <- wta_2018_welo_avg %>%
  mutate(P = tolower(P))

# merge atp_2021_welo_avg with subset_2024_m to get the average welo for player1 and player2
subset_2018_f <- subset_2018_f %>%
  left_join(wta_2018_welo_avg, by = c("player1_name" = "P")) %>%
  rename(player1_avg_welo = avg_welo_before_match) %>%
  left_join(wta_2018_welo_avg, by = c("player2_name" = "P")) %>%
  rename(player2_avg_welo = avg_welo_before_match)

colSums(is.na(subset_2018_f))
# make new dataset that contains rows where there are NAs
subset_2018_f_na <- subset_2018_f %>%
  filter(is.na(player1_avg_welo) | is.na(player2_avg_welo))

# remove rows in subset_2024_m with NAs
subset_2018_f <- subset_2018_f %>%
  filter(!is.na(player1_avg_welo) & !is.na(player2_avg_welo))

# write.csv(subset_2024_f, "../data/wimbledon_subset_2024_f.csv", row.names = FALSE)

# make new column in subset_2024_m and subset_2024_f which transforms ElapsedTime into number of seconds elapsed

subset_2018_m <- subset_2018_m %>%
  separate(ElapsedTime, into = c("h", "m", "s"), sep = ":", convert = TRUE) %>%
  mutate(ElapsedSeconds = h * 3600 + m * 60 + s) %>%
  select(-h, -m, -s)

subset_2018_f <- subset_2018_f %>%
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

write.csv(subset_2018_m, "out_data/usopen_subset_2018_m.csv", row.names = FALSE)
write.csv(subset_2018_f, "out_data/usopen_subset_2018_f.csv", row.names = FALSE)

#-----------------------------------------------------------------------------------------------------

## transform 2019 data

usopen_2019 <- usopen_2019 %>% 
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

unique_states <- unique(c(usopen_2019$state)) 
length(unique_states) 

score_importance_dtmc <- as.data.table(read.csv("../data/score_importance_dtmc.csv"))

usopen_2019 <- left_join(usopen_2019, score_importance_dtmc, by = "state")
colSums(is.na(usopen_2019))

# divide male & female
usopen_2019_male <- usopen_2019[1:16784,]
usopen_2019_female <- usopen_2019[16785:nrow(usopen_2019),]

usopen_2019_male <- add_speed_ratio_column(usopen_2019_male)
usopen_2019_female <- add_speed_ratio_column(usopen_2019_female)

subset_2019_m <- usopen_2019_male %>% # to make it easier for us to look through relevant columns
  select(match_id, slam, year, ElapsedTime, SetNo, GameNo, PointNumber, player1, player2, Speed_MPH, ServeNumber, PointServer, PointWinner, 
         ServeWidth, ServeDepth, RallyCount, P1DistanceRun, P2DistanceRun, P1Score, P2Score, state, PointWinner, GameWinner,
         RallyCount, serving_player_won, speed_ratio, P1BreakPoint, P2BreakPoint, P1GamesWon, P2GamesWon, importance,
         P1Ace, P2Ace, P1DoubleFault, P2DoubleFault)

subset_2019_f <- usopen_2019_female %>% # to make it easier for us to look through relevant columns
  select(match_id, slam, year, ElapsedTime, SetNo, GameNo, PointNumber, player1, player2, Speed_MPH, ServeNumber, PointServer, PointWinner, 
         ServeWidth, ServeDepth, RallyCount, P1DistanceRun, P2DistanceRun, P1Score, P2Score, state, PointWinner, GameWinner,
         RallyCount, serving_player_won, speed_ratio, P1BreakPoint, P2BreakPoint, P1GamesWon, P2GamesWon, importance,
         P1Ace, P2Ace, P1DoubleFault, P2DoubleFault)

# add new column for player1_name being last name (which includes every word in player1 except for 
# the first word) followed by their first initial, then a period.

subset_2019_m <- subset_2019_m %>%
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

subset_2019_f <- subset_2019_f %>%
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

## male welo 2019
library(welo)
## males welo
atp_2019 <- tennis_data("2019", "ATP") # ATP = men's, WTA = women's
atp_2019_clean <- clean(atp_2019)

atp_2019_welo <- atp_2019_clean %>%
  filter(Series == "Grand Slam", Tournament == "US Open", Surface == "Hard") 

atp_2019_welo <- welofit(atp_2019_welo)$results

# select only the P_i, P_j, WElo_i_before_match, and WElo_j_before_match columns. Then, 
# make into only two columns by putting the P_j and corresponding WElo_j_before_match in rows below the 
# P_i nd WElo_i_before_match columns.
atp_2019_welo_subset <- atp_2019_welo %>%
  select(P_i, P_j, WElo_i_before_match, WElo_j_before_match) %>%
  pivot_longer(cols = c(P_i, P_j, WElo_i_before_match, WElo_j_before_match), 
               names_to = c(".value", "player"), 
               names_pattern = "(.*)_(i|j)") %>%
  mutate(player = ifelse(player == "i", "player1", "player2")) %>%
  select(-player)

# find average welo for each player in atp_2021_welo_subset
atp_2019_welo_avg <- atp_2019_welo_subset %>%
  group_by(P) %>%
  summarise(
    avg_welo_before_match = mean(WElo, na.rm = TRUE),
    .groups = 'drop'
  )

# find number of unique entries in player1 and player2 for subset_2024_m
unique_players_2019_m <- unique(c(subset_2019_m$player1_name, subset_2019_m$player2_name)) 
length(unique_players_2019_m)  # 80

# make P column in atp_2024_welo_avg all lowercase
atp_2019_welo_avg <- atp_2019_welo_avg %>%
  mutate(P = tolower(P))

# merge atp_2024_welo_avg with subset_2024_m to get the average welo for player1 and player2
subset_2019_m <- subset_2019_m %>%
  left_join(atp_2019_welo_avg, by = c("player1_name" = "P")) %>%
  rename(player1_avg_welo = avg_welo_before_match) %>%
  left_join(atp_2019_welo_avg, by = c("player2_name" = "P")) %>%
  rename(player2_avg_welo = avg_welo_before_match)

colSums(is.na(subset_2019_m))
# make new dataset that contains rows where there are NAs
subset_2019_m_na <- subset_2019_m %>%
  filter(is.na(player1_avg_welo) | is.na(player2_avg_welo))

# remove rows in subset_2024_m with NAs
subset_2019_m <- subset_2019_m %>%
  filter(!is.na(player1_avg_welo) & !is.na(player2_avg_welo))

# write.csv(subset_2024_m, "../data/wimbledon_subset_2024_m.csv", row.names = FALSE)

### females welo
wta_2019 <- tennis_data("2019", "WTA")
wta_2019_clean <- clean(wta_2019)

wta_2019_welo <- wta_2019_clean %>%
  filter(Tournament == "US Open", Surface == "Hard") 

wta_2019_welo <- welofit(wta_2019_welo)$results

# select only the P_i, P_j, WElo_i_before_match, and WElo_j_before_match columns. Then, 
# make into only two columns by putting the P_j and corresponding WElo_j_before_match in rows below the 
# P_i nd WElo_i_before_match columns.
wta_2019_welo_subset <- wta_2019_welo %>%
  select(P_i, P_j, WElo_i_before_match, WElo_j_before_match) %>%
  pivot_longer(cols = c(P_i, P_j, WElo_i_before_match, WElo_j_before_match), 
               names_to = c(".value", "player"), 
               names_pattern = "(.*)_(i|j)") %>%
  mutate(player = ifelse(player == "i", "player1", "player2")) %>%
  select(-player)

# find average welo for each player in atp_2021_welo_subset
wta_2019_welo_avg <- wta_2019_welo_subset %>%
  group_by(P) %>%
  summarise(
    avg_welo_before_match = mean(WElo, na.rm = TRUE),
    .groups = 'drop'
  )

wta_2019_welo_avg <- wta_2019_welo_avg %>%
  mutate(P = tolower(P))

# find number of unique entries in player1 and player2 for subset_2024_m
unique_players_2019_f <- unique(c(subset_2019_f$player1_name, subset_2019_f$player2_name)) 
length(unique_players_2019_f) # 86

# make P column in atp_2021_welo_avg all lowercase
wta_2019_welo_avg <- wta_2019_welo_avg %>%
  mutate(P = tolower(P))

# merge atp_2021_welo_avg with subset_2024_m to get the average welo for player1 and player2
subset_2019_f <- subset_2019_f %>%
  left_join(wta_2019_welo_avg, by = c("player1_name" = "P")) %>%
  rename(player1_avg_welo = avg_welo_before_match) %>%
  left_join(wta_2019_welo_avg, by = c("player2_name" = "P")) %>%
  rename(player2_avg_welo = avg_welo_before_match)

colSums(is.na(subset_2019_f))
# make new dataset that contains rows where there are NAs
subset_2019_f_na <- subset_2019_f %>%
  filter(is.na(player1_avg_welo) | is.na(player2_avg_welo))

# remove rows in subset_2024_m with NAs
subset_2019_f <- subset_2019_f %>%
  filter(!is.na(player1_avg_welo) & !is.na(player2_avg_welo))

# write.csv(subset_2024_f, "../data/wimbledon_subset_2024_f.csv", row.names = FALSE)

# make new column in subset_2024_m and subset_2024_f which transforms ElapsedTime into number of seconds elapsed

subset_2019_m <- subset_2019_m %>%
  separate(ElapsedTime, into = c("h", "m", "s"), sep = ":", convert = TRUE) %>%
  mutate(ElapsedSeconds = h * 3600 + m * 60 + s) %>%
  select(-h, -m, -s)

subset_2019_f <- subset_2019_f %>%
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

write.csv(subset_2019_m, "out_data/usopen_subset_2019_m.csv", row.names = FALSE)
write.csv(subset_2019_f, "out_data/usopen_subset_2019_f.csv", row.names = FALSE)

#-----------------------------------------------------------------------------------------------------

# combine male data
subset_m <- rbindlist(list(
  subset_2018_m,
  subset_2019_m
))

subset_m <- subset_m %>%
  filter(ServeDepth != "", ServeWidth != "") %>% 
  filter(!is.na(speed_ratio))

# combine female data
subset_f <- rbindlist(list(
  subset_2018_f,
  subset_2019_f
))

subset_f <- subset_f %>%
  filter(ServeDepth != "", ServeWidth != "") %>% 
  filter(!is.na(speed_ratio))

# write.csv(subset_f, "test_subset_f.csv")

#-----------------------------------------------------------------------------------------------------

# fix large gaps in elapsed time (male)
subset_m <- subset_m %>% 
  arrange(match_id, PointNumber) %>% 
  group_by(match_id) %>% 
  mutate(
    time_diff  = ElapsedSeconds - lag(ElapsedSeconds),
    avg_dt_pos = mean(time_diff[ElapsedSeconds < 28800 & time_diff > 0],
                      na.rm = TRUE),
    jump       = ElapsedSeconds > 28800,
    offset     = cumsum(jump) * avg_dt_pos,
    ElapsedSeconds_fixed = ElapsedSeconds - offset
  ) %>% 
  ungroup() %>% 
  select(-time_diff, -avg_dt_pos, -jump, -offset)

# write.csv(subset_m, "../data/usopen_subset_m.csv", row.names = FALSE)

#-----------------------------------------------------------------------------------------------------

# fix large gaps in elapsed time (female)
problem_ids <- c("2019-usopen-2231",
                 "2019-usopen-2230",
                 "2019-usopen-2229")

## i'm raging and can't figure out a way to code this, so for now just delete these three match id's
subset_f <- subset_f %>%
  filter(!match_id %in% problem_ids)

## make ElapsedSeeconds_fixed column that is a duplicate of ElapsedSeconds
subset_f$ElapsedSeconds_fixed <- subset_f$ElapsedSeconds

# # ----------------------------------------------------------------------------- 
# # helper: repair one match’s clock  ➜ returns the match‐level data frame
# # -----------------------------------------------------------------------------
# repair_match <- function(df, reset_cut = 28800, max_gap = 600, fallback_gap = 30) {
#   df  <- arrange(df, PointNumber)
#   
#   # step-to-step change
#   diff_raw <- c(0, diff(df$ElapsedSeconds))
#   
#   # “typical” between-point gap (positive, < 10 min)
#   typ_gap  <- median(diff_raw[diff_raw > 0 & diff_raw < max_gap], na.rm = TRUE)
#   if (is.na(typ_gap) || typ_gap <= 0) typ_gap <- fallback_gap   # if jump comes right away
#   
#   # flag rows where raw clock leaps forward above 28 800 s
#   jump <- df$ElapsedSeconds > reset_cut
#   
#   # cumulative offset = (# of jumps so far) × typ_gap
#   offset <- cumsum(jump) * typ_gap
#   
#   df$ElapsedSeconds_fixed <- df$ElapsedSeconds - offset
#   df
# }
# 
# # ----------------------------------------------------------------------------- 
# # apply the repair ONLY to the three matches
# # ----------------------------------------------------------------------------- 
# subset_f_fixed <- subset_f %>%                     # your original male data
#   group_split(match_id) %>%                        # list of dfs, one per match
#   purrr::map_dfr(function(df) {
#     if (df$match_id[1] %in% problem_ids) {
#       repair_match(df)                             # fix the problem match
#     } else {
#       df$ElapsedSeconds_fixed <- df$ElapsedSeconds # leave others untouched
#       df
#     }
#   })
# 
# #-----------------------------------------------------------------------------------------------------
# 
## create bradley terry winning probabilities

# subset_m <- as.data.table(read.csv("../data/usopen_subset_m.csv"))
# subset_f <- as.data.table(read.csv("../data/usopen_subset_f.csv"))

names(subset_m)
subset_m <- as.data.table(subset_m)

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

write.csv(subset_m, "out_data/oos_test_usopen_subset_m.csv", row.names = FALSE)

#-----------------------------------------------------------------------------------------------------

# Convert ELO to logistic (Bradley-Terry scale)
names(subset_f)
subset_f <- as.data.table(subset_f)

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

write.csv(subset_f, "out_data/oos_test_usopen_subset_f.csv", row.names = FALSE)

# -----------------------------------------------------------------------------------------------------

## load 2018 and 2019 wimbledon data, then combine 
wimbledon_2018 <- as.data.table(read.csv("out_data/wimbledon_2018_combined.csv"))
wimbledon_2019 <- as.data.table(read.csv("out_data/wimbledon_2019_combined.csv"))

### transform 2018 data
wimbledon_2018 <- wimbledon_2018 %>% 
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

unique_states <- unique(c(wimbledon_2018$state)) 
length(unique_states) 

score_importance_dtmc <- as.data.table(read.csv("../data/score_importance_dtmc.csv"))

wimbledon_2018 <- left_join(wimbledon_2018, score_importance_dtmc, by = "state")
colSums(is.na(wimbledon_2018))

# divide male & female
wimbledon_2018_male <- wimbledon_2018[1:16833,]
wimbledon_2018_female <- wimbledon_2018[16834:nrow(wimbledon_2018),]

wimbledon_2018_male <- add_speed_ratio_column(wimbledon_2018_male)
wimbledon_2018_female <- add_speed_ratio_column(wimbledon_2018_female)

subset_2018_m <- wimbledon_2018_male %>% # to make it easier for us to look through relevant columns
  select(match_id, slam, year, ElapsedTime, SetNo, GameNo, PointNumber, player1, player2, Speed_MPH, ServeNumber, PointServer, PointWinner, 
         ServeWidth, ServeDepth, RallyCount, P1DistanceRun, P2DistanceRun, P1Score, P2Score, state, PointWinner, GameWinner,
         RallyCount, serving_player_won, speed_ratio, P1BreakPoint, P2BreakPoint, P1GamesWon, P2GamesWon, importance,
         P1Ace, P2Ace, P1DoubleFault, P2DoubleFault)

subset_2018_f <- wimbledon_2018_female %>% # to make it easier for us to look through relevant columns
  select(match_id, slam, year, ElapsedTime, SetNo, GameNo, PointNumber, player1, player2, Speed_MPH, ServeNumber, PointServer, PointWinner, 
         ServeWidth, ServeDepth, RallyCount, P1DistanceRun, P2DistanceRun, P1Score, P2Score, state, PointWinner, GameWinner,
         RallyCount, serving_player_won, speed_ratio, P1BreakPoint, P2BreakPoint, P1GamesWon, P2GamesWon, importance,
         P1Ace, P2Ace, P1DoubleFault, P2DoubleFault)

colSums(is.na(subset_2018_m))

# add new column for player1_name being last name (which includes every word in player1 except for 
# the first word) followed by their first initial, then a period.

subset_2018_m <- subset_2018_m %>%
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

subset_2018_f <- subset_2018_f %>%
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

## male welo 2018
library(welo)
## males welo
atp_2018 <- tennis_data("2018", "ATP") # ATP = men's, WTA = women's
atp_2018_clean <- clean(atp_2018)

atp_2018_welo <- atp_2018_clean %>%
  filter(Series == "Grand Slam", Tournament == "Wimbledon", Surface == "Grass") 

atp_2018_welo <- welofit(atp_2018_welo)$results

# select only the P_i, P_j, WElo_i_before_match, and WElo_j_before_match columns. Then, 
# make into only two columns by putting the P_j and corresponding WElo_j_before_match in rows below the 
# P_i nd WElo_i_before_match columns.
atp_2018_welo_subset <- atp_2018_welo %>%
  select(P_i, P_j, WElo_i_before_match, WElo_j_before_match) %>%
  pivot_longer(cols = c(P_i, P_j, WElo_i_before_match, WElo_j_before_match), 
               names_to = c(".value", "player"), 
               names_pattern = "(.*)_(i|j)") %>%
  mutate(player = ifelse(player == "i", "player1", "player2")) %>%
  select(-player)

# find average welo for each player in atp_2021_welo_subset
atp_2018_welo_avg <- atp_2018_welo_subset %>%
  group_by(P) %>%
  summarise(
    avg_welo_before_match = mean(WElo, na.rm = TRUE),
    .groups = 'drop'
  )

# find number of unique entries in player1 and player2 for subset_2024_m
unique_players_2018_m <- unique(c(subset_2018_m$player1_name, subset_2018_m$player2_name)) 
length(unique_players_2018_m)  # 76

# make P column in atp_2024_welo_avg all lowercase
atp_2018_welo_avg <- atp_2018_welo_avg %>%
  mutate(P = tolower(P))

# merge atp_2024_welo_avg with subset_2024_m to get the average welo for player1 and player2
subset_2018_m <- subset_2018_m %>%
  left_join(atp_2018_welo_avg, by = c("player1_name" = "P")) %>%
  rename(player1_avg_welo = avg_welo_before_match) %>%
  left_join(atp_2018_welo_avg, by = c("player2_name" = "P")) %>%
  rename(player2_avg_welo = avg_welo_before_match)

colSums(is.na(subset_2018_m))
# make new dataset that contains rows where there are NAs
subset_2018_m_na <- subset_2018_m %>%
  filter(is.na(player1_avg_welo) | is.na(player2_avg_welo))

# remove rows in subset_2024_m with NAs
subset_2018_m <- subset_2018_m %>%
  filter(!is.na(player1_avg_welo) & !is.na(player2_avg_welo))

# write.csv(subset_2024_m, "../data/wimbledon_subset_2024_m.csv", row.names = FALSE)

### females welo
wta_2018 <- tennis_data("2018", "WTA")
wta_2018_clean <- clean(wta_2018)

wta_2018_welo <- wta_2018_clean %>%
  filter(Tournament == "Wimbledon", Surface == "Grass") 

wta_2018_welo <- welofit(wta_2018_welo)$results

# select only the P_i, P_j, WElo_i_before_match, and WElo_j_before_match columns. Then, 
# make into only two columns by putting the P_j and corresponding WElo_j_before_match in rows below the 
# P_i nd WElo_i_before_match columns.
wta_2018_welo_subset <- wta_2018_welo %>%
  select(P_i, P_j, WElo_i_before_match, WElo_j_before_match) %>%
  pivot_longer(cols = c(P_i, P_j, WElo_i_before_match, WElo_j_before_match), 
               names_to = c(".value", "player"), 
               names_pattern = "(.*)_(i|j)") %>%
  mutate(player = ifelse(player == "i", "player1", "player2")) %>%
  select(-player)

# find average welo for each player in atp_2021_welo_subset
wta_2018_welo_avg <- wta_2018_welo_subset %>%
  group_by(P) %>%
  summarise(
    avg_welo_before_match = mean(WElo, na.rm = TRUE),
    .groups = 'drop'
  )

wta_2018_welo_avg <- wta_2018_welo_avg %>%
  mutate(P = tolower(P))

# find number of unique entries in player1 and player2 for subset_2024_m
unique_players_2018_f <- unique(c(subset_2018_f$player1_name, subset_2018_f$player2_name)) 
length(unique_players_2018_f) # 76

# make P column in atp_2021_welo_avg all lowercase
wta_2018_welo_avg <- wta_2018_welo_avg %>%
  mutate(P = tolower(P))

# merge atp_2021_welo_avg with subset_2024_m to get the average welo for player1 and player2
subset_2018_f <- subset_2018_f %>%
  left_join(wta_2018_welo_avg, by = c("player1_name" = "P")) %>%
  rename(player1_avg_welo = avg_welo_before_match) %>%
  left_join(wta_2018_welo_avg, by = c("player2_name" = "P")) %>%
  rename(player2_avg_welo = avg_welo_before_match)

colSums(is.na(subset_2018_f))
# make new dataset that contains rows where there are NAs
subset_2018_f_na <- subset_2018_f %>%
  filter(is.na(player1_avg_welo) | is.na(player2_avg_welo))

# remove rows in subset_2024_m with NAs
subset_2018_f <- subset_2018_f %>%
  filter(!is.na(player1_avg_welo) & !is.na(player2_avg_welo))

# write.csv(subset_2024_f, "../data/wimbledon_subset_2024_f.csv", row.names = FALSE)

# make new column in subset_2024_m and subset_2024_f which transforms ElapsedTime into number of seconds elapsed

subset_2018_m <- subset_2018_m %>%
  separate(ElapsedTime, into = c("h", "m", "s"), sep = ":", convert = TRUE) %>%
  mutate(ElapsedSeconds = h * 3600 + m * 60 + s) %>%
  select(-h, -m, -s)

subset_2018_f <- subset_2018_f %>%
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

write.csv(subset_2018_m, "out_data/wimbledon_subset_2018_m.csv", row.names = FALSE)
write.csv(subset_2018_f, "out_data/wimbledon_subset_2018_f.csv", row.names = FALSE)

#-----------------------------------------------------------------------------------------------------

## transform 2019 data

wimbledon_2019 <- wimbledon_2019 %>% 
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

unique_states <- unique(c(wimbledon_2019$state)) 
length(unique_states) 

score_importance_dtmc <- as.data.table(read.csv("../data/score_importance_dtmc.csv"))

wimbledon_2019 <- left_join(wimbledon_2019, score_importance_dtmc, by = "state")
colSums(is.na(wimbledon_2019))

# divide male & female
wimbledon_2019_male <- wimbledon_2019[1:16174,]
wimbledon_2019_female <- wimbledon_2019[16175:nrow(wimbledon_2019),]

wimbledon_2019_male <- add_speed_ratio_column(wimbledon_2019_male)
wimbledon_2019_female <- add_speed_ratio_column(wimbledon_2019_female)

subset_2019_m <- wimbledon_2019_male %>% # to make it easier for us to look through relevant columns
  select(match_id, slam, year, ElapsedTime, SetNo, GameNo, PointNumber, player1, player2, Speed_MPH, ServeNumber, PointServer, PointWinner, 
         ServeWidth, ServeDepth, RallyCount, P1DistanceRun, P2DistanceRun, P1Score, P2Score, state, PointWinner, GameWinner,
         RallyCount, serving_player_won, speed_ratio, P1BreakPoint, P2BreakPoint, P1GamesWon, P2GamesWon, importance,
         P1Ace, P2Ace, P1DoubleFault, P2DoubleFault)

subset_2019_f <- wimbledon_2019_female %>% # to make it easier for us to look through relevant columns
  select(match_id, slam, year, ElapsedTime, SetNo, GameNo, PointNumber, player1, player2, Speed_MPH, ServeNumber, PointServer, PointWinner, 
         ServeWidth, ServeDepth, RallyCount, P1DistanceRun, P2DistanceRun, P1Score, P2Score, state, PointWinner, GameWinner,
         RallyCount, serving_player_won, speed_ratio, P1BreakPoint, P2BreakPoint, P1GamesWon, P2GamesWon, importance,
         P1Ace, P2Ace, P1DoubleFault, P2DoubleFault)

# add new column for player1_name being last name (which includes every word in player1 except for 
# the first word) followed by their first initial, then a period.

subset_2019_m <- subset_2019_m %>%
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

subset_2019_f <- subset_2019_f %>%
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

## male welo 2019
library(welo)
## males welo
atp_2019 <- tennis_data("2019", "ATP") # ATP = men's, WTA = women's
atp_2019_clean <- clean(atp_2019)

atp_2019_welo <- atp_2019_clean %>%
  filter(Series == "Grand Slam", Tournament == "Wimbledon", Surface == "Grass") 

atp_2019_welo <- welofit(atp_2019_welo)$results

# select only the P_i, P_j, WElo_i_before_match, and WElo_j_before_match columns. Then, 
# make into only two columns by putting the P_j and corresponding WElo_j_before_match in rows below the 
# P_i nd WElo_i_before_match columns.
atp_2019_welo_subset <- atp_2019_welo %>%
  select(P_i, P_j, WElo_i_before_match, WElo_j_before_match) %>%
  pivot_longer(cols = c(P_i, P_j, WElo_i_before_match, WElo_j_before_match), 
               names_to = c(".value", "player"), 
               names_pattern = "(.*)_(i|j)") %>%
  mutate(player = ifelse(player == "i", "player1", "player2")) %>%
  select(-player)

# find average welo for each player in atp_2021_welo_subset
atp_2019_welo_avg <- atp_2019_welo_subset %>%
  group_by(P) %>%
  summarise(
    avg_welo_before_match = mean(WElo, na.rm = TRUE),
    .groups = 'drop'
  )

# find number of unique entries in player1 and player2 for subset_2024_m
unique_players_2019_m <- unique(c(subset_2019_m$player1_name, subset_2019_m$player2_name)) 
length(unique_players_2019_m)  # 73

# make P column in atp_2024_welo_avg all lowercase
atp_2019_welo_avg <- atp_2019_welo_avg %>%
  mutate(P = tolower(P))

# merge atp_2024_welo_avg with subset_2024_m to get the average welo for player1 and player2
subset_2019_m <- subset_2019_m %>%
  left_join(atp_2019_welo_avg, by = c("player1_name" = "P")) %>%
  rename(player1_avg_welo = avg_welo_before_match) %>%
  left_join(atp_2019_welo_avg, by = c("player2_name" = "P")) %>%
  rename(player2_avg_welo = avg_welo_before_match)

colSums(is.na(subset_2019_m))
# make new dataset that contains rows where there are NAs
subset_2019_m_na <- subset_2019_m %>%
  filter(is.na(player1_avg_welo) | is.na(player2_avg_welo))

# remove rows in subset_2024_m with NAs
subset_2019_m <- subset_2019_m %>%
  filter(!is.na(player1_avg_welo) & !is.na(player2_avg_welo))

# write.csv(subset_2024_m, "../data/wimbledon_subset_2024_m.csv", row.names = FALSE)

### females welo
wta_2019 <- tennis_data("2019", "WTA")
wta_2019_clean <- clean(wta_2019)

wta_2019_welo <- wta_2019_clean %>%
  filter(Tournament == "Wimbledon", Surface == "Grass") 

wta_2019_welo <- welofit(wta_2019_welo)$results

# select only the P_i, P_j, WElo_i_before_match, and WElo_j_before_match columns. Then, 
# make into only two columns by putting the P_j and corresponding WElo_j_before_match in rows below the 
# P_i nd WElo_i_before_match columns.
wta_2019_welo_subset <- wta_2019_welo %>%
  select(P_i, P_j, WElo_i_before_match, WElo_j_before_match) %>%
  pivot_longer(cols = c(P_i, P_j, WElo_i_before_match, WElo_j_before_match), 
               names_to = c(".value", "player"), 
               names_pattern = "(.*)_(i|j)") %>%
  mutate(player = ifelse(player == "i", "player1", "player2")) %>%
  select(-player)

# find average welo for each player in atp_2021_welo_subset
wta_2019_welo_avg <- wta_2019_welo_subset %>%
  group_by(P) %>%
  summarise(
    avg_welo_before_match = mean(WElo, na.rm = TRUE),
    .groups = 'drop'
  )

wta_2019_welo_avg <- wta_2019_welo_avg %>%
  mutate(P = tolower(P))

# find number of unique entries in player1 and player2 for subset_2024_m
unique_players_2019_f <- unique(c(subset_2019_f$player1_name, subset_2019_f$player2_name)) 
length(unique_players_2019_f) # 82

# make P column in atp_2021_welo_avg all lowercase
wta_2019_welo_avg <- wta_2019_welo_avg %>%
  mutate(P = tolower(P))

# merge atp_2021_welo_avg with subset_2024_m to get the average welo for player1 and player2
subset_2019_f <- subset_2019_f %>%
  left_join(wta_2019_welo_avg, by = c("player1_name" = "P")) %>%
  rename(player1_avg_welo = avg_welo_before_match) %>%
  left_join(wta_2019_welo_avg, by = c("player2_name" = "P")) %>%
  rename(player2_avg_welo = avg_welo_before_match)

colSums(is.na(subset_2019_f))
# make new dataset that contains rows where there are NAs
subset_2019_f_na <- subset_2019_f %>%
  filter(is.na(player1_avg_welo) | is.na(player2_avg_welo))

# remove rows in subset_2024_m with NAs
subset_2019_f <- subset_2019_f %>%
  filter(!is.na(player1_avg_welo) & !is.na(player2_avg_welo))

# write.csv(subset_2024_f, "../data/wimbledon_subset_2024_f.csv", row.names = FALSE)

# make new column in subset_2024_m and subset_2024_f which transforms ElapsedTime into number of seconds elapsed

subset_2019_m <- subset_2019_m %>%
  separate(ElapsedTime, into = c("h", "m", "s"), sep = ":", convert = TRUE) %>%
  mutate(ElapsedSeconds = h * 3600 + m * 60 + s) %>%
  select(-h, -m, -s)

subset_2019_f <- subset_2019_f %>%
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

write.csv(subset_2019_m, "out_data/wimbledon_subset_2019_m.csv", row.names = FALSE)
write.csv(subset_2019_f, "out_data/wimbledon_subset_2019_f.csv", row.names = FALSE)

#-----------------------------------------------------------------------------------------------------

# combine male data
subset_m <- rbindlist(list(
  subset_2018_m,
  subset_2019_m
))

subset_m <- subset_m %>%
  filter(ServeDepth != "", ServeWidth != "") %>% 
  filter(!is.na(speed_ratio))

# combine female data
subset_f <- rbindlist(list(
  subset_2018_f,
  subset_2019_f
))

subset_f <- subset_f %>%
  filter(ServeDepth != "", ServeWidth != "") %>% 
  filter(!is.na(speed_ratio))

# write.csv(subset_f, "test_subset_f.csv")

#-----------------------------------------------------------------------------------------------------

# fix large gaps in elapsed time (male)
problem_ids <- subset_m %>%
  filter(ElapsedSeconds > 28800) %>%   # keep only the “bad” rows
  distinct(match_id) %>%               # collapse to unique IDs
  pull(match_id)  

subset_m <- subset_m %>%
  filter(!match_id %in% problem_ids)

subset_m$ElapsedSeconds_fixed <- subset_m$ElapsedSeconds

# histogram of ElapsedSeconds_fixed
ggplot(subset_m, aes(x = ElapsedSeconds_fixed)) +
  geom_histogram(color = "black", fill = "steelblue", na.rm = TRUE) 

# write.csv(subset_m, "../data/usopen_subset_m.csv", row.names = FALSE)

#-----------------------------------------------------------------------------------------------------

# fix large gaps in elapsed time (female)
problem_ids <- subset_f %>%
  filter(ElapsedSeconds > 17600) %>%   # keep only the “bad” rows
  distinct(match_id) %>%               # collapse to unique IDs
  pull(match_id) 

## make ElapsedSeeconds_fixed column that is a duplicate of ElapsedSeconds
subset_f$ElapsedSeconds_fixed <- subset_f$ElapsedSeconds

#-----------------------------------------------------------------------------------------------------
# 
## create bradley terry winning probabilities

# subset_m <- as.data.table(read.csv("../data/usopen_subset_m.csv"))
# subset_f <- as.data.table(read.csv("../data/usopen_subset_f.csv"))

names(subset_m)
subset_m <- as.data.table(subset_m)

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

write.csv(subset_m, "out_data/oos_test_wimbledon_subset_m.csv", row.names = FALSE)

#-----------------------------------------------------------------------------------------------------

# Convert ELO to logistic (Bradley-Terry scale)
names(subset_f)
subset_f <- as.data.table(subset_f)

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

write.csv(subset_f, "out_data/oos_test_wimbledon_subset_f.csv", row.names = FALSE)

#-----------------------------------------------------------------------------------------------------

## standardize important cols

subset_m <- as.data.table(read.csv("out_data/oos_test_wimbledon_subset_m.csv"))
subset_f <- as.data.table(read.csv("out_data/oos_test_wimbledon_subset_f.csv"))

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
subset_m_test <- subset_m %>% 
  # identify who is serving this point
  mutate(ServerName = if_else(PointServer == 1, player1, player2)) %>% 
  
  # now the key columns exist in both tables
  left_join(all_rates, by = c("match_id", "ServerName"))

colSums(is.na(subset_m_test))

# 1) get every player who ever served
servers <- subset_f %>%
  filter(ServeNumber %in% c(1,2)) %>%
  mutate(ServerName = if_else(PointServer == 1, player1, player2)) %>%
  pull(ServerName) %>%
  unique()

# 2) function to compute df_pct for one server
compute_df_for <- function(name) {
  subset_f%>%
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
subset_f_test <- subset_f %>% 
  # identify who is serving this point
  mutate(ServerName = if_else(PointServer == 1, player1, player2)) %>% 
  
  # now the key columns exist in both tables
  left_join(all_rates, by = c("match_id", "ServerName"))

colSums(is.na(subset_f_test))
#-----------------------------------------------------------------------------------------------------

cols_to_standardize <- c(
  "Speed_MPH",
  "speed_ratio",
  "ElapsedSeconds_fixed",
  "df_pct_server",
  "p_server_beats_returner",
  "importance"
)

subset_m <- subset_m_test %>% 
  mutate(
    across(
      all_of(cols_to_standardize),
      ~ as.numeric(scale(.x, center = TRUE, scale = TRUE)),   # mean 0, sd 1
      .names = "{.col}_z"
    )
  )

subset_f <- subset_f_test %>%
  mutate(
    across(
      all_of(cols_to_standardize),
      ~ as.numeric(scale(.x, center = TRUE, scale = TRUE)),   # mean 0, sd 1
      .names = "{.col}_z"
    )
  )

# write the standardized data to csv
write.csv(subset_m, "out_data/scaled/wimbledon_subset_m_testing.csv", row.names = FALSE)
write.csv(subset_f, "out_data/scaled/wimbledon_subset_f_testing.csv", row.names = FALSE)

#-----------------------------------------------------------------------------------------------------

## usopen: standardize important cols

subset_m <- as.data.table(read.csv("out_data/oos_test_usopen_subset_m.csv"))
subset_f <- as.data.table(read.csv("out_data/oos_test_usopen_subset_f.csv"))

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
subset_m_test <- subset_m %>% 
  # identify who is serving this point
  mutate(ServerName = if_else(PointServer == 1, player1, player2)) %>% 
  
  # now the key columns exist in both tables
  left_join(all_rates, by = c("match_id", "ServerName"))

colSums(is.na(subset_m_test))

# same with females
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
subset_f_test <- subset_f %>% 
  # identify who is serving this point
  mutate(ServerName = if_else(PointServer == 1, player1, player2)) %>% 
  
  # now the key columns exist in both tables
  left_join(all_rates, by = c("match_id", "ServerName"))

colSums(is.na(subset_f_test))
#-----------------------------------------------------------------------------------------------------


cols_to_standardize <- c(
  "Speed_MPH",
  "speed_ratio",
  "ElapsedSeconds_fixed",
  "df_pct_server",
  "p_server_beats_returner",
  "importance"
)

subset_m <- subset_m_test %>% 
  mutate(
    across(
      all_of(cols_to_standardize),
      ~ as.numeric(scale(.x, center = TRUE, scale = TRUE)),   # mean 0, sd 1
      .names = "{.col}_z"
    )
  )

subset_f <- subset_f_test %>%
  mutate(
    across(
      all_of(cols_to_standardize),
      ~ as.numeric(scale(.x, center = TRUE, scale = TRUE)),   # mean 0, sd 1
      .names = "{.col}_z"
    )
  )

# write the standardized data to csv
write.csv(subset_m, "out_data/scaled/usopen_subset_m_testing.csv", row.names = FALSE)
write.csv(subset_f, "out_data/scaled/usopen_subset_f_testing.csv", row.names = FALSE)

#-----------------------------------------------------------------------------------------------------

