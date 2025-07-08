rm(list=ls())
# install.packages("welo")
library(welo)
library(tidyverse)
library(data.table)

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

### 2021 data

# binary variable for whether the point was won by the server
wimbledon_2021 <- as.data.table(read.csv("out_data/wimbledon_2021_combined.csv"))
names(wimbledon_2021)

#-----------------------------------------------------------------------------------------------------

wimbledon_2021 <- wimbledon_2021 %>% 
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

unique_states <- unique(c(wimbledon_2021$state)) 
length(unique_states) 

score_importance_dtmc <- as.data.table(read.csv("../data/score_importance_dtmc.csv"))

wimbledon_2021 <- left_join(wimbledon_2021, score_importance_dtmc, by = "state")
colSums(is.na(wimbledon_2021))

#-----------------------------------------------------------------------------------------------------

# divide male & female
wimbledon_2021_male <- wimbledon_2021[1:14983,]
wimbledon_2021_female <- wimbledon_2021[14984:nrow(wimbledon_2021),]

wimbledon_2021_male <- add_speed_ratio_column(wimbledon_2021_male)
wimbledon_2021_female <- add_speed_ratio_column(wimbledon_2021_female)

#-----------------------------------------------------------------------------------------------------

names(wimbledon_2021_male)

subset_2021_m <- wimbledon_2021_male %>% # to make it easier for us to look through relevant columns
  select(match_id, slam, year, ElapsedTime, SetNo, GameNo, PointNumber, player1, player2, Speed_MPH, ServeNumber, PointServer, PointWinner, 
         ServeWidth, ServeDepth, RallyCount, P1DistanceRun, P2DistanceRun, P1Score, P2Score, state, PointWinner, GameWinner,
         RallyCount, serving_player_won, speed_ratio, P1BreakPoint, P2BreakPoint, P1GamesWon, P2GamesWon, importance,
         P1Ace, P2Ace, P1DoubleFault, P2DoubleFault)

subset_2021_f <- wimbledon_2021_female %>% # to make it easier for us to look through relevant columns
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

#-----------------------------------------------------------------------------------------------------

### welo 2021 data

## males welo
atp_2021 <- tennis_data("2021", "ATP") # ATP = men's, WTA = women's
atp_2021_clean <- clean(atp_2021)

atp_2021_welo <- atp_2021_clean %>%
  filter(Series == "Grand Slam", Tournament == "Wimbledon", Surface == "Grass") 

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

# find number of unique entries in player1 and player2 for subset_2021_m
unique_players_2021_m <- unique(c(subset_2021_m$player1_name, subset_2021_m$player2_name)) 
length(unique_players_2021_m) 

# make P column in atp_2021_welo_avg all lowercase
atp_2021_welo_avg <- atp_2021_welo_avg %>%
  mutate(P = tolower(P))

# merge atp_2021_welo_avg with subset_2021_m to get the average welo for player1 and player2
subset_2021_m <- subset_2021_m %>%
  left_join(atp_2021_welo_avg, by = c("player1_name" = "P")) %>%
  rename(player1_avg_welo = avg_welo_before_match) %>%
  left_join(atp_2021_welo_avg, by = c("player2_name" = "P")) %>%
  rename(player2_avg_welo = avg_welo_before_match)

colSums(is.na(subset_2021_m))
# make new dataset that contains rows where there are NAs
subset_2021_m_na <- subset_2021_m %>%
  filter(is.na(player1_avg_welo) | is.na(player2_avg_welo))

# remove rows in subset_2021_m with NAs
subset_2021_m <- subset_2021_m %>%
  filter(!is.na(player1_avg_welo) & !is.na(player2_avg_welo))

# write.csv(subset_2024_m, "../data/wimbledon_subset_2024_m.csv", row.names = FALSE)

#-----------------------------------------------------------------------------------------------------

### females welo
wta_2021 <- tennis_data("2021", "WTA")
wta_2021_clean <- clean(wta_2021)

wta_2021_welo <- wta_2021_clean %>%
  filter(Tournament == "Wimbledon", Surface == "Grass") 

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

# find average welo for each player in atp_2022_welo_subset
wta_2021_welo_avg <- wta_2021_welo_subset %>%
  group_by(P) %>%
  summarise(
    avg_welo_before_match = mean(WElo, na.rm = TRUE),
    .groups = 'drop'
  )

wta_2021_welo_avg <- wta_2021_welo_avg %>%
  mutate(P = tolower(P))

# find number of unique entries in player1 and player2 for subset_2021_m
unique_players_2021_f <- unique(c(subset_2021_f$player1_name, subset_2021_f$player2_name)) 
length(unique_players_2021_f) # 69

# make P column in atp_2021_welo_avg all lowercase
wta_2021_welo_avg <- wta_2021_welo_avg %>%
  mutate(P = tolower(P))

# merge atp_2021_welo_avg with subset_2021_m to get the average welo for player1 and player2
subset_2021_f <- subset_2021_f %>%
  left_join(wta_2021_welo_avg, by = c("player1_name" = "P")) %>%
  rename(player1_avg_welo = avg_welo_before_match) %>%
  left_join(wta_2021_welo_avg, by = c("player2_name" = "P")) %>%
  rename(player2_avg_welo = avg_welo_before_match)

colSums(is.na(subset_2021_f))
# make new dataset that contains rows where there are NAs
subset_2021_f_na <- subset_2021_f %>%
  filter(is.na(player1_avg_welo) | is.na(player2_avg_welo))

# remove rows in subset_2021_m with NAs
subset_2021_f <- subset_2021_f %>%
  filter(!is.na(player1_avg_welo) & !is.na(player2_avg_welo))

# write.csv(subset_2024_f, "../data/wimbledon_subset_2024_f.csv", row.names = FALSE)

#-----------------------------------------------------------------------------------------------------

# make new column in subset_2024_m and subset_2024_f which transforms ElapsedTime into number of seconds elapsed

subset_2021_m <- subset_2021_m %>%
  separate(ElapsedTime, into = c("h", "m", "s"), sep = ":", convert = TRUE) %>%
  mutate(ElapsedSeconds = h * 3600 + m * 60 + s) %>%
  select(-h, -m, -s)

subset_2021_f <- subset_2021_f %>%
  separate(ElapsedTime, into = c("h", "m", "s"), sep = ":", convert = TRUE) %>%
  mutate(ElapsedSeconds = h * 3600 + m * 60 + s) %>%
  select(-h, -m, -s)

# subset_2021_m <- subset_2021_m %>%
#   mutate(ElapsedSeconds = as.numeric(
#     as.difftime(ElapsedTime, format = "%H:%M:%S", units = "secs")
#   ))
# 
# subset_2021_f <- subset_2021_f %>%
#   mutate(ElapsedSeconds = as.numeric(
#     as.difftime(ElapsedTime, format = "%H:%M:%S", units = "secs")
#   ))

write.csv(subset_2021_m, "out_data/wimbledon_subset_2021_m.csv", row.names = FALSE)
write.csv(subset_2021_f, "out_data/wimbledon_subset_2021_f.csv", row.names = FALSE)

#-----------------------------------------------------------------------------------------------------

## i believe these two should be all set for EDA now