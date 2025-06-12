# rm(list=ls())
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

### 2024 data

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
  select(match_id, slam, year, PointNumber, player1, player2, Speed_MPH, ServeNumber, PointServer, PointWinner, ServeWidth, ServeDepth, RallyCount, GameNo, P1DistanceRun,
         P2DistanceRun, RallyCount, serving_player_won, speed_ratio)

subset_2024_f <- wimbledon_2024_female %>% # to make it easier for us to look through relevant columns
  select(match_id, slam, year, PointNumber, player1, player2, Speed_MPH, ServeNumber, PointServer, PointWinner, ServeWidth, ServeDepth, RallyCount, GameNo, P1DistanceRun,
         P2DistanceRun, RallyCount, serving_player_won, speed_ratio)

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

### welo 2024 data

## males
atp_2024 <- tennis_data("2024", "ATP") # ATP = men's, WTA = women's
atp_2024_clean <- clean(atp_2024)

atp_2024_welo <- atp_2024_clean %>%
  filter(Series == "Grand Slam", Tournament == "Wimbledon", Surface == "Grass") 

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
length(unique_players_2024_m) # 71

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

write.csv(subset_2024_m, "../data/wimbledon_subset_2024_m.csv", row.names = FALSE)

### females
wta_2024 <- tennis_data("2024", "WTA")
wta_2024_clean <- clean(wta_2024)

wta_2024_welo <- wta_2024_clean %>%
  filter(Tournament == "Wimbledon", Surface == "Grass") 

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
length(unique_players_2024_f) # 72

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

write.csv(subset_2024_f, "../data/wimbledon_subset_2024_f.csv", row.names = FALSE)
