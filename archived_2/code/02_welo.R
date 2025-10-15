rm(list = ls())

library(welo)
library(tidyverse)
library(data.table)

#--------------------------------------------------------------------------------
# Configuration

TOURNAMENT <- "usopen" # Change to "usopen" as needed
tournament_label <- case_when(
  TOURNAMENT == "wimbledon" ~ "Wimbledon",
  TOURNAMENT == "usopen"    ~ "US Open",
  TRUE                      ~ NA_character_
)

year <- 2024 # Change to the desired year
surface <- ifelse(TOURNAMENT == "wimbledon", "Grass", "Hard")
importance_file <- ifelse(TOURNAMENT == "wimbledon",
                          "../data/results/score_importance_dtmc_grass.csv",
                          "../data/results/score_importance_dtmc_hard.csv")

#--------------------------------------------------------------------------------
# Helper Functions

add_speed_ratio_column <- function(data) {
  avg_first_speed <- data %>%
    filter(ServeNumber == 1) %>%
    group_by(match_id) %>%
    summarise(
      avg_p1_speed = mean(Speed_MPH[ServeIndicator == 1 & Speed_MPH > 0], na.rm = TRUE),
      avg_p2_speed = mean(Speed_MPH[ServeIndicator == 2 & Speed_MPH > 0], na.rm = TRUE),
      .groups = 'drop'
    )
  
  data %>%
    left_join(avg_first_speed, by = "match_id") %>%
    mutate(speed_ratio = Speed_MPH / ifelse(ServeIndicator == 1, avg_p1_speed, avg_p2_speed))
}

format_player_name <- function(name) {
  parts <- strsplit(name, " ")[[1]]
  last <- tolower(paste(tail(parts, -1), collapse = " "))
  first_init <- paste0(tolower(substr(name, 1, 1)), ".")
  paste(last, first_init)
}

#--------------------------------------------------------------------------------
# Load and preprocess data

file_base <- paste0("../data/processed/", TOURNAMENT, "_", year, "_combined.csv")
data <- fread(file_base) %>%
  mutate(
    P1Score = as.character(P1Score),
    P2Score = as.character(P2Score),
    ServeIndicator = as.integer(ServeIndicator),
    PointWinner = as.integer(PointWinner),
    GameWinner = as.integer(GameWinner),
    server_score = if_else(ServeIndicator == 1, P1Score, P2Score),
    returner_score = if_else(ServeIndicator == 1, P2Score, P1Score),
    state = paste(server_score, returner_score, sep = "-")
  ) %>%
  filter(state %in% fread(importance_file)$state)

importance <- fread(importance_file)
data <- left_join(data, importance %>% select(state, importance), by = "state")

# Split male/female
cutoff_match <- paste0(year, "-", TOURNAMENT, "-1701")
data_m <- data %>% filter(match_id <= cutoff_match)
data_f <- data %>% filter(match_id >  cutoff_match)

# Add speed ratio
data_m <- add_speed_ratio_column(data_m)
data_f <- add_speed_ratio_column(data_f)

# Select and rename columns
select_columns <- function(df) {
  df %>%
    mutate(
      player1_name = sapply(player1, format_player_name),
      player2_name = sapply(player2, format_player_name)
    ) %>%
    select(match_id, year, ElapsedTime, SetNo, GameNo, PointNumber, player1, player2,
           ServeNumber, ServeIndicator, PointWinner, ServeWidth, ServeDepth,
           RallyCount, P1DistanceRun, P2DistanceRun, P1Score, P2Score, state,
           GameWinner, serving_player_won, speed_ratio, importance,
           P1BreakPoint, P2BreakPoint, P1GamesWon, P2GamesWon,
           P1Ace, P2Ace, P1DoubleFault, P2DoubleFault, Speed_MPH,
           player1_name, player2_name)
}

subset_m <- select_columns(data_m)
subset_f <- select_columns(data_f)

#--------------------------------------------------------------------------------
# Add Welo ratings

add_welo <- function(subset, gender) {
  level <- ifelse(gender == "male", "ATP", "WTA")
  match_data <- tennis_data(as.character(year), level) %>%
    clean() %>%
    filter(Tournament == tournament_label, Surface == surface)
  
  welo <- welofit(match_data)$results %>%
    select(P_i, P_j, WElo_i_before_match, WElo_j_before_match) %>%
    pivot_longer(cols = everything(), names_to = c(".value", "player"), names_pattern = "(.*)_(i|j)") %>%
    group_by(P) %>% summarise(avg_welo = mean(WElo, na.rm = TRUE), .groups = "drop") %>%
    mutate(P = tolower(P))
  
  subset %>%
    left_join(welo, by = c("player1_name" = "P")) %>%
    rename(player1_avg_welo = avg_welo) %>%
    left_join(welo, by = c("player2_name" = "P")) %>%
    rename(player2_avg_welo = avg_welo) %>%
    filter(!is.na(player1_avg_welo) & !is.na(player2_avg_welo)) %>%
    mutate(
      welo_p1_bt = 0.0057565 * player1_avg_welo,
      welo_p2_bt = 0.0057565 * player2_avg_welo,
      p_server_beats_returner = ifelse(ServeIndicator == 1,
                                       1 / (1 + exp(welo_p2_bt - welo_p1_bt)),
                                       1 / (1 + exp(welo_p1_bt - welo_p2_bt)))
    )
}

subset_m <- add_welo(subset_m, "male")
subset_f <- add_welo(subset_f, "female")

#--------------------------------------------------------------------------------
# Convert ElapsedTime to seconds

convert_elapsed <- function(df) {
  df %>%
    separate(ElapsedTime, into = c("h", "m", "s"), sep = ":", convert = TRUE) %>%
    mutate(ElapsedSeconds = h * 3600 + m * 60 + s) %>%
    select(-h, -m, -s)
}

subset_m <- convert_elapsed(subset_m)
subset_f <- convert_elapsed(subset_f)

#--------------------------------------------------------------------------------
# Write output

write.csv(subset_m, paste0("../data/processed/", TOURNAMENT, "_subset_", year, "_m.csv"), row.names = FALSE)
write.csv(subset_f, paste0("../data/processed/", TOURNAMENT, "_subset_", year, "_f.csv"), row.names = FALSE)
