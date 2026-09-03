# Added by the 2026-08-27 inactive-project compression migration.
.codex_write_csv_gz <- function(x, file = "", ...) {
  if (is.character(file) && length(file) == 1L && grepl("\\.gz$", file, ignore.case = TRUE)) {
    con <- gzfile(file, open = "wt")
    on.exit(close(con), add = TRUE)
    utils::write.csv(x, file = con, ...)
  } else {
    utils::write.csv(x, file = file, ...)
  }
}

rm(list = ls())

library(welo)
library(tidyverse)
library(data.table)

#--------------------------------------------------------------------------------
# Configuration

years <- c(2016, 2017,
           2018, 2019, 2021, 2022, 2023, 2024)

tournaments <- c("wimbledon", "usopen")

#--------------------------------------------------------------------------------
# Helper Functions

add_speed_ratio_column <- function(data) {
  avg_first_speed <- data %>%
    filter(ServeNumber == 1) %>%
    group_by(match_id) %>%
    summarise(
      avg_p1_speed = mean(Speed_MPH[ServeIndicator == 1 & Speed_MPH > 0], na.rm = TRUE),
      avg_p2_speed = mean(Speed_MPH[ServeIndicator == 2 & Speed_MPH > 0], na.rm = TRUE),
      .groups = "drop"
    )
  
  data %>%
    left_join(avg_first_speed, by = "match_id") %>%
    mutate(
      speed_ratio = Speed_MPH / ifelse(ServeIndicator == 1, avg_p1_speed, avg_p2_speed)
    )
}

format_player_name <- function(name) {
  name <- as.character(name)[1]
  
  if (is.na(name) || str_trim(name) == "") {
    return(NA_character_)
  }
  
  parts <- str_split(str_squish(name), "\\s+")[[1]]
  
  if (length(parts) == 1) {
    return(tolower(parts[1]))
  }
  
  last <- tolower(paste(parts[-1], collapse = " "))
  first_init <- paste0(tolower(substr(parts[1], 1, 1)), ".")
  
  paste(last, first_init)
}

add_welo <- function(subset, gender, year, tournament_label, surface) {
  level <- ifelse(gender == "male", "ATP", "WTA")
  
  match_data <- tennis_data(as.character(year), level) %>%
    clean() %>%
    filter(Tournament == tournament_label, Surface == surface)
  
  welo_results <- welofit(match_data)$results
  
  welo_pairs <- welo_results %>%
    transmute(
      player1_name = as.character(tolower(P_i)),
      player2_name = as.character(tolower(P_j)),
      player1_avg_welo = WElo_i_before_match,
      player2_avg_welo = WElo_j_before_match
    )
  
  welo_pairs_swapped <- welo_results %>%
    transmute(
      player1_name = as.character(tolower(P_j)),
      player2_name = as.character(tolower(P_i)),
      player1_avg_welo = WElo_j_before_match,
      player2_avg_welo = WElo_i_before_match
    )
  
  welo_pairs_all <- bind_rows(welo_pairs, welo_pairs_swapped) %>%
    distinct(player1_name, player2_name, .keep_all = TRUE)
  
  subset %>%
    mutate(
      player1_name = as.character(player1_name),
      player2_name = as.character(player2_name)
    ) %>%
    left_join(welo_pairs_all, by = c("player1_name", "player2_name")) %>%
    filter(!is.na(player1_avg_welo) & !is.na(player2_avg_welo))
}

convert_elapsed <- function(df) {
  df %>%
    separate(ElapsedTime, into = c("h", "m", "s"), sep = ":", convert = TRUE, remove = FALSE) %>%
    mutate(
      ElapsedSeconds = h * 3600 + m * 60 + s
    ) %>%
    select(-h, -m, -s)
}

#--------------------------------------------------------------------------------
# Main Processing Function

process_tournament_year <- function(year, tournament) {
  short_tournament <- ifelse(tournament == "usopen", "us", "wimb")
  dir.create("data/processed/subset", recursive = TRUE, showWarnings = FALSE)
  
  tournament_label <- case_when(
    tournament == "wimbledon" ~ "Wimbledon",
    tournament == "usopen"    ~ "US Open",
    TRUE                      ~ NA_character_
  )
  
  surface <- ifelse(tournament == "wimbledon", "Grass", "Hard")
  
  importance_file <- ifelse(
    tournament == "wimbledon",
    "data/results/importance/grass.csv.gz",
    "data/results/importance/hard.csv.gz"
  )
  
  file_base <- paste0("data/processed/combined/", year, "_", short_tournament, ".csv.gz")
  
  cat("\nProcessing:", tournament, year, "\n")
  
  importance <- fread(importance_file)
  
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
    filter(state %in% importance$state)
  
  data <- data %>%
    left_join(
      importance %>% select(state, importance),
      by = "state"
    )
  
  # Split male/female
  cutoff_match <- paste0(year, "-", tournament, "-1701")
  
  data_m <- data %>% filter(match_id <= cutoff_match)
  data_f <- data %>% filter(match_id > cutoff_match)
  
  cat("Number of men's rows before Welo:", nrow(data_m), "\n")
  cat("Number of women's rows before Welo:", nrow(data_f), "\n")
  
  # Add speed ratio
  data_m <- add_speed_ratio_column(data_m)
  data_f <- add_speed_ratio_column(data_f)
  
  # Select and rename columns
  select_columns <- function(df) {
    df %>%
      mutate(
        player1_name = map_chr(player1, format_player_name),
        player2_name = map_chr(player2, format_player_name),
        player1_name = as.character(player1_name),
        player2_name = as.character(player2_name)
      ) %>%
      select(
        match_id, year, ElapsedTime, SetNo, GameNo, PointNumber,
        player1, player2,
        ServeNumber, ServeIndicator, PointWinner,
        ServeWidth, ServeDepth,
        RallyCount, P1DistanceRun, P2DistanceRun,
        P1Score, P2Score, state,
        GameWinner, serving_player_won,
        speed_ratio, importance,
        P1BreakPoint, P2BreakPoint,
        P1GamesWon, P2GamesWon,
        P1Ace, P2Ace,
        P1DoubleFault, P2DoubleFault,
        Speed_MPH,
        player1_name, player2_name
      )
  }
  
  subset_m <- select_columns(data_m)
  subset_f <- select_columns(data_f)
  
  # Check that join keys are clean character columns
  cat("Men player1_name type:", class(subset_m$player1_name), "\n")
  cat("Women player1_name type:", class(subset_f$player1_name), "\n")
  
  # Add Welo ratings before match
  subset_m <- add_welo(subset_m, "male", year, tournament_label, surface)
  subset_f <- add_welo(subset_f, "female", year, tournament_label, surface)
  
  cat("Number of men's rows after Welo:", nrow(subset_m), "\n")
  cat("Number of women's rows after Welo:", nrow(subset_f), "\n")
  
  # Convert ElapsedTime to seconds
  subset_m <- convert_elapsed(subset_m)
  subset_f <- convert_elapsed(subset_f)
  
  # Write output
  men_outfile <- paste0("data/processed/subset/", year, "_", short_tournament, "_men.csv.gz")
  women_outfile <- paste0("data/processed/subset/", year, "_", short_tournament, "_women.csv.gz")
  
  .codex_write_csv_gz(subset_m, men_outfile, row.names = FALSE)
  .codex_write_csv_gz(subset_f, women_outfile, row.names = FALSE)
  
  cat("Wrote:", men_outfile, "\n")
  cat("Wrote:", women_outfile, "\n")
}

#--------------------------------------------------------------------------------
# Run Processing

for (t in tournaments) {
  for (year in years) {
    process_tournament_year(year, t)
  }
}
