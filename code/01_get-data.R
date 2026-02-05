rm(list=ls()) # clear environment

# install.packages(c("rio", "data.table", "ggplot2","dplyr","tidyverse"))
library(rio)
library(data.table)
library(tidyverse)

#-----------------------------------------------------------------------------------------------------

# Generalized helper function to clean one year's data for a given tournament
process_tournament_year <- function(year, tournament = "wimbledon", raw_path = "data/raw", out_path = "data/processed") {
  short_tournament <- ifelse(tournament == "usopen", "us", "wimb")
  dir.create(file.path(out_path, "combined"), recursive = TRUE, showWarnings = FALSE)

  # Compose file paths
  matches_file <- paste0(raw_path, "/", year, "-", tournament, "-matches.csv")
  points_file  <- paste0(raw_path, "/", year, "-", tournament, "-points.csv")
  
  # Load and clean matches/points data
  matches <- fread(matches_file) %>% select(where(~ all(!is.na(.))))
  points  <- fread(points_file) %>% select(where(~ all(!is.na(.))))
  
  # Merge data
  combined <- left_join(points, matches, by = "match_id")
  
  # Remove rows with Speed_MPH == 0 when there's no double fault
  combined <- combined %>%
    filter(!(P1DoubleFault == 0 & P2DoubleFault == 0 & Speed_MPH == 0)) %>%
    mutate(serving_player_won = ifelse((ServeIndicator == 1 & PointWinner == 1) |
                                         (ServeIndicator == 2 & PointWinner == 2), 1, 0))
  
  # Write cleaned data
  output_file <- paste0(out_path, "/combined/", year, "_", short_tournament, ".csv")
  fwrite(combined, output_file)
  
  # Print summary
  removed <- nrow(points %>% filter(P1DoubleFault == 0 & P2DoubleFault == 0 & Speed_MPH == 0))
  cat("Tournament:", tournament, "- Year:", year, "- Rows removed due to Speed_MPH == 0 without double fault:", removed, "\n")
}

#-----------------------------------------------------------------------------------------------------

# Define years and tournaments
years <- c(2018, 2019, 2021, 2022, 2023, 2024)
tournaments <- c("wimbledon", "usopen")

# Process all years for each tournament
for (t in tournaments) {
  for (year in years) {
    process_tournament_year(year, tournament = t)
  }
}
