rm(list=ls()) # clear environment

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
  
  # Load data
  # Drop only columns that are fully NA, not columns that merely contain some NAs
  matches <- fread(matches_file) %>% select(where(~ !all(is.na(.))))
  points  <- fread(points_file) %>% select(where(~ !all(is.na(.))))
  
  # If Speed_MPH does not exist, create it from Speed_KMH
  if (!"Speed_MPH" %in% names(points)) {
    if ("Speed_KMH" %in% names(points)) {
      points <- points %>%
        mutate(Speed_MPH = Speed_KMH * 0.621371)
      
      cat("Speed_MPH not found for", tournament, year, 
          "- created Speed_MPH from Speed_KMH.\n")
    } else {
      stop("Neither Speed_MPH nor Speed_KMH exists in the points file for ",
           tournament, " ", year, ".")
    }
  }
  
  # Required point-level columns
  required_point_cols <- c(
    "match_id",
    "Speed_MPH",
    "P1DoubleFault",
    "P2DoubleFault",
    "PointServer",
    "PointWinner",
    "ServeNumber"
  )
  
  missing_cols <- setdiff(required_point_cols, names(points))
  
  if (length(missing_cols) > 0) {
    stop(
      "Missing required columns in points file for ",
      tournament, " ", year, ": ",
      paste(missing_cols, collapse = ", ")
    )
  }
  
  # Remove rows where key point-level variables are NA
  rows_before_missing_filter <- nrow(points)
  
  points <- points %>%
    filter(
      !is.na(match_id),
      !is.na(Speed_MPH),
      !is.na(P1DoubleFault),
      !is.na(P2DoubleFault),
      !is.na(PointServer),
      !is.na(PointWinner),
      !is.na(ServeNumber)
    )
  
  removed_missing_key_values <- rows_before_missing_filter - nrow(points)
  
  # Merge data
  combined <- left_join(points, matches, by = "match_id")
  
  # Count rows with Speed_MPH == 0 when there is no double fault
  removed_zero_speed <- combined %>%
    filter(P1DoubleFault == 0 & P2DoubleFault == 0 & Speed_MPH == 0) %>%
    nrow()
  
  # Remove rows with Speed_MPH == 0 when there's no double fault
  combined <- combined %>%
    filter(!(P1DoubleFault == 0 & P2DoubleFault == 0 & Speed_MPH == 0)) %>%
    mutate(
      serving_player_won = ifelse(
        (ServeIndicator == 1 & PointWinner == 1) |
          (ServeIndicator == 2 & PointWinner == 2),
        1, 0
      )
    )
  
  # Write cleaned data
  output_file <- paste0(out_path, "/combined/", year, "_", short_tournament, ".csv")
  fwrite(combined, output_file)
  
  # Print summary
  cat(
    "Tournament:", tournament,
    "- Year:", year,
    "- Rows removed due to missing key point values:", removed_missing_key_values,
    "- Rows removed due to Speed_MPH == 0 without double fault:", removed_zero_speed,
    "- Final rows:", nrow(combined),
    "\n"
  )
}

#-----------------------------------------------------------------------------------------------------

# Define years and tournaments
years <- c(2014, 2015, 2016, 2017,
           2018, 2019, 2021, 2022, 2023, 2024)

tournaments <- c("wimbledon", "usopen")

# Process all years for each tournament
for (t in tournaments) {
  for (year in years) {
    process_tournament_year(year, tournament = t)
  }
}
