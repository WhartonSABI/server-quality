rm(list=ls()) # clear environment

# install.packages(c("rio", "data.table", "ggplot2","dplyr","tidyverse"))
library(rio)
library(data.table)
library(tidyverse)

#-----------------------------------------------------------------------------------------------------

# Helper function to clean one year's data
process_wimbledon_year <- function(year, raw_path = "../data/raw_data", out_path = "out_data") {
  # Load data
  matches_file <- paste0(raw_path, "/", year, "-wimbledon-matches.csv")
  points_file <- paste0(raw_path, "/", year, "-wimbledon-points.csv")
  
  matches <- fread(matches_file) %>% select(where(~ all(!is.na(.))))
  points <- fread(points_file) %>% select(where(~ all(!is.na(.))))
  
  # Merge data
  combined <- left_join(points, matches, by = "match_id")
  
  # Remove rows with Speed_MPH == 0 when there's no double fault
  combined <- combined %>%
    filter(!(P1DoubleFault == 0 & P2DoubleFault == 0 & Speed_MPH == 0)) %>%
    mutate(serving_player_won = ifelse((ServeIndicator == 1 & PointWinner == 1) |
                                         (ServeIndicator == 2 & PointWinner == 2), 1, 0))
  
  # Save output
  output_file <- paste0(out_path, "/wimbledon_", year, "_combined.csv")
  fwrite(combined, output_file)
  
  # Optional: print number of removed invalid speed rows
  removed <- nrow(points %>% filter(P1DoubleFault == 0 & P2DoubleFault == 0 & Speed_MPH == 0))
  cat("Year", year, "- rows removed due to Speed_MPH == 0 without double fault:", removed, "\n")
}

# Years to process
years <- c(2018, 2019, 2021, 2022, 2023, 2024)

# Process all years
for (year in years) {
  process_wimbledon_year(year)
}


