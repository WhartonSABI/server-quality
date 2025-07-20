rm(list = ls())

library(welo)
library(tidyverse)
library(data.table)

#--------------------------------------------------------------------------------
combine_years <- function(tournament, years, gender, type = c("training", "testing")) {
  type <- match.arg(type)
  
  files <- paste0("../data/processed/", tournament, "_subset_", years, "_", gender, ".csv")
  data_list <- lapply(files, fread)
  combined <- rbindlist(data_list, use.names = TRUE, fill = TRUE)
  
  combined <- combined %>%
    filter(ServeDepth != "", ServeWidth != "", !is.na(speed_ratio))
  
  # Convert ELO to logistic (Bradley-Terry scale)
  combined[, welo_p1_bt := 0.0057565 * player1_avg_welo]
  combined[, welo_p2_bt := 0.0057565 * player2_avg_welo]
  
  combined <- combined %>%
    mutate(p_server_beats_returner = ifelse(ServeIndicator == 1,
                                            1 / (1 + exp(welo_p2_bt - welo_p1_bt)),
                                            1 / (1 + exp(welo_p1_bt - welo_p2_bt))))
  
  out_file <- paste0("../data/processed/", tournament, "_subset_", gender, "_", type, ".csv")
  fwrite(combined, out_file)
}

#--------------------------------------------------------------------------------
# Define years
train_years <- 2021:2024
test_years  <- 2018:2019

tournaments <- c("wimbledon", "usopen")
genders <- c("m", "f")

for (t in tournaments) {
  for (g in genders) {
    combine_years(t, train_years, g, type = "training")
    combine_years(t, test_years, g, type = "testing")
  }
}
