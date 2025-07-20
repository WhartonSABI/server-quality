# rm(list=ls())

library(tidyverse)
library(data.table)

#-----------------------------------------------------------------------------------------------------

wimbledon_2024_subset_m <- as.data.table(read.csv("../data/wimbledon_subset_2024_m.csv"))
wimbledon_2024_subset_f <- as.data.table(read.csv("../data/wimbledon_subset_2024_f.csv"))

#-----------------------------------------------------------------------------------------------------
## for each match_id, and for each player (player1 and player2), calculate each player's 
# proportion of first serves (proportion of 1's in ServeNumber column when that player served, i.e., 
# when PointServer == 1 for player1 and PointServer == 2 for player2)

calculate_first_serve_proportion <- function(data) {
  data %>%
    group_by(match_id, PointServer) %>%
    summarise(
      first_serve_count = sum(ServeNumber == 1, na.rm = TRUE),
      total_serves = n(),
      first_serve_proportion = first_serve_count / total_serves,
      .groups = 'drop'
    ) %>%
    pivot_wider(names_from = PointServer, values_from = c(first_serve_count, total_serves, first_serve_proportion), 
                names_prefix = "player") %>%
    rename_with(~ gsub("player1_", "player1_", .), starts_with("player1")) %>%
    rename_with(~ gsub("player2_", "player2_", .), starts_with("player2"))
}

wimbledon_2024_subset_m_test <- calculate_first_serve_proportion(wimbledon_2024_subset_m)
names(wimbledon_2024_subset_m_test)

# merge the two datasets based on match_id, keep first_serve_count_player1