library(tidyverse)
library(data.table)
library(dplyr)
######################################################

w24 <- fread("wimbledon_subset_2024_m.csv")

# 1) get every player who ever served
servers <- w24 %>%
  filter(ServeNumber %in% c(1,2)) %>%
  mutate(ServerName = if_else(PointServer == 1, player1, player2)) %>%
  pull(ServerName) %>%
  unique()

# 2) function to compute df_pct for one server
compute_df_for <- function(name) {
  w24 %>%
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
      df_pct        = double_faults / total_serves,
      .groups       = "drop"
    )
}

# 3) apply it to every server and bind the results
all_rates <- map_dfr(servers, compute_df_for)

print(all_rates)
