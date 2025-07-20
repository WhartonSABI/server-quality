rm(list=ls())
# install.packages("fastDummies")
library(welo)
library(tidyverse)
library(data.table)
library(ggplot2)
library(hms)
library(purrr)

#-----------------------------------------------------------------------------------------------------

subset_m <- as.data.table(read.csv("../data/output/wimbledon_subset_m.csv"))
subset_f <- as.data.table(read.csv("../data/output/wimbledon_subset_f.csv"))

colSums(is.na(subset_m))

# # model for probability of double fault:
# # filter to only where ServeNumber == 2, Speed_MPH not equal to 0 if (P1DoubleFault == 1 and P2DoubleFault == 1)
# subset_m_test <- subset_m %>%
#   filter(ServeNumber == 2, (Speed_MPH == 0 & (P1DoubleFault == 1 | P2DoubleFault == 1)) 
#          | (Speed_MPH != 0 & (P1DoubleFault == 0 & P2DoubleFault == 0))) %>%
#   mutate(DoubleFault = ifelse(P1DoubleFault == 1 | P2DoubleFault == 1, 1, 0))
#   
#-----------------------------------------------------------------------------------------------------

# 1) get every player who ever served
servers <- subset_m %>%
  filter(ServeNumber %in% c(1,2)) %>%
  mutate(ServerName = if_else(PointServer == 1, player1, player2)) %>%
  pull(ServerName) %>%
  unique()

# 2) function to compute df_pct for one server
compute_df_for <- function(name) {
  subset_m %>%
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
      df_pct_server        = double_faults / total_serves,
      .groups       = "drop"
    )
}

# 3) apply it to every server and bind the results
all_rates <- map_dfr(servers, compute_df_for)

print(all_rates)

## merge all_rates with subset_m based on match_id and ServerName
subset_m_test <- subset_m %>% 
  # identify who is serving this point
  mutate(ServerName = if_else(PointServer == 1, player1, player2)) %>% 
  
  # now the key columns exist in both tables
  left_join(all_rates, by = c("match_id", "ServerName"))

colSums(is.na(subset_m_test))
#-----------------------------------------------------------------------------------------------------

# filter to only include rows where ServeWidth is in B, BC, BW, C, or W. and ServeDepth is CTL or NCTL
subset_m_test <- subset_m_test %>%
  filter(ServeWidth %in% c("B", "BC", "BW", "C", "W"),
         ServeDepth %in% c("CTL", "NCTL"))

write.csv(subset_m_test, "../data/output/wimbledon_subset_m.csv", row.names = FALSE)

#-----------------------------------------------------------------------------------------------------

# 1) get every player who ever served
servers <- subset_f %>%
  filter(ServeNumber %in% c(1,2)) %>%
  mutate(ServerName = if_else(PointServer == 1, player1, player2)) %>%
  pull(ServerName) %>%
  unique()

# 2) function to compute df_pct for one server
compute_df_for <- function(name) {
  subset_f %>%
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
      df_pct_server        = double_faults / total_serves,
      .groups       = "drop"
    )
}

# 3) apply it to every server and bind the results
all_rates <- map_dfr(servers, compute_df_for)

print(all_rates)

## merge all_rates with subset_m based on match_id and ServerName
subset_f_test <- subset_f %>% 
  # identify who is serving this point
  mutate(ServerName = if_else(PointServer == 1, player1, player2)) %>% 
  
  # now the key columns exist in both tables
  left_join(all_rates, by = c("match_id", "ServerName"))

colSums(is.na(subset_f_test))
#-----------------------------------------------------------------------------------------------------

# filter to only include rows where ServeWidth is in B, BC, BW, C, or W. and ServeDepth is CTL or NCTL
subset_f_test <- subset_f_test %>%
  filter(ServeWidth %in% c("B", "BC", "BW", "C", "W"),
         ServeDepth %in% c("CTL", "NCTL"))

write.csv(subset_f_test, "../data/output/wimbledon_subset_f.csv", row.names = FALSE)
