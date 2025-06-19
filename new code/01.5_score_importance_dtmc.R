rm(list=ls())
# install.packages("welo")
library(welo)
library(tidyverse)
library(data.table)

#-----------------------------------------------------------------------------------------------------

wimbledon_2011 <- as.data.table(read.csv("../data/wimbledon_2011_combined.csv"))
wimbledon_2012 <- as.data.table(read.csv("../data/wimbledon_2012_combined.csv"))
wimbledon_2013 <- as.data.table(read.csv("../data/wimbledon_2013_combined.csv"))
wimbledon_2014 <- as.data.table(read.csv("../data/wimbledon_2014_combined.csv"))
wimbledon_2015 <- as.data.table(read.csv("../data/wimbledon_2015_combined.csv"))
wimbledon_2016 <- as.data.table(read.csv("../data/wimbledon_2016_combined.csv"))
wimbledon_2017 <- as.data.table(read.csv("../data/wimbledon_2017_combined.csv"))
wimbledon_2018 <- as.data.table(read.csv("../data/wimbledon_2018_combined.csv"))
wimbledon_2019 <- as.data.table(read.csv("../data/wimbledon_2019_combined.csv"))

wimbledon_2021 <- as.data.table(read.csv("../data/wimbledon_2021_combined.csv"))
wimbledon_2022 <- as.data.table(read.csv("../data/wimbledon_2022_combined.csv"))
wimbledon_2023 <- as.data.table(read.csv("../data/wimbledon_2023_combined.csv"))
wimbledon_2024 <- as.data.table(read.csv("../data/wimbledon_2024_combined.csv"))

names(wimbledon_2015)

#----------------------------------------------------------------------------------------------------------

selected_cols <- c("P1Score", "P2Score", "PointServer", "PointWinner", "GameWinner")

wimbledon_combined <- rbindlist(lapply(
  list(wimbledon_2011, wimbledon_2012, wimbledon_2013, wimbledon_2014, wimbledon_2015, wimbledon_2016,
       wimbledon_2017, wimbledon_2018, wimbledon_2019, wimbledon_2021, wimbledon_2022, wimbledon_2023, wimbledon_2024),
  function(dt) dt[, ..selected_cols]
)) %>% 
  mutate(
    P1Score = as.character(P1Score),
    P2Score = as.character(P2Score),
    PointServer = as.integer(PointServer),
    PointWinner = as.integer(PointWinner),
    GameWinner = as.integer(GameWinner),
    # server_score = if_else(PointServer == 1, P1Score, P2Score),
    # returner_score = if_else(PointServer == 1, P2Score, P1Score),
    state = paste(P1Score, P2Score, sep = "-")
  ) %>%
  filter(!is.na(P1Score), !is.na(P2Score), !is.na(PointServer), !is.na(PointWinner), !is.na(GameWinner))

# remove last row in wimbledon_combined
wimbledon_combined <- wimbledon_combined[-nrow(wimbledon_combined), ]

#-----------------------------------------------------------------------------------------------------

## dtmc for player scores (as in Sim and Choi, 2019)

# Prepare and clean the data
df <- wimbledon_combined %>%
  mutate(
    server_won_point = if_else((PointServer == 1 & PointWinner == 1) |
                                 (PointServer == 2 & PointWinner == 2), TRUE, FALSE),
    game_winner_is_server = if_else((PointServer == 1 & GameWinner == 1) |
                                      (PointServer == 2 & GameWinner == 2), 1, 0)
  ) %>% 
  filter(state %in% c("0-0", "15-0", "30-0", "40-0",
                      "0-15", "0-30", "0-40",
                      "15-15", "30-15", "40-15",
                      "15-30", "30-30", "40-30",
                      "15-40", "30-40", "40-40",
                      "40-AD", "AD-40")) 

#-----------------------------------------------------------------------------------------------------

## fix the game_winner_is_server column 

# Step 1: Add a unique game ID (based on runs of "0-0" score)
df <- df %>%
  mutate(new_game = (state == "0-0")) %>%
  mutate(game_id = cumsum(new_game))

# Step 2: Create a lookup table with the GameWinner for each game_id
game_winners <- df %>%
  filter(state == "0-0") %>%
  select(game_id, GameWinner)

# Shift GameWinner up by one row (so it applies to the previous game)
game_winners <- game_winners %>%
  mutate(GameWinner = lead(GameWinner))

# player 1 won the last game (from wimbledon_combined before removoing last row)
game_winners[nrow(game_winners), "GameWinner"] <- 1

# Step 3: Join back to assign GameWinner to all points in the same game
df <- df %>%
  select(-GameWinner) %>%  # remove old GameWinner (0s)
  left_join(game_winners, by = "game_id")  # assign correct GameWinner

df <- df %>%
  mutate(
    game_winner_is_server = if_else((PointServer == 1 & GameWinner == 1) |
                                      (PointServer == 2 & GameWinner == 2), 1, 0)
  )

df <- df %>%
  filter(PointServer %in% c(1, 2))

#-----------------------------------------------------------------------------------------------------

# ## filter to just rows where state is 0-15
# df_0_15 <- df %>%
#   filter(state == "0-15")

#-----------------------------------------------------------------------------------------------------

# Get all transitions where the current score is known
# and filter where the server had a chance to win
df_scores <- df %>%
  filter(!is.na(state)) %>%
  group_by(state, server_won_point) %>%
  summarise(
    n = n(),
    server_win_game = sum(game_winner_is_server, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(prob_win_game = server_win_game / n)

# Pivot wider so we can calculate importance
importance_df <- df_scores %>%
  select(state, server_won_point, prob_win_game) %>%
  pivot_wider(names_from = server_won_point, values_from = prob_win_game,
              names_prefix = "Pwin_") %>%
  mutate(importance = abs(Pwin_TRUE - Pwin_FALSE)) %>%
  arrange(desc(importance))

write.csv(importance_df, "../data/score_importance_dtmc.csv", row.names = FALSE)


#-----------------------------------------------------------------------------------------------------

# importance_df <- as.data.table(read.csv("../data/score_importance_dtmc.csv"))
# 
# subset_m <- as.data.table(read.csv("../data/wimbledon_subset_m.csv"))
# subset_f <- as.data.table(read.csv("../data/wimbledon_subset_f.csv"))
# 
# # get rid of importance column from subset_m
# subset_m[, importance := NULL]
# subset_f[, importance := NULL]
# 
# subset_m <- left_join(subset_m, importance_df %>% select(state, importance), by = "state")
# subset_f <- left_join(subset_f, importance_df %>% select(state, importance), by = "state")
# 
# colSums(is.na(subset_m))
# colSums(is.na(subset_f))
# 
# write.csv(subset_m, "../data/wimbledon_subset_m.csv", row.names = FALSE)
# write.csv(subset_f, "../data/wimbledon_subset_f.csv", row.names = FALSE)
