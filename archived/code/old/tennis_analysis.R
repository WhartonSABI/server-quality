rm(list=ls())
setwd("C:/UPenn/research/WSABI summer 2025/Research/brainstorming")

library(rio)
library(data.table)
library(ggplot2)
library(dplyr)
library(tidyverse)

#-----------------------------------------------------------------------------------------------------

# wimbledon 2024
wimbledon_2024 <- as.data.table(read.csv("wimbledon_2024_combined.csv"))
names(wimbledon_2024)

# binary variable: whether serving player won or lost
wimbledon_2024 <- wimbledon_2024 %>%
  filter(PointServer != 0, Speed_KMH != 0, Speed_MPH != 0) %>% 
  mutate(serving_player_won = ifelse((ServeNumber == 1 & PointWinner == 1) | (ServeNumber == 2 & PointWinner == 2), 1, 0))

# divide male & female
wimbledon_2024_male <- wimbledon_2024[1:14489,]
wimbledon_2024_female <- wimbledon_2024[14490:nrow(wimbledon_2024),]

#-----------------------------------------------------------------------------------------------------

## EDA
ggplot(wimbledon_2024, aes(x = Speed_MPH)) +
  geom_histogram(binwidth = 2, fill = "steelblue", color = "white") +
  facet_wrap(~ ServeNumber) +
  labs(title = "Distribution of Serve Speeds (M + F combined)",
       x = "Speed (MPH)",
       y = "Count") +
  theme_minimal()

first_serves_male <- wimbledon_2024_male %>% 
  filter(ServeNumber == 1)

ggplot(first_serves_male, aes(x = Speed_MPH, y = serving_player_won)) +
  geom_point() +
  labs(title = "Serve Speed vs. Win Outcome for First Serves") +
  theme_minimal()

second_serves_male <- wimbledon_2024_male %>% 
  filter(ServeNumber == 2)

ggplot(second_serves_male, aes(x = factor(serving_player_won), y = Speed_MPH)) +
  geom_boxplot() +
  labs(title = "Serve Speed vs. Win Outcome for Second Serves",
       x = "Did Server Win the Point?",
       y = "Serve Speed (MPH)") +
  theme_minimal()

#-----------------------------------------------------------------------------------------------------

#####################
### define a function to add a new column for “speed_ratio = Speed_MPH / player_avg_first_speed”, 
### then model “server_won_point ~ speed_ratio + ServeWidth + ServeDepth + …” 
### using only rows with second serves.
#####################

add_speed_ratio_column <- function(data) {
  # calculate average first serve speed for each player, for each match.
  avg_first_speed <- data %>%
    filter(ServeNumber == 1) %>%
    group_by(match_id) %>%
    summarise(
      avg_player1_first_speed = mean(Speed_MPH[PointServer == 1], na.rm = TRUE),
      avg_player2_first_speed = mean(Speed_MPH[PointServer == 2], na.rm = TRUE),
      .groups = 'drop'
    )
  # Join the average first speed back to the original data
  data <- data %>%
    left_join(avg_first_speed, by = "match_id") %>%
    mutate(speed_ratio = Speed_MPH / ifelse(PointServer == 1, avg_player1_first_speed, avg_player2_first_speed))
  
  return(data)
}

wimbledon_2024_male <- add_speed_ratio_column(wimbledon_2024_male)
wimbledon_2024_female <- add_speed_ratio_column(wimbledon_2024_female)

subset <- wimbledon_2024_male %>% # to make it easier for us to look through relevant columns
  select(match_id, slam, year, player1, player2, Speed_MPH, ServeNumber, PointServer, PointWinner, ServeWidth, ServeDepth, RallyCount, GameNo, P1DistanceRun,
         P2DistanceRun, RallyCount, serving_player_won, speed_ratio)

# speed_ratio would be x variable?

#-----------------------------------------------------------------------------------------------------

# function to transform data for analysis
transform_data_for_analysis <- function(data) {
  # Filter for second serves and select relevant columns
  data_to_analyze <- data %>%
    filter(ServeNumber == 2) %>%
    select(serving_player_won, speed_ratio, ServeWidth, ServeDepth, RallyCount) %>%
    mutate(ServeWidth = as.factor(ServeWidth), ServeDepth = as.factor(ServeDepth))
  
  # Create binary columns for each value in ServeWidth and ServeDepth
  data_to_analyze <- data_to_analyze %>%
    mutate(
      ServeWidth_B  = if_else(ServeWidth == "B", 1, 0),
      ServeWidth_BC = if_else(ServeWidth == "BC", 1, 0),
      ServeWidth_BW = if_else(ServeWidth == "BW", 1, 0),
      ServeWidth_C  = if_else(ServeWidth == "C", 1, 0),
      ServeWidth_W  = if_else(ServeWidth == "W", 1, 0),
      ServeDepth_CTL  = if_else(ServeDepth == "CTL", 1, 0),
      ServeDepth_NCTL = if_else(ServeDepth == "NCTL", 1, 0)
    )
  
  return(data_to_analyze)
}

#-----------------------------------------------------------------------------------------------------
# wimbledon 2024 female and male

data_to_analyze <- transform_data_for_analysis(subset)

# logistic regressions to predict serving_player_won probability
model1 <- glm(serving_player_won ~ speed_ratio, 
                      data = data_to_analyze, family = binomial)
summary(model1)

# leave out ServeDepth_NCTL since this is perfectly correlated with ServeDepth_CTL
model2 <- glm(serving_player_won ~ speed_ratio + ServeWidth_B + ServeWidth_BC + ServeWidth_BW + ServeWidth_C + ServeWidth_W
              + ServeDepth_CTL, data = data_to_analyze, family = binomial)
summary(model2)

model3 <- glm(serving_player_won ~ speed_ratio + ServeWidth_B + ServeWidth_BC + ServeWidth_BW + ServeWidth_C + ServeWidth_W
              + ServeDepth_CTL + RallyCount, data = data_to_analyze, family = binomial)
summary(model3)

## female
subset_f <- wimbledon_2024_female %>% 
  select(match_id, slam, year, player1, player2, Speed_MPH, ServeNumber, PointServer, PointWinner, ServeWidth, ServeDepth, RallyCount, GameNo, P1DistanceRun,
         P2DistanceRun, RallyCount, serving_player_won, speed_ratio)

data_to_analyze_f <- transform_data_for_analysis(subset_f)

# logistic regressions to predict serving_player_won probability
model1_f <- glm(serving_player_won ~ speed_ratio, 
                      data = data_to_analyze_f, family = binomial)
summary(model1_f)

# leave out ServeDepth_NCTL since this is perfectly correlated with ServeDepth_CTL
model2_f <- glm(serving_player_won ~ speed_ratio + ServeWidth_B + ServeWidth_BC + ServeWidth_BW + ServeWidth_C + ServeWidth_W
              + ServeDepth_CTL, data = data_to_analyze_f, family = binomial)
summary(model2_f)

model3_f <- glm(serving_player_won ~ speed_ratio + ServeWidth_B + ServeWidth_BC + ServeWidth_BW + ServeWidth_C + ServeWidth_W
              + ServeDepth_CTL + RallyCount, data = data_to_analyze_f, family = binomial)
summary(model3_f)

#-----------------------------------------------------------------------------------------------------

## same thing but for wimbledon 2022 data
wimbledon_2022 <- as.data.table(read.csv("wimbledon_2022_combined.csv"))

# binary variable: whether serving player won or lost
wimbledon_2022 <- wimbledon_2022 %>%
  filter(PointServer != 0, Speed_KMH != 0, Speed_MPH != 0) %>% 
  mutate(serving_player_won = ifelse((ServeNumber == 1 & PointWinner == 1) | (ServeNumber == 2 & PointWinner == 2), 1, 0))

# divide male & female
wimbledon_2022_male <- wimbledon_2022[1:14897,]
wimbledon_2022_female <- wimbledon_2022[14897:nrow(wimbledon_2022),]

#-----------------------------------------------------------------------------------------------------

wimbledon_2022_male <- add_speed_ratio_column(wimbledon_2022_male)
wimbledon_2022_female <- add_speed_ratio_column(wimbledon_2022_female)

subset <- wimbledon_2022_male %>% # to make it easier for us to look through relevant columns
  select(match_id, slam, year, player1, player2, Speed_MPH, ServeNumber, PointServer, PointWinner, ServeWidth, ServeDepth, RallyCount, GameNo, P1DistanceRun,
         P2DistanceRun, RallyCount, serving_player_won, speed_ratio)

data_to_analyze <- transform_data_for_analysis(subset)

# logistic regressions to predict serving_player_won probability
model1 <- glm(serving_player_won ~ speed_ratio, 
                      data = data_to_analyze, family = binomial)
summary(model1)

# leave out ServeDepth_NCTL since this is perfectly correlated with ServeDepth_CTL
model2 <- glm(serving_player_won ~ speed_ratio + ServeWidth_B + ServeWidth_BC + ServeWidth_BW + ServeWidth_C + ServeWidth_W
              + ServeDepth_CTL, data = data_to_analyze, family = binomial)
summary(model2)

model3 <- glm(serving_player_won ~ speed_ratio + ServeWidth_B + ServeWidth_BC + ServeWidth_BW + ServeWidth_C + ServeWidth_W
              + ServeDepth_CTL + RallyCount, data = data_to_analyze, family = binomial)
summary(model3)

# why is this significant?
second_serves_male <- wimbledon_2022_male %>% 
  filter(ServeNumber == 2)

# not much visual difference
ggplot(second_serves_male, aes(x = factor(serving_player_won), y = Speed_MPH)) +
  geom_boxplot() +
  labs(title = "Serve Speed vs. Win Outcome for Second Serves",
       x = "Did Server Win the Point?",
       y = "Serve Speed (MPH)") +
  theme_minimal()

## female

subset_f <- wimbledon_2022_female %>% # to make it easier for us to look through relevant columns
  select(match_id, slam, year, player1, player2, Speed_MPH, ServeNumber, PointServer, PointWinner, ServeWidth, ServeDepth, RallyCount, GameNo, P1DistanceRun,
         P2DistanceRun, RallyCount, serving_player_won, speed_ratio)
data_to_analyze_f <- transform_data_for_analysis(subset_f)

# logistic regressions to predict serving_player_won probability
model1_f <- glm(serving_player_won ~ speed_ratio, 
                      data = data_to_analyze_f, family = binomial)
summary(model1_f)

# leave out ServeDepth_NCTL since this is perfectly correlated with ServeDepth_CTL
model2_f <- glm(serving_player_won ~ speed_ratio + ServeWidth_B + ServeWidth_BC + ServeWidth_BW + ServeWidth_C + ServeWidth_W
              + ServeDepth_CTL, data = data_to_analyze_f, family = binomial)
summary(model2_f)

model3_f <- glm(serving_player_won ~ speed_ratio + ServeWidth_B + ServeWidth_BC + ServeWidth_BW + ServeWidth_C + ServeWidth_W
              + ServeDepth_CTL + RallyCount, data = data_to_analyze_f, family = binomial)
summary(model3_f)