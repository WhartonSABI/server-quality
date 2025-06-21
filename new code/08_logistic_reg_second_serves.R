rm(list=ls())
# install.packages("welo")
library(welo)
library(tidyverse)
library(data.table)
library(ggplot2)
# install.packages("car")
library(car)

#-----------------------------------------------------------------------------------------------------

subset_m <- as.data.table(read.csv("../data/wimbledon_subset_m.csv"))
subset_f <- as.data.table(read.csv("../data/wimbledon_subset_f.csv"))

names(subset_m)

#-----------------------------------------------------------------------------------------------------

# # Convert ELO to logistic (Bradley-Terry scale)
# subset_m[, welo_p1_bt := 0.0057565 * player1_avg_welo]
# subset_m[, welo_p2_bt := 0.0057565 * player2_avg_welo]
# 
# ## male
# subset_m <- subset_m %>%
#   mutate(p_server_beats_returner <- ifelse(PointServer == 1,
#                                            1 / (1 + exp(welo_p2_bt - welo_p1_bt)),
#                                            1 / (1 + exp(welo_p1_bt - welo_p2_bt))))
# 
# setnames(subset_m, old = c("... <- NULL"),
#          new = c("p_server_beats_returner"))
# 
# write.csv(subset_m, "../data/wimbledon_subset_m.csv", row.names = FALSE)

#-----------------------------------------------------------------------------------------------------

# # Convert ELO to logistic (Bradley-Terry scale)
# subset_f[, welo_p1_bt := 0.0057565 * player1_avg_welo]
# subset_f[, welo_p2_bt := 0.0057565 * player2_avg_welo]
# 
# ## female
# subset_f <- subset_f %>%
#   mutate(p_server_beats_returner <- ifelse(PointServer == 1,
#                                            1 / (1 + exp(welo_p2_bt - welo_p1_bt)),
#                                            1 / (1 + exp(welo_p1_bt - welo_p2_bt))))
# 
# # rename column in subset_f
# setnames(subset_f, old = c("... <- NULL"),
#          new = c("p_server_beats_returner"))
# 
# write.csv(subset_f, "../data/wimbledon_subset_f.csv", row.names = FALSE)

# -----------------------------------------------------------------------------------------------------

#-----------------------------------------------------------------------------------------------------

# new models with just second serves
subset_m_second <- subset_m[ServeNumber == 2]
subset_f_second <- subset_f[ServeNumber == 2]

## logistic regression for serving_player_won vs. p_server_beats_returner
logit_model_m <- glm(serving_player_won ~ p_server_beats_returner, 
                     data = subset_m_second, family = "binomial")
summary(logit_model_m) ## p_server_beats_returner significant *** (neg coef)

logit_model_f <- glm(serving_player_won ~ p_server_beats_returner, 
                     data = subset_f_second, family = "binomial")
summary(logit_model_f) ## not significant

#-----------------------------------------------------------------------------------------------------

## add covariates

logit_model_2_m <- glm(serving_player_won ~ p_server_beats_returner + ElapsedSeconds_fixed, 
                       data = subset_m_second, family = "binomial")
summary(logit_model_2_m) ## p_server_beats_returner significant *** (neg coef), ElapsedSeconds significant ** (neg coef)

logit_model_2_f <- glm(serving_player_won ~ p_server_beats_returner + ElapsedSeconds_fixed, 
                       data = subset_f_second, family = "binomial")
summary(logit_model_2_f) ## neither significant

#-----------------------------------------------------------------------------------------------------

## add second serve speed ratio
logit_model_3_m <- glm(serving_player_won ~ p_server_beats_returner + ElapsedSeconds_fixed + speed_ratio, 
                       data = subset_m_second, family = "binomial")
summary(logit_model_3_m) ## p_server_beats_returner significant *** (neg coef), ElapsedSeconds significant ** (neg coef), speed_ratio not significant

logit_model_3_f <- glm(serving_player_won ~ p_server_beats_returner + ElapsedSeconds_fixed + speed_ratio, 
                       data = subset_f_second, family = "binomial")
summary(logit_model_3_f) ## nothing significant

#-----------------------------------------------------------------------------------------------------

## add score importance
logit_model_4_m <- glm(serving_player_won ~ p_server_beats_returner + ElapsedSeconds_fixed + speed_ratio + importance, 
                       data = subset_m_second, family = "binomial")
summary(logit_model_4_m) ## p_server_beats_returner significant *** (neg coef), ElapsedSeconds significant ** (neg coef), speed_ratio and importance not significant

logit_model_4_f <- glm(serving_player_won ~ p_server_beats_returner + ElapsedSeconds_fixed + speed_ratio + importance,  
                       data = subset_f_second, family = "binomial")
summary(logit_model_4_f) ## nothing significant

#-----------------------------------------------------------------------------------------------------

## add location of serve
logit_model_5_m <- glm(serving_player_won ~ p_server_beats_returner + ElapsedSeconds_fixed + importance + speed_ratio + factor(ServeWidth) + factor(ServeDepth), 
                       data = subset_m_second, family = "binomial")
summary(logit_model_5_m) ## p_server_beats_returner significant *** (neg coef), ElapsedSeconds significant ** (neg coef), speed_ratio & importance & serve location not significant
logit_model_5_f <- glm(serving_player_won ~ p_server_beats_returner + ElapsedSeconds_fixed + importance + speed_ratio + factor(ServeWidth) + factor(ServeDepth), 
                       data = subset_f_second, family = "binomial")
summary(logit_model_5_f) ## nothing significant

vif(logit_model_5_m)
vif(logit_model_5_f)

#-----------------------------------------------------------------------------------------------------

## use serve speed instead of serve ratio

logit_model_6_m <- glm(serving_player_won ~ p_server_beats_returner + ElapsedSeconds_fixed + importance + Speed_MPH + factor(ServeWidth) + factor(ServeDepth), 
                       data = subset_m_second, family = "binomial")
summary(logit_model_6_m) ## p_server_beats_returner significant *** (neg coef), ElapsedSeconds significant ** (neg coef)
logit_model_6_f <- glm(serving_player_won ~ p_server_beats_returner + ElapsedSeconds_fixed + importance + Speed_MPH + factor(ServeWidth) + factor(ServeDepth), 
                       data = subset_f_second, family = "binomial")
summary(logit_model_6_f) ## nothing significant

logit_model_7_m <- glm(serving_player_won ~ p_server_beats_returner + ElapsedSeconds_fixed + importance + Speed_MPH + I(Speed_MPH**2) + factor(ServeWidth) + factor(ServeDepth), 
                       data = subset_m_second, family = "binomial")
summary(logit_model_7_m) ## p_server_beats_returner significant *** (neg coef), ElapsedSeconds significant ** (neg coef)
logit_model_7_f <- glm(serving_player_won ~ p_server_beats_returner + ElapsedSeconds_fixed + importance + Speed_MPH + I(Speed_MPH**2) + factor(ServeWidth) + factor(ServeDepth), 
                       data = subset_f_second, family = "binomial")
summary(logit_model_7_f) ## nothing significant

logit_model_8_m <- glm(serving_player_won ~ p_server_beats_returner + ElapsedSeconds_fixed + importance + splines::bs(Speed_MPH, degree = 3, df = 5) + factor(ServeWidth) + factor(ServeDepth), 
                       data = subset_m_second, family = "binomial")
summary(logit_model_8_m) ## p_server_beats_returner significant *** (neg coef), ElapsedSeconds significant ** (neg coef)
logit_model_8_f <- glm(serving_player_won ~ p_server_beats_returner + ElapsedSeconds_fixed + importance + splines::bs(Speed_MPH, degree = 3, df = 5) + factor(ServeWidth) + factor(ServeDepth), 
                       data = subset_f_second, family = "binomial")
summary(logit_model_8_f) ## nothing significant

#-----------------------------------------------------------------------------------------------------

## graph proportion of points won vs. speed_mph
# Step 1: Bin Speed_MPH into intervals (e.g., 5 mph bins)
binned_data <- subset_f_second %>%
  filter(!is.na(Speed_MPH)) %>%
  mutate(speed_bin = cut(Speed_MPH, breaks = seq(floor(min(Speed_MPH)),
                                                 ceiling(max(Speed_MPH)),
                                                 by = 5))) %>%
  group_by(speed_bin) %>%
  summarise(
    avg_speed = mean(Speed_MPH, na.rm = TRUE),
    win_rate = mean(serving_player_won == 1, na.rm = TRUE),
    n = n()
  )

# Step 2: Plot
ggplot(binned_data, aes(x = avg_speed, y = win_rate)) +
  geom_point(size = 2) +
  geom_line() +
  labs(
    title = "Empirical Win Rate vs. Second Serve Speed (Females)",
    x = "Serve Speed (MPH)",
    y = "Proportion of Points Won"
  ) +
  theme_minimal()
ggsave("../images/female_second_serve_vs_win.png", bg = "white", 
       width = 8, height = 6, units = "in")

## males
# Step 1: Bin Speed_MPH into intervals (e.g., 5 mph bins)
binned_data <- subset_m_second %>%
  filter(!is.na(Speed_MPH)) %>%
  mutate(speed_bin = cut(Speed_MPH, breaks = seq(floor(min(Speed_MPH)),
                                                 ceiling(max(Speed_MPH)),
                                                 by = 5))) %>%
  group_by(speed_bin) %>%
  summarise(
    avg_speed = mean(Speed_MPH, na.rm = TRUE),
    win_rate = mean(serving_player_won == 1, na.rm = TRUE),
    n = n()
  )

# Step 2: Plot
ggplot(binned_data, aes(x = avg_speed, y = win_rate)) +
  geom_point(size = 2) +
  geom_line() +
  labs(
    title = "Empirical Win Rate vs. Second Serve Speed (Males)",
    x = "Serve Speed (MPH)",
    y = "Proportion of Points Won"
  ) +
  theme_minimal()
ggsave("../images/male_second_serve_vs_win.png", bg = "white", 
       width = 8, height = 6, units = "in")

#-----------------------------------------------------------------------------------------------------


# player_names <- unique(c(subset_m$player1_name, subset_m$player2_name))
# 
# # Create a design matrix with columns: one column per player
# X <- matrix(0, nrow = nrow(subset_m), ncol = length(player_names))
# colnames(X) <- c(paste0("B_", player_names))
# 
# ## loop through each row in subset_m (which corresponds with each row in X)
# # put 1 in the column corresponding to player1 and -1 in the column corresponding to player2
# for (i in 1:nrow(subset_m)) {
#   row <- subset_m[i, ]
#   player1 <- row$player1_name
#   player2 <- row$player2_name
#   winner <- row$PointWinner
#   
#   # Set +1 for player1, -1 for player2
#   X[i, paste0("B_", player1)] <- 1
#   X[i, paste0("B_", player2)] <- -1
#   
#   # Store outcome (1 if player1 won, 0 if player2 won)
#   subset_m$winner[i] <- ifelse(winner == player1, 1, 0)
# }
