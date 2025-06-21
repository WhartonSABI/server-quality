rm(list=ls())
# install.packages("welo")
library(welo)
library(tidyverse)
library(data.table)
library(ggplot2)
library(car)
library(splines)

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
# # rename column in subset_m
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
#                                            1 / (1 + exp(welo_p1_bt - welo_p2_bt)),
#                                            1 / (1 + exp(welo_p2_bt - welo_p1_bt))))
# 
# # rename column in subset_f
# setnames(subset_f, old = c("... <- NULL"),
#          new = c("p_server_beats_returner"))
# 
# write.csv(subset_f, "../data/wimbledon_subset_f.csv", row.names = FALSE)

# -----------------------------------------------------------------------------------------------------

#-----------------------------------------------------------------------------------------------------

# new models with just first serves
subset_m_first <- subset_m[ServeNumber == 1]
subset_f_first <- subset_f[ServeNumber == 1]

## logistic regression for serving_player_won vs. p_server_beats_returner
logit_model_m <- glm(serving_player_won ~ p_server_beats_returner, 
                     data = subset_f_first, family = "binomial")
summary(logit_model_m) ## p_server_beats_returner *** (pos coef)

logit_model_f <- glm(serving_player_won ~ p_server_beats_returner, 
                     data = subset_f_first, family = "binomial")
summary(logit_model_f) ## p_server_beats_returner *** (pos coef)

#-----------------------------------------------------------------------------------------------------

## add covariates

logit_model_2_m <- glm(serving_player_won ~ p_server_beats_returner + ElapsedSeconds_fixed, 
                       data = subset_m_first, family = "binomial")
summary(logit_model_2_m) ## p_server_beats_returner *** (pos coef), ElapsedSeconds_fixed * (neg coef)

logit_model_2_f <- glm(serving_player_won ~ p_server_beats_returner + ElapsedSeconds_fixed, 
                       data = subset_f_first, family = "binomial")
summary(logit_model_2_f) ## p_server_beats_returner *** (pos coef)

#-----------------------------------------------------------------------------------------------------

## add second serve speed ratio
logit_model_3_m <- glm(serving_player_won ~ p_server_beats_returner + ElapsedSeconds_fixed + speed_ratio, 
                       data = subset_m_first, family = "binomial")
summary(logit_model_3_m) ## p_server_beats_returner *** (pos coef), ElapsedSeconds_fixed * (neg coef), speed_ratio *** (pos coef)

logit_model_3_f <- glm(serving_player_won ~ p_server_beats_returner + ElapsedSeconds_fixed + speed_ratio, 
                       data = subset_f_first, family = "binomial")
summary(logit_model_3_f) ## p_server_beats_returner *** (pos coef), speed_ratio *** (pos coef)

#-----------------------------------------------------------------------------------------------------

## add score importance
logit_model_4_m <- glm(serving_player_won ~ p_server_beats_returner + ElapsedSeconds_fixed + speed_ratio + importance, 
                       data = subset_m_first, family = "binomial")
summary(logit_model_4_m) ## p_server_beats_returner *** (pos coef), ElapsedSeconds_fixed * (neg coef), speed_ratio *** (pos coef), importance *** (pos coef)

logit_model_4_f <- glm(serving_player_won ~ p_server_beats_returner + ElapsedSeconds_fixed + speed_ratio + importance,  
                       data = subset_f_first, family = "binomial")
summary(logit_model_4_f) ## p_server_beats_returner *** (pos coef), speed_ratio *** (pos coef), importance *** (pos coef)

#-----------------------------------------------------------------------------------------------------

## add location of serve
logit_model_5_m <- glm(serving_player_won ~ p_server_beats_returner + ElapsedSeconds_fixed + importance + speed_ratio + factor(ServeWidth) + factor(ServeDepth), 
                       data = subset_m_first, family = "binomial")
summary(logit_model_5_m) ## p_server_beats_returner *** (pos coef), ElapsedSeconds_fixed * (neg coef), speed_ratio *** (pos coef), importance *** (pos coef), servewidth C and W *** (pos coef), servedepth NCTL *** (neg coef)
logit_model_5_f <- glm(serving_player_won ~ p_server_beats_returner + ElapsedSeconds_fixed + importance + speed_ratio + factor(ServeWidth) + factor(ServeDepth), 
                       data = subset_f_first, family = "binomial")
summary(logit_model_5_f) ## p_server_beats_returner *** (pos coef), speed_ratio *** (pos coef), importance *** (pos coef), servewidth BW, C, and W *, ***, *** (pos coefs), servedepth NCTL *** (neg coef)

vif(logit_model_5_m) # acceptable (all less than 1.6ish)
vif(logit_model_5_f)

#-----------------------------------------------------------------------------------------------------

## use serve speed instead of serve ratio

logit_model_6_m <- glm(serving_player_won ~ p_server_beats_returner + ElapsedSeconds_fixed + importance + Speed_MPH + factor(ServeWidth) + factor(ServeDepth), 
                       data = subset_m_first, family = "binomial")
summary(logit_model_6_m) ## p_server_beats_returner *** (pos coef), ElapsedSeconds_fixed * (neg coef), speed_mph *** (pos coef), importance *** (pos coef), servewidth C and W *** (pos coef), servedepth NCTL *** (neg coef)
logit_model_6_f <- glm(serving_player_won ~ p_server_beats_returner + ElapsedSeconds_fixed + importance + Speed_MPH + factor(ServeWidth) + factor(ServeDepth), 
                       data = subset_f_first, family = "binomial")
summary(logit_model_6_f) ## p_server_beats_returner *** (pos coef), speed_mph *** (pos coef), importance *** (pos coef), servewidth C and W *** (pos coefs), servedepth NCTL *** (neg coef)

logit_model_7_m <- glm(serving_player_won ~ p_server_beats_returner + ElapsedSeconds_fixed + importance + Speed_MPH + I(Speed_MPH**2) + factor(ServeWidth) + factor(ServeDepth), 
                       data = subset_m_first, family = "binomial")
summary(logit_model_7_m) ## p_server_beats_returner *** (pos coef), ElapsedSeconds_fixed * (neg coef), importance *** (pos coef), speed_mph * (neg coef), speed_mph^2 *** (pos coef), servewidth C and W *** (pos coef), servedepth NCTL *** (neg coef)
logit_model_7_f <- glm(serving_player_won ~ p_server_beats_returner + ElapsedSeconds_fixed + importance + Speed_MPH + I(Speed_MPH**2) + factor(ServeWidth) + factor(ServeDepth), 
                       data = subset_f_first, family = "binomial")
summary(logit_model_7_f) ## p_server_beats_returner *** (pos coef), importance *** (pos coef), speed_mph * (neg coef), speed_mph^2 ** (pos coef), servewidth BW, C, and W *, ***, *** (pos coefs), servedepth NCTL *** (neg coef)

logit_model_8_m <- glm(serving_player_won ~ p_server_beats_returner + ElapsedSeconds_fixed + importance + splines::bs(Speed_MPH, degree = 3, df = 5) + factor(ServeWidth) + factor(ServeDepth), 
                       data = subset_m_first, family = "binomial")
summary(logit_model_8_m) ## p_server_beats_returner *** (pos coef), ElapsedSeconds_fixed * (neg coef), importance *** (pos coef), splines::bs(Speed_MPH, degree = 3, df = 5)4 ** (pos coef), servewidth C and W *** (pos coef), servedepth NCTL *** (neg coef)
logit_model_8_f <- glm(serving_player_won ~ p_server_beats_returner + ElapsedSeconds_fixed + importance + splines::bs(Speed_MPH, degree = 3, df = 5) + factor(ServeWidth) + factor(ServeDepth), 
                       data = subset_f_first, family = "binomial")
summary(logit_model_8_f) ## p_server_beats_returner *** (pos coef), importance *** (pos coef), splines::bs(Speed_MPH, degree = 3, df = 5)4 * (pos coef), servewidth C and W *** (pos coefs), servedepth NCTL *** (neg coef)

#-----------------------------------------------------------------------------------------------------

## graph proportion of points won vs. speed_mph
# Step 1: Bin Speed_MPH into intervals (e.g., 5 mph bins)
binned_data <- subset_f_first %>%
  filter(!is.na(Speed_MPH)) %>%
  mutate(speed_bin = cut(Speed_MPH, breaks = seq(floor(min(Speed_MPH)),
                                                 ceiling(max(Speed_MPH)),
                                                 by = 10))) %>%
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
    title = "Empirical Win Rate vs. First Serve Speed (Females)",
    x = "Serve Speed (MPH)",
    y = "Proportion of Points Won"
  ) +
  theme_minimal()
ggsave("../images/female_first_serve_vs_win.png", bg = "white", 
       width = 8, height = 6, units = "in")

## males
# Step 1: Bin Speed_MPH into intervals (e.g., 5 mph bins)
binned_data <- subset_m_first %>%
  filter(!is.na(Speed_MPH)) %>%
  mutate(speed_bin = cut(Speed_MPH, breaks = seq(floor(min(Speed_MPH)),
                                                 ceiling(max(Speed_MPH)),
                                                 by = 10))) %>%
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
    title = "Empirical Win Rate vs. First Serve Speed (Males)",
    x = "Serve Speed (MPH)",
    y = "Proportion of Points Won"
  ) +
  theme_minimal()
ggsave("../images/male_first_serve_vs_win.png", bg = "white", 
       width = 8, height = 6, units = "in")

#-----------------------------------------------------------------------------------------------------

## graph splines
speed_vals <- seq(min(subset_f_first$Speed_MPH, na.rm = TRUE),
                  max(subset_f_first$Speed_MPH, na.rm = TRUE),
                  length.out = 200)

predict_df <- data.frame(
  Speed_MPH = speed_vals,
  p_server_beats_returner = mean(subset_f_first$p_server_beats_returner, na.rm = TRUE),
  ElapsedSeconds_fixed = mean(subset_f_first$ElapsedSeconds_fixed, na.rm = TRUE),
  importance = mean(subset_f_first$importance, na.rm = TRUE),
  ServeWidth = "BC",  # choose a representative category
  ServeDepth = "NCTL" # choose a representative category
)

# Predict probabilities
predict_df$prob_win <- predict(logit_model_8_f, newdata = predict_df, type = "response")

# Plot
ggplot(predict_df, aes(x = Speed_MPH, y = prob_win)) +
  geom_line(size = 1.2, color = "blue") +
  labs(title = "Effect of Serve Speed on Probability of Winning Point",
       x = "Serve Speed (MPH)",
       y = "Predicted Probability") +
  theme_minimal()

# Basis matrix for cubic spline
basis_mat <- as.data.frame(splines::bs(speed_vals, degree = 3, df = 5))
colnames(basis_mat) <- paste0("bs", 1:5)
basis_mat$Speed_MPH <- speed_vals

basis_long <- pivot_longer(basis_mat, cols = starts_with("bs"), names_to = "basis", values_to = "value")

ggplot(basis_long, aes(x = Speed_MPH, y = value, color = basis)) +
  geom_line(size = 1) +
  labs(title = "Cubic Spline Basis Functions for Speed_MPH",
       y = "Basis Function Value") +
  theme_minimal()
ggsave("../images/cubic_spline.png", bg = "white", 
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
