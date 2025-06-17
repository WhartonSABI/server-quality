rm(list=ls())
# install.packages("welo")
library(welo)
library(tidyverse)
library(data.table)
library(ggplot2)

#-----------------------------------------------------------------------------------------------------

subset_m <- as.data.table(read.csv("../data/wimbledon_subset_m.csv"))
subset_f <- as.data.table(read.csv("../data/wimbledon_subset_f.csv"))

names(subset_m)

#-----------------------------------------------------------------------------------------------------

## TODO: create new column for state of game (based on P1Score and P2Score). then, add score importance metric.

#-----------------------------------------------------------------------------------------------------

# # Convert ELO to logistic (Bradley-Terry scale)
# subset_m[, welo_p1_bt := 0.0057565 * player1_avg_welo]
# subset_m[, welo_p2_bt := 0.0057565 * player2_avg_welo]
# 
# ## male
# subset_m <- subset_m %>% 
#   mutate(p_server_beats_returner <- ifelse(PointServer == 1,
#                                            1 / (1 + exp(welo_p1_bt - welo_p2_bt)),
#                                            1 / (1 + exp(welo_p2_bt - welo_p1_bt))))
# 
# # rename column in subset_m
# setnames(subset_m, old = c("... <- NULL"), 
#          new = c("p_server_beats_returner"))
# 
# # write.csv(subset_m, "../data/wimbledon_subset_m.csv", row.names = FALSE)

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
# # write.csv(subset_f, "../data/wimbledon_subset_f.csv", row.names = FALSE)

# -----------------------------------------------------------------------------------------------------

## logistic regression for serving_player_won vs. p_server_beats_returner
logit_model_m <- glm(serving_player_won ~ p_server_beats_returner + 0, 
                     data = subset_m, family = "binomial")
summary(logit_model_m) ## not significant

logit_model_f <- glm(serving_player_won ~ p_server_beats_returner + 0, 
                     data = subset_f, family = "binomial")
summary(logit_model_f) ## not significant

#-----------------------------------------------------------------------------------------------------

## add covariates

logit_model_2_m <- glm(serving_player_won ~ p_server_beats_returner + ElapsedSeconds + 0, 
                     data = subset_m, family = "binomial")
summary(logit_model_2_m) ## not significant

logit_model_2_f <- glm(serving_player_won ~ p_server_beats_returner + ElapsedSeconds + 0, 
                     data = subset_f, family = "binomial")
summary(logit_model_2_f) ## only elapsedseconds significant * (neg coef)

#-----------------------------------------------------------------------------------------------------

#-----------------------------------------------------------------------------------------------------

# new models with just second serves
subset_m_second <- subset_m[ServeNumber == 2]
subset_f_second <- subset_f[ServeNumber == 2]

## logistic regression for serving_player_won vs. p_server_beats_returner
logit_model_m <- glm(serving_player_won ~ p_server_beats_returner, 
                     data = subset_m_second, family = "binomial")
summary(logit_model_m) ## p_server_beats_returner significant *** (pos coef)

logit_model_f <- glm(serving_player_won ~ p_server_beats_returner, 
                     data = subset_f_second, family = "binomial")
summary(logit_model_f) ## not significant

#-----------------------------------------------------------------------------------------------------

## add covariates

logit_model_2_m <- glm(serving_player_won ~ p_server_beats_returner + ElapsedSeconds, 
                       data = subset_m_second, family = "binomial")
summary(logit_model_2_m) ## p_server_beats_returner significant *** (pos coef), ElapsedSeconds not significant

logit_model_2_f <- glm(serving_player_won ~ p_server_beats_returner + ElapsedSeconds, 
                       data = subset_f_second, family = "binomial")
summary(logit_model_2_f) ## neither significant

#-----------------------------------------------------------------------------------------------------

## add second serve speed ratio
logit_model_3_m <- glm(serving_player_won ~ p_server_beats_returner + ElapsedSeconds + speed_ratio, 
                       data = subset_m_second, family = "binomial")
summary(logit_model_3_m) ## p_server_beats_returner significant *** (pos coef), ElapsedSeconds not significant, speed_ratio not significant

logit_model_3_f <- glm(serving_player_won ~ p_server_beats_returner + ElapsedSeconds + speed_ratio, 
                       data = subset_f_second, family = "binomial")
summary(logit_model_3_f) ## nothing significant

#-----------------------------------------------------------------------------------------------------

## add score importance
logit_model_4_m <- glm(serving_player_won ~ p_server_beats_returner + ElapsedSeconds + speed_ratio + importance, 
                       data = subset_m_second, family = "binomial")
summary(logit_model_4_m) ## p_server_beats_returner significant *** (pos coef), nothing else significant. but intercept is significant *** (neg coef)

logit_model_4_f <- glm(serving_player_won ~ p_server_beats_returner + ElapsedSeconds + speed_ratio + importance,  
                       data = subset_f_second, family = "binomial")
summary(logit_model_4_f) ## nothing significant

#-----------------------------------------------------------------------------------------------------

## add location of serve
## add intercept b/c location of serve is categorical
logit_model_5_m <- glm(serving_player_won ~ p_server_beats_returner + ElapsedSeconds + importance + speed_ratio + factor(ServeWidth) + factor(ServeDepth) + 0, 
                       data = subset_m_second, family = "binomial")
summary(logit_model_5_m) ## p_server_beats_returner significant *** (pos coef), each factor in ServeWidth significant (**, ***) (neg coefs) but not ServeDepth
logit_model_5_f <- glm(serving_player_won ~ p_server_beats_returner + ElapsedSeconds + importance + speed_ratio + factor(ServeWidth) + factor(ServeDepth) + 0, 
                       data = subset_f_second, family = "binomial")
summary(logit_model_5_f) ## nothing significant

#-----------------------------------------------------------------------------------------------------

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
