rm(list=ls())
# install.packages("welo")
library(welo)
library(tidyverse)
library(data.table)
library(ggplot2)
# install.packages("car")
library(car)

#-----------------------------------------------------------------------------------------------------

subset_m <- as.data.table(read.csv("../data/usopen_subset_m.csv"))
subset_f <- as.data.table(read.csv("../data/usopen_subset_f.csv"))

names(subset_m)

#-----------------------------------------------------------------------------------------------------

# new models with just first serves
subset_m_first <- subset_m[ServeNumber == 1]
subset_f_first <- subset_f[ServeNumber == 1]

## speed ratio linear
logit_model_5_m <- glm(serving_player_won ~ p_server_beats_returner + ElapsedSeconds_fixed + importance + speed_ratio + factor(ServeWidth) + factor(ServeDepth), 
                       data = subset_m_first, family = "binomial")
summary(logit_model_5_m) ## pretty much everything **, ***
logit_model_5_f <- glm(serving_player_won ~ p_server_beats_returner + ElapsedSeconds_fixed + importance + speed_ratio + factor(ServeWidth) + factor(ServeDepth), 
                       data = subset_f_first, family = "binomial")
summary(logit_model_5_f) ## pretty much everything **, *** except elapsed_time

vif(logit_model_5_m) # acceptable (all less than 1.6ish)
vif(logit_model_5_f)

## speed_ratio quadratic and spline
logit_model_9_m <- glm(serving_player_won ~ p_server_beats_returner + ElapsedSeconds_fixed + importance + speed_ratio + I(speed_ratio**2) + factor(ServeWidth) + factor(ServeDepth), 
                       data = subset_m_first, family = "binomial")
summary(logit_model_9_m) ## doesn't work as well
logit_model_9_f <- glm(serving_player_won ~ p_server_beats_returner + ElapsedSeconds_fixed + importance + speed_ratio + I(speed_ratio**2) + factor(ServeWidth) + factor(ServeDepth), 
                       data = subset_f_first, family = "binomial")
summary(logit_model_9_f) ## hmm quadratic works here though

logit_model_10_m <- glm(serving_player_won ~ p_server_beats_returner + ElapsedSeconds_fixed + importance + splines::bs(speed_ratio, degree = 3, df = 5) + factor(ServeWidth) + factor(ServeDepth), 
                        data = subset_m_first, family = "binomial")
summary(logit_model_10_m) ## p_server_beats_returner *** (pos coef), ElapsedSeconds_fixed * (neg coef), importance *** (pos coef), splines::bs(speed_ratio, degree = 3, df = 5)4 * (pos coef), servewidth C and W *** (pos coef), servedepth NCTL *** (neg coef)
logit_model_10_f <- glm(serving_player_won ~ p_server_beats_returner + ElapsedSeconds_fixed + importance + splines::bs(speed_ratio, degree = 3, df = 5) + factor(ServeWidth) + factor(ServeDepth), 
                        data = subset_f_first, family = "binomial")
summary(logit_model_10_f) ## p_server_beats_returner ** (pos coef), importance *** (pos coef), servewidth BW, C, and W *, ***, *** (pos coefs), servedepth NCTL *** (neg coef)

#-----------------------------------------------------------------------------------------------------

## use serve speed instead of serve ratio
logit_model_6_m <- glm(serving_player_won ~ p_server_beats_returner + ElapsedSeconds_fixed + importance + Speed_MPH + factor(ServeWidth) + factor(ServeDepth), 
                       data = subset_m_first, family = "binomial")
summary(logit_model_6_m) ## pretty much everything significant
logit_model_6_f <- glm(serving_player_won ~ p_server_beats_returner + ElapsedSeconds_fixed + importance + Speed_MPH + factor(ServeWidth) + factor(ServeDepth), 
                       data = subset_f_first, family = "binomial")
summary(logit_model_6_f) ## pretty much everything except elapsed_time ***

logit_model_7_m <- glm(serving_player_won ~ p_server_beats_returner + ElapsedSeconds_fixed + importance + Speed_MPH + I(Speed_MPH**2) + factor(ServeWidth) + factor(ServeDepth), 
                       data = subset_m_first, family = "binomial")
summary(logit_model_7_m) # quadratic works pretty well ***
logit_model_7_f <- glm(serving_player_won ~ p_server_beats_returner + ElapsedSeconds_fixed + importance + Speed_MPH + I(Speed_MPH**2) + factor(ServeWidth) + factor(ServeDepth), 
                       data = subset_f_first, family = "binomial")
summary(logit_model_7_f) # quadratic works pretty well ***

logit_model_8_m <- glm(serving_player_won ~ p_server_beats_returner + ElapsedSeconds_fixed + importance + splines::bs(Speed_MPH, degree = 3, df = 5) + factor(ServeWidth) + factor(ServeDepth), 
                       data = subset_m_first, family = "binomial")
summary(logit_model_8_m) # spline is *, **
logit_model_8_f <- glm(serving_player_won ~ p_server_beats_returner + ElapsedSeconds_fixed + importance + splines::bs(Speed_MPH, degree = 3, df = 5) + factor(ServeWidth) + factor(ServeDepth), 
                       data = subset_f_first, family = "binomial")
summary(logit_model_8_f) # no spline
