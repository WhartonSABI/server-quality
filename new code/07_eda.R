# rm(list=ls())
# install.packages("welo")
library(welo)
library(tidyverse)
library(data.table)
library(ggplot2)

#-----------------------------------------------------------------------------------------------------

## wimbledon 2021
subset_m <- as.data.table(read.csv("../data/wimbledon_subset_m.csv"))
subset_f <- as.data.table(read.csv("../data/wimbledon_subset_f.csv"))

names(subset_m)

#-----------------------------------------------------------------------------------------------------

## EDA male
# graph distribution of serve speed (speed_mph) after splitting into first & second serves

mean_speeds <- subset_m %>%
  group_by(ServeNumber) %>%
  summarise(mean_speed = mean(Speed_MPH, na.rm = TRUE))

ggplot(subset_m, aes(x = Speed_MPH)) +
  geom_histogram(binwidth = 5, fill = "blue", color = "black", alpha = 0.7) +
  facet_wrap(~ ServeNumber) +
  geom_vline(data = mean_speeds, aes(xintercept = mean_speed), 
             color = "red", linetype = "dashed", size = 1) +
  labs(title = "Distribution of Serve Speed (mph) by Serve Number (Males)",
       x = "Serve Speed (mph)",
       y = "Count") +
  theme_minimal()
# save graph
ggsave("../images/serve_speed_males.png")

#-----------------------------------------------------------------------------------------------------

## EDA female
# graph distribution of serve speed (speed_mph) after splitting into first & second serves

mean_speeds_f <- subset_f %>%
  group_by(ServeNumber) %>%
  summarise(mean_speed = mean(Speed_MPH, na.rm = TRUE))

ggplot(subset_f, aes(x = Speed_MPH)) +
  geom_histogram(binwidth = 5, fill = "blue", color = "black", alpha = 0.7) +
  facet_wrap(~ ServeNumber) +
  geom_vline(data = mean_speeds_f, aes(xintercept = mean_speed), 
             color = "red", linetype = "dashed", size = 1) +
  labs(title = "Distribution of Serve Speed (mph) by Serve Number (2021 Males)",
       x = "Serve Speed (mph)",
       y = "Count") +
  theme_minimal()

#-----------------------------------------------------------------------------------------------------

## serve speeds vs. win outcome

## all serves (first and second)
ggplot(subset_m, aes(x = Speed_MPH, y = serving_player_won)) +
  geom_density2d_filled(alpha = 0.8) +
  labs(title = "Serve Speed vs. Win Outcome (Males, All Serves)") +
  theme_minimal()

ggplot(subset_m, aes(x = Speed_MPH, y = serving_player_won)) +
  geom_point() +
  labs(title = "Serve Speed vs. Win Outcome") +
  theme_minimal()

## split between 1st and 2nd serves
# first serves
first_serves <- subset_m %>% 
  filter(ServeNumber == 1)

ggplot(first_serves, aes(x = Speed_MPH, y = serving_player_won)) +
  geom_point() +
  labs(title = "Serve Speed vs. Win Outcome for First Serves") +
  theme_minimal()

ggplot(first_serves, aes(x = Speed_MPH, y = serving_player_won)) +
  geom_density2d_filled(alpha = 0.8) +
  labs(title = "Serve Speed vs. Win Outcome for First Serves (Males, First Serves)") +
  theme_minimal()

second_serves <- subset_m %>% 
  filter(ServeNumber == 2)

ggplot(second_serves, aes(x = Speed_MPH, y = serving_player_won)) +
  geom_point() +
  labs(title = "Serve Speed vs. Win Outcome for Second Serves (Males, Second Serves)") +
  theme_minimal()

ggplot(second_serves, aes(x = Speed_MPH, y = serving_player_won)) +
  geom_density2d_filled(alpha = 0.8) +
  labs(title = "Serve Speed vs. Win Outcome for Second Serves (Males, Second Serves)") +
  theme_minimal()

## serve speeds are all pretty much the same

