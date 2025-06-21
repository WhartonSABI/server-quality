rm(list=ls())
# install.packages("fastDummies")
library(welo)
library(tidyverse)
library(data.table)
library(ggplot2)
library(reshape2)

#-----------------------------------------------------------------------------------------------------

subset_m <- as.data.table(read.csv("../data/wimbledon_subset_m.csv"))
subset_f <- as.data.table(read.csv("../data/wimbledon_subset_f.csv"))

names(subset_m)

subset_m_second <- subset_m[ServeNumber == 2]
subset_f_second <- subset_f[ServeNumber == 2]

#-----------------------------------------------------------------------------------------------------

## corr matrix male
numerical_x_cols <- c("p_server_beats_returner", "ElapsedSeconds_fixed", "importance", "speed_ratio")

subset_m_x <- subset_m_second %>% select(all_of(numerical_x_cols))

cor_matrix <- cor(subset_m_x, use = "complete.obs", method = "pearson")
cor_melt <- melt(cor_matrix)

ggplot(cor_melt, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(value, 2)), size = 3) +  # Add correlation values
  scale_fill_gradient2(low = "blue", high = "red", mid = "white",
                       midpoint = 0, limit = c(-1, 1), name = "Correlation") +
  labs(x = "", y = "", title = "Correlation Matrix with Coefficients") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# include categorical variables
subset_m_dummies <- subset_m_second %>%
  mutate(across(c(ServeWidth, ServeDepth), as.factor)) %>%
  fastDummies::dummy_cols(select_columns = c("ServeWidth", "ServeDepth"), remove_first_dummy = TRUE, remove_selected_columns = TRUE)

subset_m_all <- subset_m_dummies %>%
  select(all_of(numerical_x_cols), starts_with("ServeWidth_"), starts_with("ServeDepth_"))

cor_matrix <- cor(subset_m_all, use = "complete.obs", method = "pearson")
cor_melt <- melt(cor_matrix)

ggplot(cor_melt, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(value, 2)), size = 3) +  # Add correlation values
  scale_fill_gradient2(low = "blue", high = "red", mid = "white",
                       midpoint = 0, limit = c(-1, 1), name = "Correlation") +
  labs(x = "", y = "", title = "Correlation Matrix with Coefficients") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("../images/corr_matrix_male.png", bg = "white", 
       width = 8, height = 6, units = "in")

#-----------------------------------------------------------------------------------------------------

## corr matrix female
numerical_x_cols <- c("p_server_beats_returner", "ElapsedSeconds_fixed", "importance", "speed_ratio")

subset_f_x <- subset_f_second %>% select(all_of(numerical_x_cols))

cor_matrix <- cor(subset_f_x, use = "complete.obs", method = "pearson")
cor_melt <- melt(cor_matrix)

ggplot(cor_melt, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(value, 2)), size = 3) +  # Add correlation values
  scale_fill_gradient2(low = "blue", high = "red", mid = "white",
                       midpoint = 0, limit = c(-1, 1), name = "Correlation") +
  labs(x = "", y = "", title = "Correlation Matrix with Coefficients") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# include categorical variables
subset_f_dummies <- subset_f_second %>%
  mutate(across(c(ServeWidth, ServeDepth), as.factor)) %>%
  fastDummies::dummy_cols(select_columns = c("ServeWidth", "ServeDepth"), remove_first_dummy = TRUE, remove_selected_columns = TRUE)

subset_f_all <- subset_f_dummies %>%
  select(all_of(numerical_x_cols), starts_with("ServeWidth_"), starts_with("ServeDepth_"))

cor_matrix <- cor(subset_f_all, use = "complete.obs", method = "pearson")
cor_melt <- melt(cor_matrix)

ggplot(cor_melt, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(value, 2)), size = 3) +  # Add correlation values
  scale_fill_gradient2(low = "blue", high = "red", mid = "white",
                       midpoint = 0, limit = c(-1, 1), name = "Correlation") +
  labs(x = "", y = "", title = "Correlation Matrix with Coefficients") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("../images/corr_matrix_female.png", bg = "white", 
       width = 8, height = 6, units = "in")

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
ggsave("../images/serve_speed_males.png", bg = "white", 
       width = 8, height = 6, units = "in")

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
  labs(title = "Distribution of Serve Speed (mph) by Serve Number (Females)",
       x = "Serve Speed (mph)",
       y = "Count") +
  theme_minimal()
ggsave("../images/serve_speed_females.png", bg = "white", 
       width = 8, height = 6, units = "in")

#-----------------------------------------------------------------------------------------------------

## serve speeds vs. win outcome

## all serves (first and second)
ggplot(subset_m, aes(x = Speed_MPH, y = serving_player_won)) +
  geom_density2d_filled(alpha = 0.8) +
  labs(title = "Serve Speed vs. Win Outcome (Males, All Serves)") +
  theme_minimal()
ggsave("../images/serve_speed_vs_win_males_all.png", bg = "white", 
       width = 8, height = 6, units = "in")

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
ggsave("../images/serve_speed_vs_win_males_first.png", bg = "white", 
       width = 8, height = 6, units = "in")


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
ggsave("../images/serve_speed_vs_win_males_second.png", bg = "white", 
       width = 8, height = 6, units = "in")
## serve speeds are all pretty much the same

