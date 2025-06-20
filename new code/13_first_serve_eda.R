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

subset_m_first <- subset_m[ServeNumber == 1]
subset_f_first <- subset_f[ServeNumber == 1]

#-----------------------------------------------------------------------------------------------------

## corr matrix male
numerical_x_cols <- c("p_server_beats_returner", "ElapsedSeconds_fixed", "importance", "speed_ratio")

subset_m_x <- subset_m_first %>% select(all_of(numerical_x_cols))

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
subset_m_dummies <- subset_m_first %>%
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
ggsave("../images/corr_matrix_male_first.png", bg = "white", 
       width = 8, height = 6, units = "in")

#-----------------------------------------------------------------------------------------------------

## corr matrix female
numerical_x_cols <- c("p_server_beats_returner", "ElapsedSeconds_fixed", "importance", "speed_ratio")

subset_f_x <- subset_f_first %>% select(all_of(numerical_x_cols))

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
subset_f_dummies <- subset_f_first %>%
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
ggsave("../images/corr_matrix_female_first.png", bg = "white", 
       width = 8, height = 6, units = "in")

#-----------------------------------------------------------------------------------------------------

# wElo ratings vs. serve speed ratio and/or serve location (do better players perform differently?)

m_second_welos <- subset_m_first %>% 
  mutate(server_welo = if_else(PointServer == 1, player1_avg_welo, player2_avg_welo)) %>%
  select(server_welo, speed_ratio)

cor(m_second_welos$server_welo, m_second_welos$speed_ratio, use = "complete.obs") # -0.001 ish

ggplot(m_second_welos, aes(x = server_welo, y = speed_ratio)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Server wElo vs. First Serve Speed Ratio -- Males (corr ~ -0.05)",
       x = "Server wElo",
       y = "First Serve Speed Ratio (compared to avg first serve speed)") +
  theme_minimal()
ggsave("../images/male_welo_vs_first_serve_speed.png", bg = "white", 
       width = 8, height = 6, units = "in")
# slight negative correlation, but very weak

f_second_welos <- subset_f_first %>% 
  mutate(server_welo = if_else(PointServer == 1, player1_avg_welo, player2_avg_welo)) %>%
  select(server_welo, speed_ratio)

cor(f_second_welos$server_welo, f_second_welos$speed_ratio, use = "complete.obs") # -0.002 ish

ggplot(f_second_welos, aes(x = server_welo, y = speed_ratio)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Server wElo vs. First Serve Speed Ratio -- Females (corr ~ -0.14)",
       x = "Server wElo",
       y = "First Serve Speed Ratio (compared to avg first serve speed)") +
  theme_minimal()
ggsave("../images/female_welo_vs_first_serve_speed.png", bg = "white", 
       width = 8, height = 6, units = "in")

## server wElo vs. serve location
m_categorical_welos <- subset_m_first %>%
  mutate(server_welo = if_else(PointServer == 1, player1_avg_welo, player2_avg_welo))

# Boxplot: Server wElo vs. ServeWidth
ggplot(m_categorical_welos, aes(x = ServeWidth, y = server_welo)) +
  geom_boxplot() +
  labs(title = "Server wElo by Serve Width (Males)",
       x = "Serve Width",
       y = "Server wElo") +
  theme_minimal()
ggsave("../images/male_welo_vs_servewidth_first.png", bg = "white", width = 7, height = 5, units = "in")

# Boxplot: Server wElo vs. ServeDepth
ggplot(m_categorical_welos, aes(x = ServeDepth, y = server_welo)) +
  geom_boxplot() +
  labs(title = "Server wElo by Serve Depth (Males)",
       x = "Serve Depth",
       y = "Server wElo") +
  theme_minimal()
ggsave("../images/male_welo_vs_servedepth_first.png", bg = "white", width = 7, height = 5, units = "in")

f_categorical_welos <- subset_f_first %>%
  mutate(server_welo = if_else(PointServer == 1, player1_avg_welo, player2_avg_welo))

# Boxplot: Server wElo vs. ServeWidth
ggplot(f_categorical_welos, aes(x = ServeWidth, y = server_welo)) +
  geom_boxplot() +
  labs(title = "Server wElo by Serve Width (Females)",
       x = "Serve Width",
       y = "Server wElo") +
  theme_minimal()
ggsave("../images/female_welo_vs_servewidth_first.png", bg = "white", width = 7, height = 5, units = "in")

# Boxplot: Server wElo vs. ServeDepth
ggplot(f_categorical_welos, aes(x = ServeDepth, y = server_welo)) +
  geom_boxplot() +
  labs(title = "Server wElo by Serve Depth (Females)",
       x = "Serve Depth",
       y = "Server wElo") +
  theme_minimal()
ggsave("../images/female_welo_vs_servedepth_first.png", bg = "white", width = 7, height = 5, units = "in")

#-----------------------------------------------------------------------------------------------------

# elapsed_seconds vs. serve speed ratio and/or serve location (does strategy change as match goes on?)

m_second_elapsed <- subset_m_first %>%
  select(ElapsedSeconds_fixed, speed_ratio)

cor(m_second_elapsed$ElapsedSeconds_fixed, m_second_elapsed$speed_ratio, use = "complete.obs") # -0.02 ish

ggplot(m_second_elapsed, aes(x = ElapsedSeconds_fixed, y = speed_ratio)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Seconds Elapsed (Match) vs. First Serve Speed Ratio -- Males (corr ~ 0.02)",
       x = "Seconds Elapsed",
       y = "First Serve Speed Ratio (compared to avg first serve speed)") +
  theme_minimal()
ggsave("../images/male_time_vs_first_serve_speed.png", bg = "white", 
       width = 8, height = 6, units = "in")

f_second_elapsed <- subset_f_first %>% 
  select(ElapsedSeconds_fixed, speed_ratio)

cor(f_second_elapsed$ElapsedSeconds_fixed, f_second_elapsed$speed_ratio, use = "complete.obs") # -0.06 ish

ggplot(f_second_elapsed, aes(x = ElapsedSeconds_fixed, y = speed_ratio)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Seconds Elapsed (Match) vs. First Serve Speed Ratio -- Females (corr ~ -0.06)",
       x = "Seconds Elapsed",
       y = "First Serve Speed Ratio (compared to avg first serve speed)") +
  theme_minimal()
ggsave("../images/female_time_vs_second_serve_speed.png", bg = "white", 
       width = 8, height = 6, units = "in")

#-----------------------------------------------------------------------------------------------------

## seconds elapsed vs. serve location

# Boxplot: ServeWidth
ggplot(subset_m_first, aes(x = ServeWidth, y = ElapsedSeconds_fixed)) +
  geom_boxplot() +
  labs(title = "Seconds Elapsed by Serve Width (Males)",
       x = "Serve Width",
       y = "Seconds Elapsed") +
  theme_minimal()
ggsave("../images/male_time_vs_servewidth_first.png", bg = "white", width = 7, height = 5, units = "in")

# Boxplot: ServeDepth
ggplot(subset_m_first, aes(x = ServeDepth, y = ElapsedSeconds_fixed)) +
  geom_boxplot() +
  labs(title = "Seconds Elapsed by Serve Depth (Males)",
       x = "Serve Depth",
       y = "Seconds Elapsed") +
  theme_minimal()
ggsave("../images/male_time_vs_servedepth_first.png", bg = "white", width = 7, height = 5, units = "in")

# Boxplot: ServeWidth
ggplot(subset_f_first, aes(x = ServeWidth, y = ElapsedSeconds_fixed)) +
  geom_boxplot() +
  labs(title = "Seconds Elapsed by Serve Width (Females)",
       x = "Serve Width",
       y = "Seconds Elapsed") +
  theme_minimal()
ggsave("../images/female_time_vs_servewidth_first.png", bg = "white", width = 7, height = 5, units = "in")

# Boxplot: ServeDepth
ggplot(subset_f_first, aes(x = ServeDepth, y = ElapsedSeconds_fixed)) +
  geom_boxplot() +
  labs(title = "Seconds Elapsed by Serve Depth (Females)",
       x = "Serve Depth",
       y = "Seconds Elapsed") +
  theme_minimal()
ggsave("../images/female_time_vs_servedepth_first.png", bg = "white", width = 7, height = 5, units = "in")

#-----------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------

# importance vs. serve speed ratio and/or serve location (does strategy change in important points?)

cor(subset_m_first$importance, subset_m_first$speed_ratio, use = "complete.obs") # 0.02 ish

ggplot(subset_m_first, aes(x = importance, y = speed_ratio)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Point Importance vs. First Serve Speed Ratio -- Males (corr ~ 0.02)",
       x = "Point Importance",
       y = "First Serve Speed Ratio (compared to avg first serve speed)") +
  theme_minimal()
ggsave("../images/male_importance_vs_first_serve_speed.png", bg = "white", 
       width = 8, height = 6, units = "in")

cor(subset_f_first$importance, subset_f_first$speed_ratio, use = "complete.obs") # -0.008 ish

ggplot(subset_f_first, aes(x = importance, y = speed_ratio)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Point Importance vs. First Serve Speed Ratio -- Females (corr ~ -0.008)",
       x = "Point Importance",
       y = "First Serve Speed Ratio (compared to avg first serve speed)") +
  theme_minimal()
ggsave("../images/female_importance_vs_first_serve_speed.png", bg = "white", 
       width = 8, height = 6, units = "in")

#-----------------------------------------------------------------------------------------------------

## importance vs. serve location

# Boxplot: ServeWidth
ggplot(subset_m_first, aes(x = ServeWidth, y = importance)) +
  geom_boxplot() +
  labs(title = "Importance by Serve Width (Males)",
       x = "Serve Width",
       y = "Importance") +
  theme_minimal()
ggsave("../images/male_importance_vs_servewidth_first.png", bg = "white", width = 7, height = 5, units = "in")

# Boxplot: ServeDepth
ggplot(subset_m_first, aes(x = ServeDepth, y = importance)) +
  geom_boxplot() +
  labs(title = "Importance by Serve Depth (Males)",
       x = "Serve Depth",
       y = "Importance") +
  theme_minimal()
ggsave("../images/male_importance_vs_servedepth_first.png", bg = "white", width = 7, height = 5, units = "in")

# Boxplot: ServeWidth
ggplot(subset_f_first, aes(x = ServeWidth, y = importance)) +
  geom_boxplot() +
  labs(title = "Importance by Serve Width (Females)",
       x = "Serve Width",
       y = "Seconds Elapsed") +
  theme_minimal()
ggsave("../images/female_importance_vs_servewidth_first.png", bg = "white", width = 7, height = 5, units = "in")

# Boxplot: ServeDepth
ggplot(subset_f_first, aes(x = ServeDepth, y = importance)) +
  geom_boxplot() +
  labs(title = "Importance by Serve Depth (Females)",
       x = "Serve Depth",
       y = "Importance") +
  theme_minimal()
ggsave("../images/female_importance_vs_servedepth_first.png", bg = "white", width = 7, height = 5, units = "in")

#-----------------------------------------------------------------------------------------------------
