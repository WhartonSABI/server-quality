## ideas:
# (done) wElo ratings vs. serve speed ratio and/or serve location (do better players perform differently?)
# (done) elapsed_seconds vs. serve speed ratio and/or serve location (does strategy change as match goes on?)
# (done) importance vs. serve speed ratio and/or serve location (does strategy change in important points?)

#-----------------------------------------------------------------------------------------------------

rm(list=ls())
# install.packages("fastDummies")
library(welo)
library(tidyverse)
library(data.table)
library(ggplot2)

#-----------------------------------------------------------------------------------------------------

subset_m <- as.data.table(read.csv("../data/wimbledon_subset_m.csv"))
subset_f <- as.data.table(read.csv("../data/wimbledon_subset_f.csv"))

names(subset_m)

subset_m_second <- subset_m[ServeNumber == 2]
subset_f_second <- subset_f[ServeNumber == 2]

#-----------------------------------------------------------------------------------------------------

# wElo ratings vs. serve speed ratio and/or serve location (do better players perform differently?)

m_second_welos <- subset_m_second %>% 
  mutate(server_welo = if_else(PointServer == 1, player1_avg_welo, player2_avg_welo)) %>%
  select(server_welo, speed_ratio)

cor(m_second_welos$server_welo, m_second_welos$speed_ratio, use = "complete.obs") # -0.05 ish

ggplot(m_second_welos, aes(x = server_welo, y = speed_ratio)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Server wElo vs. Second Serve Speed Ratio -- Males (corr ~ -0.05)",
       x = "Server wElo",
       y = "Second Serve Speed Ratio (compared to avg first serve speed)") +
  theme_minimal()
ggsave("../images/male_welo_vs_second_serve_speed.png", bg = "white", 
       width = 8, height = 6, units = "in")
# slight negative correlation, but very weak

f_second_welos <- subset_f_second %>% 
  mutate(server_welo = if_else(PointServer == 1, player1_avg_welo, player2_avg_welo)) %>%
  select(server_welo, speed_ratio)

cor(f_second_welos$server_welo, f_second_welos$speed_ratio, use = "complete.obs") # -0.14 ish

ggplot(f_second_welos, aes(x = server_welo, y = speed_ratio)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Server wElo vs. Second Serve Speed Ratio -- Females (corr ~ -0.14)",
       x = "Server wElo",
       y = "Second Serve Speed Ratio (compared to avg first serve speed)") +
  theme_minimal()
ggsave("../images/female_welo_vs_second_serve_speed.png", bg = "white", 
       width = 8, height = 6, units = "in")

## server wElo vs. serve location
m_categorical_welos <- subset_m_second %>%
  mutate(server_welo = if_else(PointServer == 1, player1_avg_welo, player2_avg_welo))

# Boxplot: Server wElo vs. ServeWidth
ggplot(m_categorical_welos, aes(x = ServeWidth, y = server_welo)) +
  geom_boxplot() +
  labs(title = "Server wElo by Serve Width (Males)",
       x = "Serve Width",
       y = "Server wElo") +
  theme_minimal()
ggsave("../images/male_welo_vs_servewidth.png", bg = "white", width = 7, height = 5, units = "in")

# Boxplot: Server wElo vs. ServeDepth
ggplot(m_categorical_welos, aes(x = ServeDepth, y = server_welo)) +
  geom_boxplot() +
  labs(title = "Server wElo by Serve Depth (Males)",
       x = "Serve Depth",
       y = "Server wElo") +
  theme_minimal()
ggsave("../images/male_welo_vs_servedepth.png", bg = "white", width = 7, height = 5, units = "in")

f_categorical_welos <- subset_f_second %>%
  mutate(server_welo = if_else(PointServer == 1, player1_avg_welo, player2_avg_welo))

# Boxplot: Server wElo vs. ServeWidth
ggplot(f_categorical_welos, aes(x = ServeWidth, y = server_welo)) +
  geom_boxplot() +
  labs(title = "Server wElo by Serve Width (Females)",
       x = "Serve Width",
       y = "Server wElo") +
  theme_minimal()
ggsave("../images/female_welo_vs_servewidth.png", bg = "white", width = 7, height = 5, units = "in")

# Boxplot: Server wElo vs. ServeDepth
ggplot(f_categorical_welos, aes(x = ServeDepth, y = server_welo)) +
  geom_boxplot() +
  labs(title = "Server wElo by Serve Depth (Females)",
       x = "Serve Depth",
       y = "Server wElo") +
  theme_minimal()
ggsave("../images/female_welo_vs_servedepth.png", bg = "white", width = 7, height = 5, units = "in")

#-----------------------------------------------------------------------------------------------------

# elapsed_seconds vs. serve speed ratio and/or serve location (does strategy change as match goes on?)

m_second_elapsed <- subset_m_second %>%
  select(ElapsedSeconds_fixed, speed_ratio)

cor(m_second_elapsed$ElapsedSeconds_fixed, m_second_elapsed$speed_ratio, use = "complete.obs") # -0.04 ish

ggplot(m_second_elapsed, aes(x = ElapsedSeconds_fixed, y = speed_ratio)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Seconds Elapsed (Match) vs. Second Serve Speed Ratio -- Males (corr ~ 0.04)",
       x = "Seconds Elapsed",
       y = "Second Serve Speed Ratio (compared to avg first serve speed)") +
  theme_minimal()
ggsave("../images/male_time_vs_second_serve_speed.png", bg = "white", 
       width = 8, height = 6, units = "in")

f_second_elapsed <- subset_f_second %>% 
  select(ElapsedSeconds_fixed, speed_ratio)

cor(f_second_elapsed$ElapsedSeconds_fixed, f_second_elapsed$speed_ratio, use = "complete.obs") # -0.07 ish

ggplot(f_second_elapsed, aes(x = ElapsedSeconds_fixed, y = speed_ratio)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Seconds Elapsed (Match) vs. Second Serve Speed Ratio -- Females (corr ~ -0.04)",
       x = "Seconds Elapsed",
       y = "Second Serve Speed Ratio (compared to avg first serve speed)") +
  theme_minimal()
ggsave("../images/female_time_vs_second_serve_speed.png", bg = "white", 
       width = 8, height = 6, units = "in")

#-----------------------------------------------------------------------------------------------------

## seconds elapsed vs. serve location

# Boxplot: ServeWidth
ggplot(subset_m_second, aes(x = ServeWidth, y = ElapsedSeconds_fixed)) +
  geom_boxplot() +
  labs(title = "Seconds Elapsed by Serve Width (Males)",
       x = "Serve Width",
       y = "Seconds Elapsed") +
  theme_minimal()
ggsave("../images/male_time_vs_servewidth.png", bg = "white", width = 7, height = 5, units = "in")

# Boxplot: ServeDepth
ggplot(subset_m_second, aes(x = ServeDepth, y = ElapsedSeconds_fixed)) +
  geom_boxplot() +
  labs(title = "Seconds Elapsed by Serve Depth (Males)",
       x = "Serve Depth",
       y = "Seconds Elapsed") +
  theme_minimal()
ggsave("../images/male_time_vs_servedepth.png", bg = "white", width = 7, height = 5, units = "in")

# Boxplot: ServeWidth
ggplot(subset_f_second, aes(x = ServeWidth, y = ElapsedSeconds_fixed)) +
  geom_boxplot() +
  labs(title = "Seconds Elapsed by Serve Width (Females)",
       x = "Serve Width",
       y = "Seconds Elapsed") +
  theme_minimal()
ggsave("../images/female_time_vs_servewidth.png", bg = "white", width = 7, height = 5, units = "in")

# Boxplot: ServeDepth
ggplot(subset_f_second, aes(x = ServeDepth, y = ElapsedSeconds_fixed)) +
  geom_boxplot() +
  labs(title = "Seconds Elapsed by Serve Depth (Females)",
       x = "Serve Depth",
       y = "Seconds Elapsed") +
  theme_minimal()
ggsave("../images/female_time_vs_servedepth.png", bg = "white", width = 7, height = 5, units = "in")

#-----------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------

# importance vs. serve speed ratio and/or serve location (does strategy change in important points?)

cor(subset_m_second$importance, subset_m_second$speed_ratio, use = "complete.obs") # 0.04 ish

ggplot(subset_m_second, aes(x = importance, y = speed_ratio)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Point Importance vs. Second Serve Speed Ratio -- Males (corr ~ 0.04)",
       x = "Point Importance",
       y = "Second Serve Speed Ratio (compared to avg first serve speed)") +
  theme_minimal()
ggsave("../images/male_importance_vs_second_serve_speed.png", bg = "white", 
       width = 8, height = 6, units = "in")

cor(subset_f_second$importance, subset_f_second$speed_ratio, use = "complete.obs") # 0.05 ish

ggplot(subset_f_second, aes(x = importance, y = speed_ratio)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Point Importance vs. Second Serve Speed Ratio -- Females (corr ~ 0.05)",
       x = "Point Importance",
       y = "Second Serve Speed Ratio (compared to avg first serve speed)") +
  theme_minimal()
ggsave("../images/female_importance_vs_second_serve_speed.png", bg = "white", 
       width = 8, height = 6, units = "in")

#-----------------------------------------------------------------------------------------------------

## importance vs. serve location

# Boxplot: ServeWidth
ggplot(subset_m_second, aes(x = ServeWidth, y = importance)) +
  geom_boxplot() +
  labs(title = "Importance by Serve Width (Males)",
       x = "Serve Width",
       y = "Importance") +
  theme_minimal()
ggsave("../images/male_importance_vs_servewidth.png", bg = "white", width = 7, height = 5, units = "in")

# Boxplot: ServeDepth
ggplot(subset_m_second, aes(x = ServeDepth, y = importance)) +
  geom_boxplot() +
  labs(title = "Importance by Serve Depth (Males)",
       x = "Serve Depth",
       y = "Importance") +
  theme_minimal()
ggsave("../images/male_importance_vs_servedepth.png", bg = "white", width = 7, height = 5, units = "in")

# Boxplot: ServeWidth
ggplot(subset_f_second, aes(x = ServeWidth, y = importance)) +
  geom_boxplot() +
  labs(title = "Importance by Serve Width (Females)",
       x = "Serve Width",
       y = "Seconds Elapsed") +
  theme_minimal()
ggsave("../images/female_importance_vs_servewidth.png", bg = "white", width = 7, height = 5, units = "in")

# Boxplot: ServeDepth
ggplot(subset_f_second, aes(x = ServeDepth, y = importance)) +
  geom_boxplot() +
  labs(title = "Importance by Serve Depth (Females)",
       x = "Serve Depth",
       y = "Importance") +
  theme_minimal()
ggsave("../images/female_importance_vs_servedepth.png", bg = "white", width = 7, height = 5, units = "in")

#-----------------------------------------------------------------------------------------------------
