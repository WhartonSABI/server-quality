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

m_one_rally <- subset_m %>% 
  filter(RallyCount == 1)

m_more_rally <- subset_m %>% 
  filter(RallyCount > 1)

# rallycount == 1 doesn't imply server automatically wins?

# plot distn of P1DistanceRun and P2Distance Run 
# if rally count = 1
ggplot(m_one_rally, aes(x = P1DistanceRun)) +
  geom_histogram(binwidth = 10, fill = "blue", alpha = 0.5) +
  labs(title = "Distribution of P1 Distance Run in One Rally Matches",
       x = "P1 Distance Run (m)", y = "Frequency") +
  geom_vline(data = m_one_rally, aes(xintercept = mean(P1DistanceRun, na.rm = TRUE)), 
             color = "red", linetype = "dashed", size = 1) +
  theme_minimal()

ggplot(m_one_rally, aes(x = P2DistanceRun)) +
  geom_histogram(binwidth = 10, fill = "blue", alpha = 0.5) +
  labs(title = "Distribution of P2 Distance Run in One Rally Matches",
       x = "P1 Distance Run (m)", y = "Frequency") +
  geom_vline(data = m_one_rally, aes(xintercept = mean(P2DistanceRun, na.rm = TRUE)), 
             color = "red", linetype = "dashed", size = 1) +
  theme_minimal()

# if rally count > 1
ggplot(m_more_rally, aes(x = P1DistanceRun)) +
  geom_histogram(binwidth = 10, fill = "blue", alpha = 0.5) +
  labs(title = "Distribution of P1 Distance Run if RallyCount > 1",
       x = "P1 Distance Run (m)", y = "Frequency") +
  geom_vline(data = m_more_rally, aes(xintercept = mean(P1DistanceRun, na.rm = TRUE)), 
             color = "red", linetype = "dashed", size = 1) +
  theme_minimal()

ggplot(m_more_rally, aes(x = P2DistanceRun)) +
  geom_histogram(binwidth = 10, fill = "blue", alpha = 0.5) +
  labs(title = "Distribution of P2 Distance Run if RallyCount > 1",
       x = "P1 Distance Run (m)", y = "Frequency") +
  geom_vline(data = m_more_rally, aes(xintercept = mean(P1DistanceRun, na.rm = TRUE)), 
             color = "red", linetype = "dashed", size = 1) +
  theme_minimal()

#-----------------------------------------------------------------------------------------------------

## maybe run two more groups of regressions--one with RallyCount == 1 data and one with RallyCount > 1 data?
## but idk then our formula would be E(server win probability | it's a short rally & elapsed time & serve speed & ... )
## and {short rally} might not be independent of elapsed time & importance & ... 
## if anything, rally count could be a y variable :P but nah