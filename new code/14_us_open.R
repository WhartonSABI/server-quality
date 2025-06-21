rm(list=ls()) # clear environment

# install.packages(c("rio", "data.table", "ggplot2","dplyr","tidyverse"))
library(rio)
library(data.table)
library(ggplot2)
library(dplyr)
library(tidyverse)

#-----------------------------------------------------------------------------------------------------

## us open 2021

usopen_2021_matches <- as.data.table(read.csv("../data/raw_data/2021-usopen-matches.csv"))
colSums(is.na(usopen_2021_matches))

usopen_2021_points <- as.data.table(read.csv("../data/raw_data/2021-usopen-points.csv"))
colSums(is.na(usopen_2021_points))

# remove all cols with NAs
usopen_2021_matches <- usopen_2021_matches %>%
  select(where(~ all(!is.na(.))))
names(usopen_2021_matches)

usopen_2021_points <- usopen_2021_points %>%
  select(where(~ all(!is.na(.))))
names(usopen_2021_points)

usopen_2021 <- left_join(usopen_2021_points, usopen_2021_matches, by = "match_id")
names(usopen_2021)
colSums(is.na(usopen_2021))

# binary variable: whether serving player won or lost
usopen_2021 <- usopen_2021 %>%
  filter(PointServer != 0, Speed_KMH != 0, Speed_MPH != 0) %>%
  mutate(serving_player_won = ifelse((PointServer == 1 & PointWinner == 1) | (PointServer == 2 & PointWinner == 2), 1, 0))


write.csv(wimbledon_2011, "../data/wimbledon_2011_combined.csv", row.names = FALSE)


#-----------------------------------------------------------------------------------------------------
