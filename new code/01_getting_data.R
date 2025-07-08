rm(list=ls()) # clear environment

# install.packages(c("rio", "data.table", "ggplot2","dplyr","tidyverse"))
library(rio)
library(data.table)
library(ggplot2)
library(dplyr)
library(tidyverse)

#-----------------------------------------------------------------------------------------------------

## wimbledon 2011

wimbledon_2011_matches <- as.data.table(read.csv("../data/raw_data/2011-wimbledon-matches.csv"))
colSums(is.na(wimbledon_2011_matches))

wimbledon_2011_points <- as.data.table(read.csv("../data/raw_data/2011-wimbledon-points.csv"))
colSums(is.na(wimbledon_2011_points))

# remove all cols with NAs
wimbledon_2011_matches <- wimbledon_2011_matches %>%
  select(where(~ all(!is.na(.))))
names(wimbledon_2011_matches)

wimbledon_2011_points <- wimbledon_2011_points %>%
  select(where(~ all(!is.na(.))))
names(wimbledon_2011_points)

wimbledon_2011 <- left_join(wimbledon_2011_points, wimbledon_2011_matches, by = "match_id")
names(wimbledon_2011)
colSums(is.na(wimbledon_2011))

write.csv(wimbledon_2011, "../data/wimbledon_2011_combined.csv", row.names = FALSE)


#-----------------------------------------------------------------------------------------------------

## wimbledon 2012

wimbledon_2012_matches <- as.data.table(read.csv("../data/raw_data/2012-wimbledon-matches.csv"))
colSums(is.na(wimbledon_2012_matches))

wimbledon_2012_points <- as.data.table(read.csv("../data/raw_data/2012-wimbledon-points.csv"))
colSums(is.na(wimbledon_2012_points))

# remove all cols with NAs
wimbledon_2012_matches <- wimbledon_2012_matches %>%
  select(where(~ all(!is.na(.))))
names(wimbledon_2012_matches)

wimbledon_2012_points <- wimbledon_2012_points %>%
  select(where(~ all(!is.na(.))))
names(wimbledon_2012_points)

wimbledon_2012 <- left_join(wimbledon_2012_points, wimbledon_2012_matches, by = "match_id")
names(wimbledon_2012)
colSums(is.na(wimbledon_2012))

write.csv(wimbledon_2012, "../data/wimbledon_2012_combined.csv", row.names = FALSE)


#-----------------------------------------------------------------------------------------------------

## wimbledon 2013

wimbledon_2013_matches <- as.data.table(read.csv("../data/raw_data/2013-wimbledon-matches.csv"))
colSums(is.na(wimbledon_2013_matches))

wimbledon_2013_points <- as.data.table(read.csv("../data/raw_data/2013-wimbledon-points.csv"))
colSums(is.na(wimbledon_2013_points))

# remove all cols with NAs
wimbledon_2013_matches <- wimbledon_2013_matches %>%
  select(where(~ all(!is.na(.))))
names(wimbledon_2013_matches)

wimbledon_2013_points <- wimbledon_2013_points %>%
  select(where(~ all(!is.na(.))))
names(wimbledon_2013_points)

wimbledon_2013 <- left_join(wimbledon_2013_points, wimbledon_2013_matches, by = "match_id")
names(wimbledon_2013)
colSums(is.na(wimbledon_2013))

write.csv(wimbledon_2013, "../data/wimbledon_2013_combined.csv", row.names = FALSE)


#-----------------------------------------------------------------------------------------------------

## wimbledon 2014

wimbledon_2014_matches <- as.data.table(read.csv("../data/raw_data/2014-wimbledon-matches.csv"))
colSums(is.na(wimbledon_2014_matches))

wimbledon_2014_points <- as.data.table(read.csv("../data/raw_data/2014-wimbledon-points.csv"))
colSums(is.na(wimbledon_2014_points))

# remove all cols with NAs
wimbledon_2014_matches <- wimbledon_2014_matches %>%
  select(where(~ all(!is.na(.))))
names(wimbledon_2014_matches)

wimbledon_2014_points <- wimbledon_2014_points %>%
  select(where(~ all(!is.na(.))))
names(wimbledon_2014_points)

wimbledon_2014 <- left_join(wimbledon_2014_points, wimbledon_2014_matches, by = "match_id")
names(wimbledon_2014)
colSums(is.na(wimbledon_2014))


write.csv(wimbledon_2014, "../data/wimbledon_2014_combined.csv", row.names = FALSE)


#-----------------------------------------------------------------------------------------------------

## wimbledon 2015

wimbledon_2015_matches <- as.data.table(read.csv("../data/raw_data/2015-wimbledon-matches.csv"))
colSums(is.na(wimbledon_2015_matches))

wimbledon_2015_points <- as.data.table(read.csv("../data/raw_data/2015-wimbledon-points.csv"))
colSums(is.na(wimbledon_2015_points))

wimbledon_2015_matches <- wimbledon_2015_matches %>%
  select(where(~ all(!is.na(.))))
names(wimbledon_2015_matches)

# remove rows where PointWinner has NA value
wimbledon_2015_points <- wimbledon_2015_points %>%
  filter(!is.na(PointWinner))

wimbledon_2015_points <- wimbledon_2015_points %>%
  select(where(~ all(!is.na(.))))
names(wimbledon_2015_points)

wimbledon_2015 <- left_join(wimbledon_2015_points, wimbledon_2015_matches, by = "match_id")
names(wimbledon_2015)
colSums(is.na(wimbledon_2015))

write.csv(wimbledon_2015, "../data/wimbledon_2015_combined.csv", row.names = FALSE)


#-----------------------------------------------------------------------------------------------------

## wimbledon 2016

wimbledon_2016_matches <- as.data.table(read.csv("../data/raw_data/2016-wimbledon-matches.csv"))
colSums(is.na(wimbledon_2016_matches))

wimbledon_2016_points <- as.data.table(read.csv("../data/raw_data/2016-wimbledon-points.csv"))
colSums(is.na(wimbledon_2016_points))

# remove all cols with NAs
wimbledon_2016_matches <- wimbledon_2016_matches %>%
  select(where(~ all(!is.na(.))))
names(wimbledon_2016_matches)

wimbledon_2016_points <- wimbledon_2016_points %>%
  select(where(~ all(!is.na(.))))
names(wimbledon_2016_points)

wimbledon_2016 <- left_join(wimbledon_2016_points, wimbledon_2016_matches, by = "match_id")
names(wimbledon_2016)
colSums(is.na(wimbledon_2016))

write.csv(wimbledon_2016, "../data/wimbledon_2016_combined.csv", row.names = FALSE)


#-----------------------------------------------------------------------------------------------------

## wimbledon 2016

wimbledon_2016_matches <- as.data.table(read.csv("../data/raw_data/2016-wimbledon-matches.csv"))
colSums(is.na(wimbledon_2016_matches))

wimbledon_2016_points <- as.data.table(read.csv("../data/raw_data/2016-wimbledon-points.csv"))
colSums(is.na(wimbledon_2016_points))

# remove all cols with NAs
wimbledon_2016_matches <- wimbledon_2016_matches %>%
  select(where(~ all(!is.na(.))))
names(wimbledon_2016_matches)

wimbledon_2016_points <- wimbledon_2016_points %>%
  select(where(~ all(!is.na(.))))
names(wimbledon_2016_points)

wimbledon_2016 <- left_join(wimbledon_2016_points, wimbledon_2016_matches, by = "match_id")
names(wimbledon_2016)
colSums(is.na(wimbledon_2016))

write.csv(wimbledon_2016, "../data/wimbledon_2016_combined.csv", row.names = FALSE)

#-----------------------------------------------------------------------------------------------------

## wimbledon 2017

wimbledon_2017_matches <- as.data.table(read.csv("../data/raw_data/2017-wimbledon-matches.csv"))
colSums(is.na(wimbledon_2017_matches))

wimbledon_2017_points <- as.data.table(read.csv("../data/raw_data/2017-wimbledon-points.csv"))
colSums(is.na(wimbledon_2017_points))

# remove all cols with NAs
wimbledon_2017_matches <- wimbledon_2017_matches %>%
  select(where(~ all(!is.na(.))))
names(wimbledon_2017_matches)

wimbledon_2017_points <- wimbledon_2017_points %>%
  select(where(~ all(!is.na(.))))
names(wimbledon_2017_points)

wimbledon_2017 <- left_join(wimbledon_2017_points, wimbledon_2017_matches, by = "match_id")
names(wimbledon_2017)
colSums(is.na(wimbledon_2017))

write.csv(wimbledon_2017, "../data/wimbledon_2017_combined.csv", row.names = FALSE)

#-----------------------------------------------------------------------------------------------------

## wimbledon 2018

wimbledon_2018_matches <- as.data.table(read.csv("../data/raw_data/2018-wimbledon-matches.csv"))
colSums(is.na(wimbledon_2018_matches))

wimbledon_2018_points <- as.data.table(read.csv("../data/raw_data/2018-wimbledon-points.csv"))
colSums(is.na(wimbledon_2018_points))

# remove all cols with NAs
wimbledon_2018_matches <- wimbledon_2018_matches %>%
  select(where(~ all(!is.na(.))))
names(wimbledon_2018_matches)

wimbledon_2018_points <- wimbledon_2018_points %>%
  select(where(~ all(!is.na(.))))
names(wimbledon_2018_points)

wimbledon_2018 <- left_join(wimbledon_2018_points, wimbledon_2018_matches, by = "match_id")
names(wimbledon_2018)
colSums(is.na(wimbledon_2018))

# print number of rows where there is no double fault, but Speed_MPH == 0
print(nrow(wimbledon_2018 %>% filter(P1DoubleFault == 0 & P2DoubleFault == 0 & Speed_MPH == 0)))
# get rid of rows where there is no double fault, but Speed_MPH == 0
wimbledon_2018 <- wimbledon_2018 %>%
  filter(!(P1DoubleFault == 0 & P2DoubleFault == 0 & Speed_MPH == 0)) %>% 
  mutate(serving_player_won = ifelse((PointServer == 1 & PointWinner == 1) | (PointServer == 2 & PointWinner == 2), 1, 0))

write.csv(wimbledon_2018, "out_data/wimbledon_2018_combined.csv", row.names = FALSE)

#-----------------------------------------------------------------------------------------------------

## wimbledon 2019

wimbledon_2019_matches <- as.data.table(read.csv("../data/raw_data/2019-wimbledon-matches.csv"))
# names(wimbledon_2019_matches)
colSums(is.na(wimbledon_2019_matches))

wimbledon_2019_points <- as.data.table(read.csv("../data/raw_data/2019-wimbledon-points.csv"))
# names(wimbledon_2019_points)
colSums(is.na(wimbledon_2019_points))
# find how many columns in wimbledon_2019_points have P1DoubleFault == 1 or P2DoubleFault == 2
nrow(wimbledon_2019_points %>% filter(P1DoubleFault == 1 | P2DoubleFault == 1))

# remove all cols with NAs
wimbledon_2019_matches <- wimbledon_2019_matches %>%
  select(where(~ all(!is.na(.))))
names(wimbledon_2019_matches)

wimbledon_2019_points <- wimbledon_2019_points %>%
  select(where(~ all(!is.na(.))))
names(wimbledon_2019_points)

wimbledon_2019 <- left_join(wimbledon_2019_points, wimbledon_2019_matches, by = "match_id")
names(wimbledon_2019)
colSums(is.na(wimbledon_2019))

# get rid of rows where there is no double fault, but Speed_MPH == 0
wimbledon_2019 <- wimbledon_2019 %>%
  filter(!(P1DoubleFault == 0 & P2DoubleFault == 0 & Speed_MPH == 0)) %>% 
  mutate(serving_player_won = ifelse((PointServer == 1 & PointWinner == 1) | (PointServer == 2 & PointWinner == 2), 1, 0))

write.csv(wimbledon_2019, "out_data/wimbledon_2019_combined.csv", row.names = FALSE)

#-----------------------------------------------------------------------------------------------------

## wimbledon 2021

wimbledon_2021_matches <- as.data.table(read.csv("../data/raw_data/2021-wimbledon-matches.csv"))
# names(wimbledon_2021_matches)
colSums(is.na(wimbledon_2021_matches))

wimbledon_2021_points <- as.data.table(read.csv("../data/raw_data/2021-wimbledon-points.csv"))
# names(wimbledon_2021_points)
colSums(is.na(wimbledon_2021_points))

# remove all cols with NAs
wimbledon_2021_matches <- wimbledon_2021_matches %>%
  select(where(~ all(!is.na(.))))
names(wimbledon_2021_matches)

wimbledon_2021_points <- wimbledon_2021_points %>%
  select(where(~ all(!is.na(.))))
names(wimbledon_2021_points)

wimbledon_2021 <- left_join(wimbledon_2021_points, wimbledon_2021_matches, by = "match_id")
names(wimbledon_2021)
colSums(is.na(wimbledon_2021))

# wimbledon_2021 <- wimbledon_2021 %>%
#   filter(PointServer != 0) %>%
  # mutate(serving_player_won = ifelse((PointServer == 1 & PointWinner == 1) | (PointServer == 2 & PointWinner == 2), 1, 0))

# get rid of rows where there is no double fault, but Speed_MPH == 0
wimbledon_2021 <- wimbledon_2021 %>%
  filter(!(P1DoubleFault == 0 & P2DoubleFault == 0 & Speed_MPH == 0)) %>% 
  mutate(serving_player_won = ifelse((PointServer == 1 & PointWinner == 1) | (PointServer == 2 & PointWinner == 2), 1, 0))

# binary variable: whether serving player won or lost
# wimbledon_2021 <- wimbledon_2021 %>%
  # filter(PointServer != 0, Speed_KMH != 0, Speed_MPH != 0) %>% 
  # mutate(serving_player_won = ifelse((PointServer == 1 & PointWinner == 1) | (PointServer == 2 & PointWinner == 2), 1, 0))


write.csv(wimbledon_2021, "out_data/wimbledon_2021_combined.csv", row.names = FALSE)

#-----------------------------------------------------------------------------------------------------

## wimbledon 2022

wimbledon_2022_matches <- as.data.table(read.csv("../data/raw_data/2022-wimbledon-matches.csv"))
# names(wimbledon_2022_matches)
colSums(is.na(wimbledon_2022_matches))

wimbledon_2022_points <- as.data.table(read.csv("../data/raw_data/2022-wimbledon-points.csv"))
# names(wimbledon_2022_points)
colSums(is.na(wimbledon_2022_points))

# remove all cols with NAs
wimbledon_2022_matches <- wimbledon_2022_matches %>%
  select(where(~ all(!is.na(.))))
names(wimbledon_2022_matches)

wimbledon_2022_points <- wimbledon_2022_points %>%
  select(where(~ all(!is.na(.))))
names(wimbledon_2022_points)

wimbledon_2022 <- left_join(wimbledon_2022_points, wimbledon_2022_matches, by = "match_id")
names(wimbledon_2022)
colSums(is.na(wimbledon_2022))

# wimbledon_2022 <- wimbledon_2022 %>%
#   filter(PointServer != 0) %>%
#   mutate(serving_player_won = ifelse((PointServer == 1 & PointWinner == 1) | (PointServer == 2 & PointWinner == 2), 1, 0))

# get rid of rows where there is no double fault, but Speed_MPH == 0
wimbledon_2022 <- wimbledon_2022 %>%
  filter(!(P1DoubleFault == 0 & P2DoubleFault == 0 & Speed_MPH == 0)) %>% 
  mutate(serving_player_won = ifelse((PointServer == 1 & PointWinner == 1) | (PointServer == 2 & PointWinner == 2), 1, 0))

# binary variable: whether serving player won or lost
# wimbledon_2022 <- wimbledon_2022 %>%
#   filter(PointServer != 0, Speed_KMH != 0, Speed_MPH != 0) %>% 
#   mutate(serving_player_won = ifelse((PointServer == 1 & PointWinner == 1) | (PointServer == 2 & PointWinner == 2), 1, 0))


write.csv(wimbledon_2022, "out_data/wimbledon_2022_combined.csv", row.names = FALSE)

#-----------------------------------------------------------------------------------------------------

## wimbledon 2023

wimbledon_2023_matches <- as.data.table(read.csv("../data/raw_data/2023-wimbledon-matches.csv"))
# names(wimbledon_2023_matches)
colSums(is.na(wimbledon_2023_matches))

wimbledon_2023_points <- as.data.table(read.csv("../data/raw_data/2023-wimbledon-points.csv"))
# names(wimbledon_2023_points)
colSums(is.na(wimbledon_2023_points))

# remove all cols with NAs
wimbledon_2023_matches <- wimbledon_2023_matches %>%
  select(where(~ all(!is.na(.))))
names(wimbledon_2023_matches)

wimbledon_2023_points <- wimbledon_2023_points %>%
  select(where(~ all(!is.na(.))))
names(wimbledon_2023_points)

wimbledon_2023 <- left_join(wimbledon_2023_points, wimbledon_2023_matches, by = "match_id")
names(wimbledon_2023)
colSums(is.na(wimbledon_2023))

# wimbledon_2023 <- wimbledon_2023 %>%
#   filter(PointServer != 0) %>%
#   mutate(serving_player_won = ifelse((PointServer == 1 & PointWinner == 1) | (PointServer == 2 & PointWinner == 2), 1, 0))

# get rid of rows where there is no double fault, but Speed_MPH == 0
wimbledon_2023 <- wimbledon_2023 %>%
  filter(!(P1DoubleFault == 0 & P2DoubleFault == 0 & Speed_MPH == 0)) %>% 
  mutate(serving_player_won = ifelse((PointServer == 1 & PointWinner == 1) | (PointServer == 2 & PointWinner == 2), 1, 0))

# binary variable: whether serving player won or lost
# wimbledon_2023 <- wimbledon_2023 %>%
#   filter(PointServer != 0, Speed_KMH != 0, Speed_MPH != 0) %>% 
#   mutate(serving_player_won = ifelse((PointServer == 1 & PointWinner == 1) | (PointServer == 2 & PointWinner == 2), 1, 0))


write.csv(wimbledon_2023, "out_data/wimbledon_2023_combined.csv", row.names = FALSE)

#-----------------------------------------------------------------------------------------------------

## wimbledon 2024

wimbledon_2024_matches <- as.data.table(read.csv("../data/raw_data/2024-wimbledon-matches.csv"))
# names(wimbledon_2024_matches)
colSums(is.na(wimbledon_2024_matches))

wimbledon_2024_points <- as.data.table(read.csv("../data/raw_data/2024-wimbledon-points.csv"))
# names(wimbledon_2024_points)
colSums(is.na(wimbledon_2024_points))

# remove all cols with NAs
wimbledon_2024_matches <- wimbledon_2024_matches %>%
  select(where(~ all(!is.na(.))))
names(wimbledon_2024_matches)

wimbledon_2024_points <- wimbledon_2024_points %>%
  select(where(~ all(!is.na(.))))
names(wimbledon_2024_points)

wimbledon_2024 <- left_join(wimbledon_2024_points, wimbledon_2024_matches, by = "match_id")
names(wimbledon_2024)
colSums(is.na(wimbledon_2024))

# wimbledon_2024 <- wimbledon_2024 %>%
#   filter(PointServer != 0) %>%
#   mutate(serving_player_won = ifelse((PointServer == 1 & PointWinner == 1) | (PointServer == 2 & PointWinner == 2), 1, 0))

# print number of rows where there is no double fault, but Speed_MPH == 0
print(nrow(wimbledon_2024 %>% filter(P1DoubleFault == 0 & P2DoubleFault == 0 & Speed_MPH == 0)))

# get rid of rows where there is no double fault, but Speed_MPH == 0
wimbledon_2024 <- wimbledon_2024 %>%
  filter(!(P1DoubleFault == 0 & P2DoubleFault == 0 & Speed_MPH == 0)) %>% 
  mutate(serving_player_won = ifelse((PointServer == 1 & PointWinner == 1) | (PointServer == 2 & PointWinner == 2), 1, 0))

# binary variable: whether serving player won or lost
# wimbledon_2024 <- wimbledon_2024 %>%
#   filter(PointServer != 0, Speed_KMH != 0, Speed_MPH != 0) %>% 
#   mutate(serving_player_won = ifelse((PointServer == 1 & PointWinner == 1) | (PointServer == 2 & PointWinner == 2), 1, 0))


write.csv(wimbledon_2024, "out_data/wimbledon_2024_combined.csv", row.names = FALSE)

#-----------------------------------------------------------------------------------------------------