rm(list=ls())
# install.packages("welo")
library(welo)
library(tidyverse)
library(data.table)

#-----------------------------------------------------------------------------------------------------

subset_2021_m <- as.data.table(read.csv("out_data/wimbledon_subset_2021_m.csv"))
subset_2021_f <- as.data.table(read.csv("out_data/wimbledon_subset_2021_f.csv"))

subset_2022_m <- as.data.table(read.csv("out_data/wimbledon_subset_2022_m.csv"))
subset_2022_f <- as.data.table(read.csv("out_data/wimbledon_subset_2022_f.csv"))

subset_2023_m <- as.data.table(read.csv("out_data/wimbledon_subset_2023_m.csv"))
subset_2023_f <- as.data.table(read.csv("out_data/wimbledon_subset_2023_f.csv"))

subset_2024_m <- as.data.table(read.csv("out_data/wimbledon_subset_2024_m.csv"))
subset_2024_f <- as.data.table(read.csv("out_data/wimbledon_subset_2024_f.csv"))

#-----------------------------------------------------------------------------------------------------

# combine male data
subset_m <- rbindlist(list(
  subset_2021_m,
  subset_2022_m,
  subset_2023_m,
  subset_2024_m
))

subset_m <- subset_m %>%
  filter(ServeDepth != "", ServeWidth != "") %>% 
  filter(!is.na(speed_ratio))

write.csv(subset_m, "out_data/wimbledon_subset_m.csv")

# combine female data
subset_f <- rbindlist(list(
  subset_2021_f,
  subset_2022_f,
  subset_2023_f,
  subset_2024_f
))

subset_f <- subset_f %>%
  filter(ServeDepth != "", ServeWidth != "")%>% 
  filter(!is.na(speed_ratio))

write.csv(subset_f, "out_data/wimbledon_subset_f.csv")

