rm(list = ls())
library(tidyverse)
library(data.table)
library(ggrepel)
library(ggplot2)
library(scales)

#REGRESSION
lm1 <- lm(avg_speed ~ importance, data = importance_summary)
lm2 <- lm(sd_speed ~ importance, data = importance_summary)
lm3 <- lm(ace_pct ~ importance, data = importance_summary)
lm4 <- lm(location_entropy ~ importance, data = importance_summary)

sink("../data/results/importance_analysis/linear_model_summaries.txt")
cat("Regression Summaries:\n\n")
summary(lm1)
summary(lm2)
summary(lm3)
summary(lm4)
sink()

#Importance Frequency
modal_counts <- df_clean %>%
  group_by(importance, location_bin) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(importance) %>%
  mutate(prop = n / sum(n)) %>%
  pivot_wider(names_from = location_bin, values_from = prop, values_fill = 0)

write.csv(modal_counts, "../data/results/importance_analysis/modal_location_props.csv", row.names = FALSE)
