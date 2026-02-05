rm(list = ls())

library(welo)
library(tidyverse)
library(data.table)

#--------------------------------------------------------------------------------
combine_years <- function(tournament, years, gender) {
  files <- paste0("data/processed/subset/", years, "_", tournament, "_", gender, ".csv")
  data_list <- lapply(files, fread)
  combined <- rbindlist(data_list, use.names = TRUE, fill = TRUE)

  combined <- combined %>%
    filter(ServeDepth != "", ServeWidth != "", !is.na(speed_ratio))

  combined
}

#--------------------------------------------------------------------------------
# Define years
years <- c(2018, 2019, 2021, 2022, 2023, 2024)

tournaments <- c("wimb", "us")
genders <- c("men", "women")
dir.create("data/processed/splits", recursive = TRUE, showWarnings = FALSE)

set.seed(123)

for (t in tournaments) {
  for (g in genders) {
    combined <- combine_years(t, years, g)

    # Match-level 80/20 split within each year
    split_by_year <- combined %>%
      group_by(year) %>%
      group_split()

    training <- list()
    testing <- list()

    for (yr_df in split_by_year) {
      match_ids <- unique(yr_df$match_id)
      train_match_ids <- sample(match_ids, size = floor(0.8 * length(match_ids)))
      training[[length(training) + 1]] <- yr_df %>% filter(match_id %in% train_match_ids)
      testing[[length(testing) + 1]]  <- yr_df %>% filter(!match_id %in% train_match_ids)
    }

    training <- bind_rows(training)
    testing <- bind_rows(testing)

    # Write outputs for downstream scripts
    fwrite(training, paste0("data/processed/splits/", t, "_", g, "_train.csv"))
    fwrite(testing,  paste0("data/processed/splits/", t, "_", g, "_test.csv"))
  }
}
