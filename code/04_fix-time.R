# Added by the 2026-08-27 inactive-project compression migration.
.codex_write_csv_gz <- function(x, file = "", ...) {
  if (is.character(file) && length(file) == 1L && grepl("\\.gz$", file, ignore.case = TRUE)) {
    con <- gzfile(file, open = "wt")
    on.exit(close(con), add = TRUE)
    utils::write.csv(x, file = con, ...)
  } else {
    utils::write.csv(x, file = file, ...)
  }
}

rm(list = ls())
library(tidyverse)
library(data.table)
library(hms)

#-----------------------------------------------------------------------------------------------------
# Optional: fix large gaps in ElapsedSeconds after splitting.

tournaments <- c("wimb", "us")
genders <- c("men", "women")
splits <- c("train", "test")
dir.create("data/processed/splits", recursive = TRUE, showWarnings = FALSE)

fix_elapsed_time <- function(df) {
  df <- df %>%
    arrange(match_id, PointNumber) %>%
    group_by(match_id) %>%
    mutate(
      lag_elapsed = lag(ElapsedSeconds),
      time_diff = ElapsedSeconds - lag_elapsed
    )

  avg_diffs <- df %>%
    filter(!is.na(time_diff) & time_diff <= 1200) %>%
    group_by(match_id) %>%
    summarise(avg_time_diff = mean(time_diff, na.rm = TRUE), .groups = "drop")

  df <- df %>%
    left_join(avg_diffs, by = "match_id") %>%
    arrange(match_id, PointNumber) %>%
    mutate(
      ElapsedSeconds_fixed = ElapsedSeconds,
      flagged = time_diff > 1200
    ) %>%
    ungroup()

  for (i in 2:nrow(df)) {
    if (df$match_id[i] == df$match_id[i - 1] &&
        (df$flagged[i] || df$ElapsedSeconds_fixed[i] - df$ElapsedSeconds_fixed[i - 1] > 1200)) {
      df$ElapsedSeconds_fixed[i] <- df$ElapsedSeconds_fixed[i - 1] + df$avg_time_diff[i]
      df$flagged[i] <- TRUE
    }
  }

  df %>%
    select(-lag_elapsed, -time_diff, -avg_time_diff, -flagged) %>%
    filter(!(ElapsedSeconds_fixed < 0))
}

process_split <- function(tournament, gender, split) {
  subset_input_path <- file.path(
    "data/processed/splits",
    paste0(tournament, "_", gender, "_", split, ".csv.gz")
  )
  subset_output_path <- file.path(
    "data/processed/splits",
    paste0(tournament, "_", gender, "_", split, ".csv.gz")
  )
  final_output_path <- file.path(
    "data/processed/splits",
    paste0(tournament, "_", gender, "_", split, ".csv.gz")
  )

  if (!file.exists(subset_input_path)) {
    message("Missing input: ", subset_input_path)
    return(invisible(NULL))
  }

  df <- as.data.table(read.csv(subset_input_path))
  df <- fix_elapsed_time(df)
  .codex_write_csv_gz(df, subset_output_path, row.names = FALSE)
  .codex_write_csv_gz(df, final_output_path, row.names = FALSE)
}

for (t in tournaments) {
  for (g in genders) {
    for (s in splits) {
      process_split(t, g, s)
    }
  }
}
