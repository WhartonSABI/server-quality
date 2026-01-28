rm(list = ls())
library(tidyverse)
library(data.table)

# --- Config ---
tournament <- "usopen"  # "wimbledon" or "usopen"
gender <- "m"              # "m" or "f"
tag_prefix <- paste0(tournament, "_", ifelse(gender == "m", "males", "females"))

# --- Paths ---
training_path <- file.path("../data/processed/scaled", paste0(tournament, "_", gender, "_training.csv"))
testing_path <- file.path("../data/processed/scaled", paste0(tournament, "_", gender, "_testing.csv"))

output_dir <- file.path("../data/results", tag_prefix)
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

# --- Load Data ---
path <- file.path(output_dir, paste0(tag_prefix, "_metrics.csv"))
df_sqs <- read_csv(path)
names(df_sqs)

df_test <- fread(testing_path)
names(df_test)


#######################################
### clean test data and join with sqs
#######################################

df_test_clean <- df_test %>%
  filter(!is.na(ServeWidth), !is.na(ServeDepth), ServeWidth != "", ServeDepth != "") %>%
  filter(ServeNumber %in% c(1, 2)) %>%
  mutate(
    ServerName  = tolower(ifelse(ServeIndicator == 1, player1, player2)),
    server_won  = as.integer(ifelse(ServeIndicator == 1, PointWinner == 1, PointWinner == 2)),
    rally_le3   = if_else(RallyCount <= 3, 1L, 0L)
  )

# make ServerName capitalized for joining (initially lowercased to prevent differenecs in capitalizing middle names, etc.)
df_test_clean <- df_test_clean %>%
  mutate(ServerName = str_to_title(ServerName))

# --- Aggregate test outcomes per server (all serves combined) ---
test_outcomes <- df_test_clean %>%
  group_by(ServerName) %>%
  summarise(
    n_serves_test         = n(),
    wins_total            = sum(server_won, na.rm = TRUE),
    wins_rally_le3        = sum(server_won * rally_le3, na.rm = TRUE),
    win_pct_test          = wins_total / n_serves_test,
    serve_efficiency_test = wins_rally_le3 / n_serves_test,
    .groups = "drop"
  )

# --- Join training-based predictions (SQS_prob_combined) to test outcomes ---
eval_df <- df_sqs %>%
  select(ServerName, SQS_prob_combined) %>%
  inner_join(test_outcomes, by = "ServerName") %>%
  filter(n_serves_test > 20)

names(eval_df)

# --- add welo to test set ---
welo_baseline <- df_test_clean %>%
  mutate(
    welo_value = ifelse(ServeIndicator == 1, player1_avg_welo, player2_avg_welo)
  ) %>%
  group_by(ServerName) %>%
  summarise(
    welo_mean_test = mean(welo_value, na.rm = TRUE),
    .groups = "drop"
  )

# merge baseline (welo) with test outcomes
baseline_eval <- test_outcomes %>%
  left_join(welo_baseline,  by = "ServerName") %>%
  left_join(df_sqs %>% select(ServerName, SQS_prob_combined), by = "ServerName") %>% 
  filter(n_serves_test > 20)

# --- Z-score standardization helper ---
zscore <- function(x) {
  if (is.numeric(x)) (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE) else x
}

# --- Standardize everything (within baseline_eval) ---
baseline_eval_std <- baseline_eval %>%
  mutate(
    SQS_prob_combined_z  = zscore(SQS_prob_combined),
    welo_mean_test_z     = zscore(welo_mean_test),
    serve_efficiency_z   = zscore(serve_efficiency_test),
    win_pct_z            = zscore(win_pct_test)
  )

names(baseline_eval_std)
