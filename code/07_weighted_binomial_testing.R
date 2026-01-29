rm(list = ls())
library(tidyverse)
library(data.table)

# --- Config ---
tournament <- "wimbledon"  # "wimbledon" or "usopen"
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

# convert SQS_logodds_1st and SQS_logodds_2nd to probability scale
df_sqs <- df_sqs %>%
  mutate(
    SQS_prob_first    = 1 / (1 + exp(-SQS_logodds_1st)),
    SQS_prob_second   = 1 / (1 + exp(-SQS_logodds_2nd))
  )

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
  select(ServerName, SQS_prob_combined, SQS_prob_first, SQS_prob_second) %>%
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
  left_join(df_sqs %>% select(ServerName, SQS_prob_combined, 
                              SQS_prob_first, SQS_prob_second), by = "ServerName") %>% 
  filter(n_serves_test > 20)

names(baseline_eval)

#######################################
### weighted binomial for out of sample testing
#######################################

# combined metric, first serves only, and second serves only
sqs_col_map <- list(
  all    = "SQS_prob_combined",
  first  = "SQS_prob_first",
  second = "SQS_prob_second"
)

# Z-score helper
zscore <- function(x) (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)

# function to run evaluation for a given serve type
run_eval_for_serve_type <- function(st = c("all", "first", "second")) {
  st <- match.arg(st)
  
  # filter points for this serve type
  df_points <- df_test_clean %>%
    { if (st == "all") . else filter(., ServeNumber == ifelse(st == "first", 1, 2)) }
  
  # outcomes on this serve type
  outcomes <- df_points %>%
    group_by(ServerName) %>%
    summarise(
      n_serves_test  = n(),
      wins_total     = sum(server_won, na.rm = TRUE),
      wins_rally_le3 = sum(server_won * rally_le3, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    filter(n_serves_test > 20)
  
  # wELO baseline on this serve type
  welo_baseline <- df_points %>%
    mutate(welo_value = ifelse(ServeIndicator == 1, player1_avg_welo, player2_avg_welo)) %>%
    group_by(ServerName) %>%
    summarise(welo_mean_test = mean(welo_value, na.rm = TRUE), .groups = "drop")
  
  # choose the matching SQS column and join
  sqs_col <- sqs_col_map[[st]]
  
  eval_df <- outcomes %>%
    left_join(welo_baseline, by = "ServerName") %>%
    left_join(df_sqs %>% select(ServerName, all_of(sqs_col)), by = "ServerName") %>%
    rename(SQS_prob = all_of(sqs_col)) %>%
    filter(!is.na(SQS_prob), !is.na(welo_mean_test)) %>%
    mutate(
      SQS_z  = zscore(SQS_prob),
      welo_z = zscore(welo_mean_test)
    )
  
  eval_df <- eval_df %>%
    mutate(
      win_pct_test  = wins_total / n_serves_test,
      eff_test      = wins_rally_le3 / n_serves_test
    )
  
  # --- models ---
  # win%
  m_win_sqs <- glm(cbind(wins_total, n_serves_test - wins_total) ~ SQS_z,
                   family = binomial, data = eval_df)
  m_win_welo <- glm(cbind(wins_total, n_serves_test - wins_total) ~ welo_z,
                    family = binomial, data = eval_df)
  
  # efficiency
  m_eff_sqs <- glm(cbind(wins_rally_le3, n_serves_test - wins_rally_le3) ~ SQS_z,
                   family = binomial, data = eval_df)
  m_eff_welo <- glm(cbind(wins_rally_le3, n_serves_test - wins_rally_le3) ~ welo_z,
                    family = binomial, data = eval_df)
  
  # --- correlations ---
  cor_results <- tibble(
    model = c(
      paste0("win_sqs_", st),
      paste0("win_welo_", st),
      paste0("eff_sqs_", st),
      paste0("eff_welo_", st)
    ),
    term = c("SQS_z", "welo_z", "SQS_z", "welo_z"),
    correlation = c(
      cor(eval_df$SQS_z,  eval_df$win_pct_test, use = "complete.obs"),
      cor(eval_df$welo_z, eval_df$win_pct_test, use = "complete.obs"),
      cor(eval_df$SQS_z,  eval_df$eff_test,     use = "complete.obs"),
      cor(eval_df$welo_z, eval_df$eff_test,     use = "complete.obs")
    ),
    serve_type = st
  )
  
  # --- tidy coefficient extraction ---
  extract_coefs <- function(model, model_name) {
    cf <- as.data.frame(summary(model)$coefficients)
    cf$term <- rownames(cf)
    rownames(cf) <- NULL
    cf$model <- model_name
    cf
  }
  
  results <- bind_rows(
    extract_coefs(m_win_sqs,  paste0("win_sqs_",  st)),
    extract_coefs(m_win_welo, paste0("win_welo_", st)),
    extract_coefs(m_eff_sqs,  paste0("eff_sqs_",  st)),
    extract_coefs(m_eff_welo, paste0("eff_welo_", st))
  ) %>%
    mutate(serve_type = st)
  
  results <- results %>%
    left_join(cor_results, by = c("model", "term", "serve_type"))
  
  return(results)
}

# Run all three: combined(all), first-only, second-only
results_all_types <- map_dfr(c("all", "first", "second"), run_eval_for_serve_type)

# Save to CSV
write.csv(results_all_types, 
          file = file.path(output_dir, paste0(tag_prefix, "_weighted_binomial_results_by_serve_type.csv")),
          row.names = FALSE
)
