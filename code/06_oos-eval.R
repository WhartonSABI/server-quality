rm(list = ls())
library(tidyverse)
library(data.table)

tournaments <- c("wimb", "us")
genders <- c("men", "women")

tag_prefix_for <- function(tournament, gender) {
  paste0(tournament, "_", gender)
}

zscore <- function(x) (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)

run_eval_for_serve_type <- function(df_test_clean, df_sqs, st = c("first", "second")) {
  st <- match.arg(st)

  df_points <- df_test_clean %>%
    filter(ServeNumber == ifelse(st == "first", 1, 2))

  outcomes <- df_points %>%
    group_by(ServerName) %>%
    summarise(
      n_serves_test  = n(),
      wins_total     = sum(server_won, na.rm = TRUE),
      wins_rally_le3 = sum(server_won * rally_le3, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    filter(n_serves_test > 20)

  welo_baseline <- df_points %>%
    mutate(welo_value = ifelse(ServeIndicator == 1, player1_avg_welo, player2_avg_welo)) %>%
    group_by(ServerName) %>%
    summarise(welo_mean_test = mean(welo_value, na.rm = TRUE), .groups = "drop")

  sqs_col <- ifelse(st == "first", "SQS_logodds_first", "SQS_logodds_second")

  eval_df <- outcomes %>%
    left_join(welo_baseline, by = "ServerName") %>%
    left_join(df_sqs %>% select(ServerName, all_of(sqs_col)), by = "ServerName") %>%
    rename(SQS_logodds = all_of(sqs_col)) %>%
    filter(!is.na(SQS_logodds), !is.na(welo_mean_test)) %>%
    mutate(
      SQS_z  = zscore(SQS_logodds),
      welo_z = zscore(welo_mean_test),
      win_pct_test  = wins_total / n_serves_test,
      eff_test      = wins_rally_le3 / n_serves_test
    )

  m_win_sqs <- glm(cbind(wins_total, n_serves_test - wins_total) ~ SQS_z,
                   family = binomial, data = eval_df)
  m_win_welo <- glm(cbind(wins_total, n_serves_test - wins_total) ~ welo_z,
                    family = binomial, data = eval_df)

  m_eff_sqs <- glm(cbind(wins_rally_le3, n_serves_test - wins_rally_le3) ~ SQS_z,
                   family = binomial, data = eval_df)
  m_eff_welo <- glm(cbind(wins_rally_le3, n_serves_test - wins_rally_le3) ~ welo_z,
                    family = binomial, data = eval_df)

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
    mutate(serve_type = st) %>%
    left_join(cor_results, by = c("model", "term", "serve_type"))

  results
}

process_tournament_gender <- function(tournament, gender) {
  tag_prefix <- tag_prefix_for(tournament, gender)

  testing_path <- file.path("data/processed/splits", paste0(tournament, "_", gender, "_test.csv"))
  output_dir <- file.path("data/results", tag_prefix)
  evaluation_dir <- file.path(output_dir, "evaluation")
  rankings_dir <- file.path(output_dir, "rankings")
  dir.create(evaluation_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(rankings_dir, recursive = TRUE, showWarnings = FALSE)

  metrics_first_path <- file.path(rankings_dir, "first.csv")
  metrics_second_path <- file.path(rankings_dir, "second.csv")

  if (!file.exists(testing_path) || !file.exists(metrics_first_path) || !file.exists(metrics_second_path)) {
    message("Missing inputs for ", tag_prefix)
    return(invisible(NULL))
  }

  df_sqs_first <- read_csv(metrics_first_path)
  df_sqs_second <- read_csv(metrics_second_path)

  df_sqs <- full_join(
    df_sqs_first %>% select(ServerName, SQS_logodds) %>% rename(SQS_logodds_first = SQS_logodds),
    df_sqs_second %>% select(ServerName, SQS_logodds) %>% rename(SQS_logodds_second = SQS_logodds),
    by = "ServerName"
  )

  df_test <- fread(testing_path)

  df_test_clean <- df_test %>%
    filter(!is.na(ServeWidth), !is.na(ServeDepth), ServeWidth != "", ServeDepth != "") %>%
    filter(ServeNumber %in% c(1, 2)) %>%
    mutate(
      ServerName  = tolower(ifelse(ServeIndicator == 1, player1, player2)),
      server_won  = as.integer(ifelse(ServeIndicator == 1, PointWinner == 1, PointWinner == 2)),
      rally_le3   = if_else(RallyCount <= 3, 1L, 0L)
    ) %>%
    mutate(ServerName = str_to_title(ServerName))

  results_all_types <- map_dfr(c("first", "second"), ~ run_eval_for_serve_type(df_test_clean, df_sqs, .x))

  write.csv(results_all_types,
            file = file.path(evaluation_dir, "combined.csv"),
            row.names = FALSE)
}

for (t in tournaments) {
  for (g in genders) {
    process_tournament_gender(t, g)
  }
}
