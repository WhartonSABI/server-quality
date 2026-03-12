rm(list = ls())
library(tidyverse)
library(lme4)

tournaments <- c("wimb", "us")
genders <- c("men", "women")

tag_prefix_for <- function(tournament, gender) {
  paste0(tournament, "_", gender)
}

safe_zscore <- function(x) {
  mu <- mean(x, na.rm = TRUE)
  sig <- sd(x, na.rm = TRUE)
  if (!is.finite(sig) || sig == 0) {
    return(rep(NA_real_, length(x)))
  }
  (x - mu) / sig
}

clean_points <- function(df) {
  df %>%
    filter(!is.na(ServeWidth), !is.na(ServeDepth), ServeWidth != "", ServeDepth != "") %>%
    filter(ServeNumber %in% c(1, 2)) %>%
    mutate(
      ServerName = tolower(ifelse(ServeIndicator == 1, player1, player2)),
      ReturnerName = tolower(ifelse(ServeIndicator == 1, player2, player1)),
      server_won = as.integer(ifelse(ServeIndicator == 1, PointWinner == 1, PointWinner == 2)),
      is_ace = ifelse(ServeIndicator == 1, P1Ace, P2Ace),
      rally_le1 = if_else(RallyCount <= 1, 1L, 0L),
      rally_le3 = if_else(RallyCount <= 3, 1L, 0L),
      is_efficient = as.integer(server_won & (RallyCount <= 3))
    ) %>%
    mutate(
      ServerName = str_to_title(ServerName),
      ReturnerName = str_to_title(ReturnerName)
    )
}

compute_standard_serve_stats <- function(df_train_clean, serve_num) {
  serve_specific <- df_train_clean %>%
    filter(ServeNumber == serve_num) %>%
    group_by(ServerName) %>%
    summarise(
      n_serves_train_type = n(),
      ace_rate_train = mean(is_ace, na.rm = TRUE),
      # Proxy for "unreturned serve rate" using one-shot rallies
      unreturned_rate_train = mean(rally_le1, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    filter(n_serves_train_type > 20)

  overall <- df_train_clean %>%
    group_by(ServerName) %>%
    summarise(
      first_serve_in_pct_train = mean(ServeNumber == 1, na.rm = TRUE),
      first_serve_points_won_train = ifelse(
        sum(ServeNumber == 1, na.rm = TRUE) > 0,
        sum(server_won * (ServeNumber == 1), na.rm = TRUE) / sum(ServeNumber == 1, na.rm = TRUE),
        NA_real_
      ),
      .groups = "drop"
    )

  serve_specific %>%
    left_join(overall, by = "ServerName")
}

fit_random_effects_only <- function(df_train_clean, serve_num) {
  df_model <- df_train_clean %>%
    filter(ServeNumber == serve_num) %>%
    group_by(ServerName) %>%
    mutate(n_serves_train_type = n()) %>%
    ungroup() %>%
    filter(n_serves_train_type > 20) %>%
    select(is_efficient, ServerName, ReturnerName)

  if (nrow(df_model) == 0) {
    return(tibble(ServerName = character(), re_only_logodds = numeric()))
  }

  fit <- tryCatch(
    suppressWarnings(
      glmer(
        is_efficient ~ (1 | ServerName) + (1 | ReturnerName),
        data = df_model,
        family = binomial(),
        control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
      )
    ),
    error = function(e) NULL
  )

  if (is.null(fit)) {
    return(tibble(ServerName = character(), re_only_logodds = numeric()))
  }

  re <- ranef(fit)$ServerName
  tibble(
    ServerName = rownames(re),
    re_only_logodds = as.numeric(re[, "(Intercept)"])
  )
}

fit_single_predictor <- function(eval_df, success_col, total_col, predictor_col,
                                 predictor_label, predictor_term, serve_type, outcome_label) {
  model_df <- eval_df %>%
    transmute(
      successes = .data[[success_col]],
      total = .data[[total_col]],
      predictor_raw = .data[[predictor_col]]
    ) %>%
    mutate(predictor_z = safe_zscore(predictor_raw)) %>%
    filter(is.finite(successes), is.finite(total), total > 0, is.finite(predictor_z))

  n_servers <- nrow(model_df)
  model_name <- paste0(outcome_label, "_", predictor_label, "_", serve_type)

  if (n_servers < 3) {
    return(tibble(
      `Estimate` = NA_real_,
      `Std. Error` = NA_real_,
      `z value` = NA_real_,
      `Pr(>|z|)` = NA_real_,
      term = predictor_term,
      model = model_name,
      predictor = predictor_label,
      outcome = outcome_label,
      serve_type = serve_type,
      n_servers = n_servers,
      correlation = NA_real_
    ))
  }

  fit <- glm(
    cbind(successes, total - successes) ~ predictor_z,
    family = binomial(),
    data = model_df
  )

  cf <- as.data.frame(summary(fit)$coefficients)
  cf$term <- rownames(cf)
  rownames(cf) <- NULL
  cf$term <- ifelse(cf$term == "predictor_z", predictor_term, cf$term)

  outcome_rate <- model_df$successes / model_df$total
  corr_val <- cor(model_df$predictor_z, outcome_rate, use = "complete.obs")

  cf %>%
    mutate(
      model = model_name,
      predictor = predictor_label,
      outcome = outcome_label,
      serve_type = serve_type,
      n_servers = n_servers,
      correlation = if_else(term == predictor_term, corr_val, NA_real_)
    )
}

run_eval_for_serve_type <- function(df_test_clean, df_sqs, standard_stats, re_only_scores,
                                    fixed_only_scores, st = c("first", "second")) {
  st <- match.arg(st)
  serve_num <- ifelse(st == "first", 1, 2)
  sqs_col <- ifelse(st == "first", "SQS_logodds_first", "SQS_logodds_second")

  df_points <- df_test_clean %>%
    filter(ServeNumber == serve_num)

  outcomes <- df_points %>%
    group_by(ServerName) %>%
    summarise(
      n_serves_test = n(),
      wins_total = sum(server_won, na.rm = TRUE),
      wins_rally_le3 = sum(server_won * rally_le3, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    filter(n_serves_test > 20)

  welo_baseline <- df_points %>%
    mutate(welo_value = ifelse(ServeIndicator == 1, player1_avg_welo, player2_avg_welo)) %>%
    group_by(ServerName) %>%
    summarise(welo_mean_test = mean(welo_value, na.rm = TRUE), .groups = "drop")

  eval_df <- outcomes %>%
    left_join(welo_baseline, by = "ServerName") %>%
    left_join(df_sqs %>% select(ServerName, all_of(sqs_col)), by = "ServerName") %>%
    rename(SQS_logodds = all_of(sqs_col)) %>%
    left_join(standard_stats, by = "ServerName") %>%
    left_join(re_only_scores, by = "ServerName") %>%
    left_join(fixed_only_scores, by = "ServerName")

  required_predictors <- c(
    "SQS_logodds", "welo_mean_test", "ace_rate_train", "unreturned_rate_train",
    "first_serve_points_won_train", "first_serve_in_pct_train",
    "re_only_logodds", "fixed_only_logodds"
  )
  eval_df <- eval_df %>%
    filter(if_all(all_of(required_predictors), ~ is.finite(.x)))

  predictor_specs <- tribble(
    ~predictor_col, ~predictor_label, ~predictor_term,
    "SQS_logodds", "sqs", "SQS_z",
    "welo_mean_test", "welo", "welo_z",
    "ace_rate_train", "ace_rate", "ace_rate_z",
    "unreturned_rate_train", "unreturned_rate", "unreturned_rate_z",
    "first_serve_points_won_train", "first_serve_points_won", "first_serve_points_won_z",
    "first_serve_in_pct_train", "first_serve_in_pct", "first_serve_in_pct_z",
    "re_only_logodds", "random_effects_only", "random_effects_only_z",
    "fixed_only_logodds", "fixed_effects_only", "fixed_effects_only_z"
  )

  results_win <- pmap_dfr(
    predictor_specs,
    function(predictor_col, predictor_label, predictor_term) {
      fit_single_predictor(
        eval_df = eval_df,
        success_col = "wins_total",
        total_col = "n_serves_test",
        predictor_col = predictor_col,
        predictor_label = predictor_label,
        predictor_term = predictor_term,
        serve_type = st,
        outcome_label = "win"
      )
    }
  )

  results_eff <- pmap_dfr(
    predictor_specs,
    function(predictor_col, predictor_label, predictor_term) {
      fit_single_predictor(
        eval_df = eval_df,
        success_col = "wins_rally_le3",
        total_col = "n_serves_test",
        predictor_col = predictor_col,
        predictor_label = predictor_label,
        predictor_term = predictor_term,
        serve_type = st,
        outcome_label = "eff"
      )
    }
  )

  bind_rows(results_win, results_eff)
}

process_tournament_gender <- function(tournament, gender) {
  tag_prefix <- tag_prefix_for(tournament, gender)

  training_path <- file.path("data/processed/splits", paste0(tournament, "_", gender, "_train.csv"))
  testing_path <- file.path("data/processed/splits", paste0(tournament, "_", gender, "_test.csv"))
  output_dir <- file.path("data/results", tag_prefix)
  evaluation_dir <- file.path(output_dir, "evaluation")
  rankings_dir <- file.path(output_dir, "rankings")
  dir.create(evaluation_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(rankings_dir, recursive = TRUE, showWarnings = FALSE)

  metrics_first_path <- file.path(rankings_dir, "first.csv")
  metrics_second_path <- file.path(rankings_dir, "second.csv")

  if (!file.exists(training_path) ||
      !file.exists(testing_path) ||
      !file.exists(metrics_first_path) ||
      !file.exists(metrics_second_path)) {
    message("Missing inputs for ", tag_prefix)
    return(invisible(NULL))
  }

  df_sqs_first <- read_csv(metrics_first_path, show_col_types = FALSE)
  df_sqs_second <- read_csv(metrics_second_path, show_col_types = FALSE)

  df_sqs <- full_join(
    df_sqs_first %>% select(ServerName, SQS_logodds) %>% rename(SQS_logodds_first = SQS_logodds),
    df_sqs_second %>% select(ServerName, SQS_logodds) %>% rename(SQS_logodds_second = SQS_logodds),
    by = "ServerName"
  )

  fixed_only_first <- df_sqs_first %>%
    select(ServerName, fixed_only_logodds = MeasuredSkill)
  fixed_only_second <- df_sqs_second %>%
    select(ServerName, fixed_only_logodds = MeasuredSkill)

  df_train_clean <- clean_points(read_csv(training_path, show_col_types = FALSE))
  df_test_clean <- clean_points(read_csv(testing_path, show_col_types = FALSE))

  stats_first <- compute_standard_serve_stats(df_train_clean, serve_num = 1)
  stats_second <- compute_standard_serve_stats(df_train_clean, serve_num = 2)
  re_only_first <- fit_random_effects_only(df_train_clean, serve_num = 1)
  re_only_second <- fit_random_effects_only(df_train_clean, serve_num = 2)

  results_first <- run_eval_for_serve_type(
    df_test_clean = df_test_clean,
    df_sqs = df_sqs,
    standard_stats = stats_first,
    re_only_scores = re_only_first,
    fixed_only_scores = fixed_only_first,
    st = "first"
  )

  results_second <- run_eval_for_serve_type(
    df_test_clean = df_test_clean,
    df_sqs = df_sqs,
    standard_stats = stats_second,
    re_only_scores = re_only_second,
    fixed_only_scores = fixed_only_second,
    st = "second"
  )

  results_all_types <- bind_rows(results_first, results_second)

  write.csv(
    results_all_types,
    file = file.path(evaluation_dir, "combined.csv"),
    row.names = FALSE
  )
}

for (t in tournaments) {
  for (g in genders) {
    process_tournament_gender(t, g)
  }
}
