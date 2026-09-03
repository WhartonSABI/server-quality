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
library(lme4)

train_years <- c(2018, 2019, 2021, 2022)
test_years  <- c(2023, 2024)

tournaments <- c("wimb", "us")
genders     <- c("men", "women")

#-------------------------------------------------------------------------------
# Helpers (inlined from 03/05/06)

combine_years <- function(tournament, years, gender) {
  files <- paste0("data/processed/subset/", years, "_", tournament, "_", gender, ".csv.gz")
  files <- files[file.exists(files)]
  if (length(files) == 0) return(NULL)
  combined <- map_dfr(files, ~ read_csv(.x, show_col_types = FALSE))
  combined %>%
    filter(ServeDepth != "", ServeWidth != "", !is.na(speed_ratio))
}

compute_entropy <- function(x) {
  p <- prop.table(table(x))
  -sum(p * log2(p))
}

get_mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

get_serve_profiles <- function(df, serve_number_label) {
  df %>%
    filter(ServeNumber %in% serve_number_label) %>%
    group_by(ServerName) %>%
    summarise(
      avg_speed = mean(Speed_MPH, na.rm = TRUE),
      sd_speed = sd(Speed_MPH, na.rm = TRUE),
      ace_pct = mean(is_ace, na.rm = TRUE),
      location_entropy = compute_entropy(location_bin),
      modal_location = get_mode(location_bin),
      n_serves = n(),
      .groups = "drop"
    ) %>%
    filter(n_serves > 20)
}

scale_cols <- function(d) {
  num_cols <- c("avg_speed", "sd_speed", "location_entropy")
  mu  <- sapply(d[num_cols], mean, na.rm = TRUE)
  sig <- sapply(d[num_cols], sd,   na.rm = TRUE)
  d %>%
    mutate(
      avg_speed_z        = (avg_speed        - mu["avg_speed"])        / sig["avg_speed"],
      sd_speed_z         = (sd_speed         - mu["sd_speed"])         / sig["sd_speed"],
      location_entropy_z = (location_entropy - mu["location_entropy"]) / sig["location_entropy"],
      modal_location     = factor(modal_location)
    )
}

build_sqs <- function(model, profiles_z) {
  b  <- fixef(model)
  re <- ranef(model)$ServerName
  u  <- re[, "(Intercept)"]
  names(u) <- rownames(re)

  mm   <- model.matrix(
    ~ avg_speed_z + sd_speed_z + location_entropy_z + modal_location,
    data = profiles_z
  )
  cols <- intersect(colnames(mm), names(b))
  mm   <- mm[, cols, drop = FALSE]
  bvec <- b[cols]

  measured <- as.numeric(mm %*% bvec)
  u_vec <- u[profiles_z$ServerName]
  u_vec[is.na(u_vec)] <- 0

  tibble(
    ServerName      = profiles_z$ServerName,
    SQS_logodds     = measured + u_vec,
    MeasuredSkill   = measured,
    UnmeasuredCraft = u_vec
  )
}

safe_zscore <- function(x) {
  mu <- mean(x, na.rm = TRUE)
  sig <- sd(x, na.rm = TRUE)
  if (!is.finite(sig) || sig == 0) {
    return(rep(NA_real_, length(x)))
  }
  (x - mu) / sig
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

#-------------------------------------------------------------------------------
# Evaluation (mirrors 06_oos-eval.R)

run_temporal_eval <- function(df_test_clean, df_sqs, standard_stats, re_only_scores,
                              fixed_only_scores, st = c("first", "second")) {
  st <- match.arg(st)
  serve_num <- ifelse(st == "first", 1, 2)

  df_points <- df_test_clean %>% filter(ServeNumber == serve_num)

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

#-------------------------------------------------------------------------------
# Main loop

process_temporal <- function(tournament, gender) {
  tag <- paste0(tournament, "_", gender)
  message("=== Temporal validation: ", tag, " ===")

  train_data <- combine_years(tournament, train_years, gender)
  test_data  <- combine_years(tournament, test_years,  gender)

  if (is.null(train_data) || nrow(train_data) == 0) {
    message("  No training data; skipping."); return(invisible(NULL))
  }
  if (is.null(test_data) || nrow(test_data) == 0) {
    message("  No test data; skipping."); return(invisible(NULL))
  }

  message("  Train points: ", nrow(train_data), "  Test points: ", nrow(test_data))

  # --- Clean training data ---
  df_clean <- train_data %>%
    filter(!is.na(ServeWidth), !is.na(ServeDepth), ServeWidth != "", ServeDepth != "") %>%
    filter(ServeNumber %in% c(1, 2)) %>%
    mutate(
      location_bin = paste0("W", ServeWidth, "_D", ServeDepth),
      ServerName   = tolower(ifelse(ServeIndicator == 1, player1, player2)),
      ReturnerName = tolower(ifelse(ServeIndicator == 1, player2, player1)),
      is_ace       = ifelse(ServeIndicator == 1, P1Ace, P2Ace),
      is_df        = ifelse(ServeIndicator == 1, P1DoubleFault, P2DoubleFault),
      server_won   = as.integer(ifelse(ServeIndicator == 1, PointWinner == 1, PointWinner == 2)),
      rally_le1    = if_else(RallyCount <= 1, 1L, 0L),
      is_efficient = as.integer(server_won & (RallyCount <= 3))
    )

  # --- Serve profiles & scaling ---
  serve1_profiles   <- get_serve_profiles(df_clean, serve_number_label = 1)
  serve2_profiles   <- get_serve_profiles(df_clean, serve_number_label = 2)
  serve1_profiles_z <- scale_cols(serve1_profiles)
  serve2_profiles_z <- scale_cols(serve2_profiles)

  # --- Build model data ---
  m1_df <- df_clean %>%
    filter(ServeNumber == 1) %>%
    inner_join(serve1_profiles_z %>% select(ServerName, avg_speed_z, sd_speed_z,
                                            location_entropy_z, modal_location),
               by = "ServerName") %>%
    select(server_won, is_efficient, ServerName, ReturnerName,
           avg_speed_z, sd_speed_z, location_entropy_z, modal_location)

  m2_df <- df_clean %>%
    filter(ServeNumber == 2) %>%
    inner_join(serve2_profiles_z %>% select(ServerName, avg_speed_z, sd_speed_z,
                                            location_entropy_z, modal_location),
               by = "ServerName") %>%
    select(server_won, is_efficient, ServerName, ReturnerName,
           avg_speed_z, sd_speed_z, location_entropy_z, modal_location)

  # --- Fit GLMMs ---
  message("  Fitting first-serve GLMM ...")
  m1 <- glmer(
    is_efficient ~ avg_speed_z + sd_speed_z + location_entropy_z + modal_location +
      (1 | ServerName) + (1 | ReturnerName),
    data = m1_df, family = binomial()
  )

  message("  Fitting second-serve GLMM ...")
  m2 <- glmer(
    is_efficient ~ avg_speed_z + sd_speed_z + location_entropy_z + modal_location +
      (1 | ServerName) + (1 | ReturnerName),
    data = m2_df, family = binomial()
  )

  # --- Build SQS ---
  sqs_first  <- build_sqs(m1, serve1_profiles_z) %>%
    mutate(ServerName = str_to_title(ServerName)) %>%
    arrange(desc(SQS_logodds))
  sqs_second <- build_sqs(m2, serve2_profiles_z) %>%
    mutate(ServerName = str_to_title(ServerName)) %>%
    arrange(desc(SQS_logodds))

  fixed_only_first <- sqs_first %>%
    select(ServerName, fixed_only_logodds = MeasuredSkill)
  fixed_only_second <- sqs_second %>%
    select(ServerName, fixed_only_logodds = MeasuredSkill)

  df_clean_titled <- df_clean %>%
    mutate(
      ServerName = str_to_title(ServerName),
      ReturnerName = str_to_title(ReturnerName)
    )

  stats_first <- compute_standard_serve_stats(df_clean_titled, serve_num = 1)
  stats_second <- compute_standard_serve_stats(df_clean_titled, serve_num = 2)
  re_only_first <- fit_random_effects_only(df_clean_titled, serve_num = 1)
  re_only_second <- fit_random_effects_only(df_clean_titled, serve_num = 2)

  df_sqs <- full_join(
    sqs_first  %>% select(ServerName, SQS_logodds) %>% rename(SQS_logodds_first  = SQS_logodds),
    sqs_second %>% select(ServerName, SQS_logodds) %>% rename(SQS_logodds_second = SQS_logodds),
    by = "ServerName"
  )

  # --- Clean test data ---
  df_test_clean <- test_data %>%
    filter(!is.na(ServeWidth), !is.na(ServeDepth), ServeWidth != "", ServeDepth != "") %>%
    filter(ServeNumber %in% c(1, 2)) %>%
    mutate(
      ServerName = tolower(ifelse(ServeIndicator == 1, player1, player2)),
      server_won = as.integer(ifelse(ServeIndicator == 1, PointWinner == 1, PointWinner == 2)),
      rally_le3  = if_else(RallyCount <= 3, 1L, 0L)
    ) %>%
    mutate(ServerName = str_to_title(ServerName))

  # --- Evaluate ---
  message("  Evaluating on temporal test set ...")
  results <- bind_rows(
    run_temporal_eval(
      df_test_clean = df_test_clean,
      df_sqs = df_sqs,
      standard_stats = stats_first,
      re_only_scores = re_only_first,
      fixed_only_scores = fixed_only_first,
      st = "first"
    ),
    run_temporal_eval(
      df_test_clean = df_test_clean,
      df_sqs = df_sqs,
      standard_stats = stats_second,
      re_only_scores = re_only_second,
      fixed_only_scores = fixed_only_second,
      st = "second"
    )
  )

  # --- Write results ---
  out_dir <- file.path("data/results", tag, "evaluation")
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  .codex_write_csv_gz(results, file.path(out_dir, "temporal_combined.csv.gz"), row.names = FALSE)
  message("  Results written to ", file.path(out_dir, "temporal_combined.csv.gz"))
}

for (t in tournaments) {
  for (g in genders) {
    process_temporal(t, g)
  }
}
