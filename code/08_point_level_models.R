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

# 07_point_level_model.R

rm(list = ls())
library(tidyverse)
library(data.table)
library(lme4)
# install.packages('broom.mixed')

tournaments <- c("wimb", "us")
genders <- c("men", "women")

tag_prefix_for <- function(tournament, gender) {
  paste0(tournament, "_", gender)
}

get_mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

# -------------------------------------------------
# Helpers for preprocessing
# -------------------------------------------------

compute_server_modal_location <- function(df, serve_number_label) {
  df %>%
    filter(ServeNumber %in% serve_number_label) %>%
    group_by(ServerName) %>%
    summarise(
      modal_location = get_mode(location_bin),
      n_serves = n(),
      .groups = "drop"
    ) %>%
    filter(n_serves > 20)
}

scale_speed_train <- function(d) {
  mu  <- mean(d$Speed_MPH, na.rm = TRUE)
  sig <- sd(d$Speed_MPH, na.rm = TRUE)
  
  if (!is.finite(sig) || sig == 0) {
    sig <- 1
  }
  
  d %>%
    mutate(Speed_MPH_z = (Speed_MPH - mu) / sig)
}

apply_speed_scaling <- function(d, mu, sig) {
  if (!is.finite(sig) || sig == 0) {
    sig <- 1
  }
  
  d %>%
    mutate(Speed_MPH_z = (Speed_MPH - mu) / sig)
}

# -------------------------------------------------
# Build server-level SQS from point-level model
# -------------------------------------------------
# Idea:
# For each server, average the fixed-effect design rows across that player's
# observed serve points in training, then add the server random intercept.
# This gives a server-level score implied by the point-level model.

build_point_level_sqs_with_ci <- function(model, model_df, server_re_name = "ServerName", level = 0.95) {
  # fixed effects + covariance
  b <- fixef(model)
  Vbeta <- as.matrix(vcov(model))
  
  # server random effects + conditional variance
  re_list <- ranef(model, condVar = TRUE)
  re <- re_list[[server_re_name]]
  u_hat <- re[, "(Intercept)"]
  names(u_hat) <- rownames(re)
  
  postVar <- attr(re, "postVar")
  if (is.null(postVar)) {
    warning("condVar not available; random-effect SEs will be NA")
  }
  
  # fixed-effects-only formula
  fixed_formula <- lme4::nobars(formula(model))
  fixed_terms   <- delete.response(terms(fixed_formula))
  
  # design matrix at the point level
  mm <- model.matrix(fixed_terms, data = model_df)
  
  # align to fixef names
  cols <- intersect(colnames(mm), names(b))
  mm2  <- mm[, cols, drop = FALSE]
  bvec <- b[cols]
  
  # aggregate mean design vector by server
  mm_df <- as_tibble(mm2) %>%
    mutate(ServerName = model_df[[server_re_name]]) %>%
    group_by(ServerName) %>%
    summarise(across(everything(), ~ mean(.x, na.rm = TRUE)), .groups = "drop")
  
  server_names <- mm_df$ServerName
  Xbar <- as.matrix(mm_df[, cols, drop = FALSE])
  
  measured <- as.numeric(Xbar %*% bvec)
  
  # fixed-effect variance for each averaged design row
  Vsub <- Vbeta[cols, cols, drop = FALSE]
  var_fixed <- rowSums((Xbar %*% Vsub) * Xbar)
  
  # random intercept + conditional variance
  u_vec <- u_hat[server_names]
  u_vec[is.na(u_vec)] <- 0
  
  var_u <- rep(NA_real_, length(server_names))
  if (!is.null(postVar)) {
    server_levels <- rownames(re)
    idx <- match(server_names, server_levels)
    var_u <- ifelse(is.na(idx), 0, as.numeric(postVar[1, 1, idx]))
  }
  
  var_sqs <- var_fixed + var_u
  se_sqs <- sqrt(var_sqs)
  
  alpha <- 1 - level
  zcrit <- qnorm(1 - alpha / 2)
  
  sqs <- measured + u_vec
  ci_low  <- sqs - zcrit * se_sqs
  ci_high <- sqs + zcrit * se_sqs
  
  tibble(
    ServerName       = server_names,
    SQS_logodds      = sqs,
    SE_SQS           = se_sqs,
    CI_low           = ci_low,
    CI_high          = ci_high,
    MeasuredSkill    = measured,
    UnmeasuredCraft  = u_vec
  ) %>%
    arrange(desc(SQS_logodds))
}

# -------------------------------------------------
# OOS evaluation
# -------------------------------------------------

eval_by_serve_type <- function(df_test_clean, sqs_tbl, out_dir, serve_num, tag_label) {
  preds <- sqs_tbl %>%
    select(ServerName, SQS_logodds)
  
  test_type <- df_test_clean %>%
    filter(ServeNumber == serve_num) %>%
    mutate(welo_value = ifelse(ServeIndicator == 1, player1_avg_welo, player2_avg_welo)) %>%
    group_by(ServerName) %>%
    summarise(
      n_serves_test_type   = n(),
      wins_total_type      = sum(server_won, na.rm = TRUE),
      wins_rally_le3_type  = sum(server_won * (RallyCount <= 3), na.rm = TRUE),
      win_pct_type         = wins_total_type / n_serves_test_type,
      serve_eff_type       = wins_rally_le3_type / n_serves_test_type,
      welo_mean_type       = mean(welo_value, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    filter(n_serves_test_type > 20)
  
  eval_df <- preds %>%
    inner_join(test_type, by = "ServerName")
  
  zscore <- function(x) {
    if (is.numeric(x)) {
      s <- sd(x, na.rm = TRUE)
      if (!is.finite(s) || s == 0) return(rep(0, length(x)))
      (x - mean(x, na.rm = TRUE)) / s
    } else {
      x
    }
  }
  
  eval_std <- eval_df %>%
    mutate(
      SQS_logodds_z = zscore(SQS_logodds),
      welo_z        = zscore(welo_mean_type),
      eff_z         = zscore(serve_eff_type),
      win_z         = zscore(win_pct_type)
    )
  
  rmse_fun <- function(pred, obs) sqrt(mean((pred - obs)^2, na.rm = TRUE))
  
  corr_stats <- function(pred, obs, name_pred, name_outcome) {
    keep <- is.finite(pred) & is.finite(obs)
    
    if (sum(keep) < 3) {
      return(
        tibble(
          predictor = name_pred,
          outcome   = name_outcome,
          n         = sum(keep),
          rmse      = NA_real_,
          cor       = NA_real_,
          p_value   = NA_real_
        )
      )
    }
    
    ct <- suppressWarnings(cor.test(pred[keep], obs[keep], method = "pearson"))
    
    tibble(
      predictor = name_pred,
      outcome   = name_outcome,
      n         = sum(keep),
      rmse      = rmse_fun(pred[keep], obs[keep]),
      cor       = unname(ct$estimate),
      p_value   = ct$p.value
    )
  }
  
  metrics <- bind_rows(
    corr_stats(eval_std$SQS_logodds_z, eval_std$eff_z, paste0("SQS_", tag_label), "serve_efficiency"),
    corr_stats(eval_std$welo_z,        eval_std$eff_z, "welo",                     "serve_efficiency"),
    corr_stats(eval_std$SQS_logodds_z, eval_std$win_z, paste0("SQS_", tag_label), "win_pct"),
    corr_stats(eval_std$welo_z,        eval_std$win_z, "welo",                     "win_pct")
  ) %>%
    mutate(serve_type = tag_label)
  
  out_path <- file.path(out_dir, paste0(tag_label, ".csv.gz"))
  write_csv(metrics, out_path)
}

# -------------------------------------------------
# Main processing function
# -------------------------------------------------

process_tournament_gender <- function(tournament, gender) {
  tag_prefix <- tag_prefix_for(tournament, gender)
  
  training_path <- file.path("data/processed/splits", paste0(tournament, "_", gender, "_train.csv.gz"))
  testing_path  <- file.path("data/processed/splits", paste0(tournament, "_", gender, "_test.csv.gz"))
  
  output_dir     <- file.path("data/results/results_point_level", tag_prefix)
  evaluation_dir <- file.path(output_dir, "evaluation")
  rankings_dir   <- file.path(output_dir, "rankings")
  models_dir     <- file.path(output_dir, "model_summaries")
  
  dir.create(evaluation_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(rankings_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(models_dir, recursive = TRUE, showWarnings = FALSE)
  
  if (!file.exists(training_path) || !file.exists(testing_path)) {
    message("Missing train/test for ", tag_prefix)
    return(invisible(NULL))
  }
  
  # -------------------------
  # Training data
  # -------------------------
  df <- fread(training_path)
  
  df_clean <- df %>%
    filter(!is.na(ServeWidth), !is.na(ServeDepth), ServeWidth != "", ServeDepth != "") %>%
    filter(!is.na(Speed_MPH)) %>%
    filter(ServeNumber %in% c(1, 2)) %>%
    mutate(
      location_bin   = paste0("W", ServeWidth, "_D", ServeDepth),
      ServerName     = tolower(ifelse(ServeIndicator == 1, player1, player2)),
      ReturnerName   = tolower(ifelse(ServeIndicator == 1, player2, player1)),
      server_won     = as.integer(ifelse(ServeIndicator == 1, PointWinner == 1, PointWinner == 2)),
      is_efficient   = as.integer(server_won & (RallyCount <= 3))
    )
  
  # first-serve modal locations
  serve1_modal <- compute_server_modal_location(df_clean, serve_number_label = 1)
  
  m1_df <- df_clean %>%
    filter(ServeNumber == 1) %>%
    inner_join(serve1_modal %>% select(ServerName, modal_location), by = "ServerName") %>%
    mutate(
      is_modal_location = as.integer(location_bin == modal_location),
      location_bin = factor(location_bin)
    ) %>%
    filter(!is.na(is_modal_location))
  
  # second-serve modal locations
  serve2_modal <- compute_server_modal_location(df_clean, serve_number_label = 2)
  
  m2_df <- df_clean %>%
    filter(ServeNumber == 2) %>%
    inner_join(serve2_modal %>% select(ServerName, modal_location), by = "ServerName") %>%
    mutate(
      is_modal_location = as.integer(location_bin == modal_location),
      location_bin = factor(location_bin)
    ) %>%
    filter(!is.na(is_modal_location))
  
  # speed scaling separately by serve type, using training data only
  speed1_mu <- mean(m1_df$Speed_MPH, na.rm = TRUE)
  speed1_sd <- sd(m1_df$Speed_MPH, na.rm = TRUE)
  speed2_mu <- mean(m2_df$Speed_MPH, na.rm = TRUE)
  speed2_sd <- sd(m2_df$Speed_MPH, na.rm = TRUE)
  
  m1_df <- apply_speed_scaling(m1_df, speed1_mu, speed1_sd)
  m2_df <- apply_speed_scaling(m2_df, speed2_mu, speed2_sd)
  
  # -------------------------
  # Point-level GLMMs
  # -------------------------
  m1 <- glmer(
    is_efficient ~ Speed_MPH_z + location_bin + is_modal_location +
      (1 | ServerName) + (1 | ReturnerName),
    data = m1_df,
    family = binomial()
  )
  
  m2 <- glmer(
    is_efficient ~ Speed_MPH_z + location_bin + is_modal_location +
      (1 | ServerName) + (1 | ReturnerName),
    data = m2_df,
    family = binomial()
  )
  
  # save coefficient tables
  first_coef <- broom.mixed::tidy(m1, effects = "fixed")
  second_coef <- broom.mixed::tidy(m2, effects = "fixed")
  
  write_csv(first_coef,  file.path(models_dir, "first_fixed_effects.csv.gz"))
  write_csv(second_coef, file.path(models_dir, "second_fixed_effects.csv.gz"))
  
  # -------------------------
  # Build server-level SQS rankings
  # -------------------------
  sqs_first <- build_point_level_sqs_with_ci(
    model = m1,
    model_df = m1_df,
    server_re_name = "ServerName",
    level = 0.95
  )
  
  sqs_second <- build_point_level_sqs_with_ci(
    model = m2,
    model_df = m2_df,
    server_re_name = "ServerName",
    level = 0.95
  )
  
  sqs_first_out <- sqs_first %>%
    mutate(ServerName = str_to_title(ServerName)) %>%
    arrange(desc(SQS_logodds))
  
  sqs_second_out <- sqs_second %>%
    mutate(ServerName = str_to_title(ServerName)) %>%
    arrange(desc(SQS_logodds))
  
  .codex_write_csv_gz(sqs_first_out,  file.path(rankings_dir, "first.csv.gz"),  row.names = FALSE)
  .codex_write_csv_gz(sqs_second_out, file.path(rankings_dir, "second.csv.gz"), row.names = FALSE)
  
  # -------------------------
  # Testing data for OOS evaluation
  # -------------------------
  df_test <- fread(testing_path)
  
  df_test_clean <- df_test %>%
    filter(!is.na(ServeWidth), !is.na(ServeDepth), ServeWidth != "", ServeDepth != "") %>%
    filter(ServeNumber %in% c(1, 2)) %>%
    mutate(
      ServerName = tolower(ifelse(ServeIndicator == 1, player1, player2)),
      server_won = as.integer(ifelse(ServeIndicator == 1, PointWinner == 1, PointWinner == 2)),
      rally_le3  = if_else(RallyCount <= 3, 1L, 0L)
    ) %>%
    mutate(ServerName = str_to_title(ServerName))
  
  eval_by_serve_type(df_test_clean, sqs_first_out,  evaluation_dir, serve_num = 1, tag_label = "first")
  eval_by_serve_type(df_test_clean, sqs_second_out, evaluation_dir, serve_num = 2, tag_label = "second")
  
  message("Finished point-level model for ", tag_prefix)
}

# -------------------------------------------------
# Run all tournament x gender combinations
# -------------------------------------------------

for (t in tournaments) {
  for (g in genders) {
    process_tournament_gender(t, g)
  }
}