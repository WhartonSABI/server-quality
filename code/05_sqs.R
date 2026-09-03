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
library(lme4)

tournaments <- c("wimb", "us")
genders <- c("men", "women")

tag_prefix_for <- function(tournament, gender) {
  paste0(tournament, "_", gender)
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

build_sqs_with_ci <- function(model, profiles_z, server_re_name = "ServerName", level = 0.95) {
  # fixed effects + covariance
  b     <- fixef(model)
  Vbeta <- as.matrix(vcov(model))
  
  # random effects with conditional variance (BLUP variance)
  re_list <- ranef(model, condVar = TRUE)
  re      <- re_list[[server_re_name]]
  u_hat   <- re[, "(Intercept)"]
  names(u_hat) <- rownames(re)
  
  postVar <- attr(re, "postVar") # array: 1 x 1 x J (for random intercept)
  if (is.null(postVar)) {
    warning("condVar not available; random-effect SEs will be NA")
  }
  
  # fixed-effect design matrix for player profiles
  mm <- model.matrix(
    ~ avg_speed_z + sd_speed_z + location_entropy_z + modal_location,
    data = profiles_z
  )
  
  # align columns with fixef names
  cols <- intersect(colnames(mm), names(b))
  mm2  <- mm[, cols, drop = FALSE]
  bvec <- b[cols]
  
  measured <- as.numeric(mm2 %*% bvec)
  
  # random intercept vector aligned to profiles
  u_vec <- u_hat[profiles_z[[server_re_name]]]
  u_vec[is.na(u_vec)] <- 0
  
  # Var(SQS) = x'Vbeta x + Var(u_hat)
  
  # fixed part variance x'Vbeta x for each row
  Vsub <- Vbeta[cols, cols, drop = FALSE]
  var_fixed <- rowSums((mm2 %*% Vsub) * mm2)
  
  # random intercept conditional variance per server
  var_u <- rep(NA_real_, nrow(profiles_z))
  if (!is.null(postVar)) {
    server_levels <- rownames(re)
    idx <- match(profiles_z[[server_re_name]], server_levels)
    var_u <- ifelse(is.na(idx), 0, as.numeric(postVar[1, 1, idx]))
  }
  
  var_sqs <- var_fixed + var_u
  se_sqs  <- sqrt(var_sqs)
  
  alpha <- 1 - level
  zcrit <- qnorm(1 - alpha/2)
  
  sqs <- measured + u_vec
  ci_low  <- sqs - zcrit * se_sqs
  ci_high <- sqs + zcrit * se_sqs
  
  tibble(
    ServerName     = profiles_z[[server_re_name]],
    SQS_logodds    = sqs,
    SE_SQS         = se_sqs,
    CI_low         = ci_low,
    CI_high        = ci_high,
    MeasuredSkill  = measured,
    UnmeasuredCraft = u_vec
  )
}

decode_width <- function(code) {
  if (is.na(code)) return(NA_character_)
  prefix <- strsplit(code, "_", fixed = TRUE)[[1]][1]
  width_code <- substring(prefix, 2)
  dplyr::case_when(
    width_code == "B"  ~ "Body",
    width_code == "BC" ~ "Body/center",
    width_code == "BW" ~ "Body/wide",
    width_code == "C"  ~ "Center",
    width_code == "W"  ~ "Wide",
    TRUE ~ NA_character_
  )
}

decode_depth <- function(code) {
  if (is.na(code)) return(NA_character_)
  suffix <- strsplit(code, "_", fixed = TRUE)[[1]][2]
  depth_code <- substring(suffix, 2)
  dplyr::case_when(
    depth_code == "NCTL" ~ "Not close to line",
    depth_code == "CTL"  ~ "Close to line",
    TRUE ~ NA_character_
  )
}

decode_modal_location <- function(code) {
  if (is.na(code)) return(NA_character_)
  width <- decode_width(code)
  depth <- decode_depth(code)
  paste0(width, ", ", tolower(depth))
}

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

  zscore <- function(x) if (is.numeric(x)) (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE) else x
  eval_std <- eval_df %>%
    mutate(
      SQS_logodds_z   = zscore(SQS_logodds),
      welo_z          = zscore(welo_mean_type),
      eff_z           = zscore(serve_eff_type),
      win_z           = zscore(win_pct_type)
    )

  rmse_fun <- function(pred, obs) sqrt(mean((pred - obs)^2, na.rm = TRUE))
  corr_stats <- function(pred, obs, name_pred, name_outcome) {
    keep <- is.finite(pred) & is.finite(obs)
    if (sum(keep) < 3) {
      return(tibble(predictor = name_pred, outcome = name_outcome,
                    n = sum(keep), rmse = NA_real_, cor = NA_real_, p_value = NA_real_))
    }
    ct <- suppressWarnings(cor.test(pred[keep], obs[keep], method = "pearson"))
    tibble(
      predictor = name_pred, outcome = name_outcome, n = sum(keep),
      rmse = rmse_fun(pred[keep], obs[keep]),
      cor  = unname(ct$estimate), p_value = ct$p.value
    )
  }

  metrics <- bind_rows(
    corr_stats(eval_std$SQS_logodds_z, eval_std$eff_z,  paste0("SQS_", tag_label), "serve_efficiency"),
    corr_stats(eval_std$welo_z,     eval_std$eff_z,  "welo", "serve_efficiency"),
    corr_stats(eval_std$SQS_logodds_z, eval_std$win_z,  paste0("SQS_", tag_label), "win_pct"),
    corr_stats(eval_std$welo_z,     eval_std$win_z,  "welo", "win_pct")
  ) %>%
    mutate(serve_type = tag_label)

  out_path <- file.path(out_dir, paste0(tag_label, ".csv.gz"))
  write_csv(metrics, out_path)
}

process_tournament_gender <- function(tournament, gender) {
  tag_prefix <- tag_prefix_for(tournament, gender)

  training_path <- file.path("data/processed/splits", paste0(tournament, "_", gender, "_train.csv.gz"))
  testing_path <- file.path("data/processed/splits", paste0(tournament, "_", gender, "_test.csv.gz"))

  output_dir <- file.path("data/results", tag_prefix)
  evaluation_dir <- file.path(output_dir, "evaluation")
  rankings_dir <- file.path(output_dir, "rankings")
  dir.create(evaluation_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(rankings_dir, recursive = TRUE, showWarnings = FALSE)

  if (!file.exists(training_path) || !file.exists(testing_path)) {
    message("Missing train/test for ", tag_prefix)
    return(invisible(NULL))
  }

  df <- fread(training_path)

  df_clean <- df %>%
    filter(!is.na(ServeWidth), !is.na(ServeDepth), ServeWidth != "", ServeDepth != "") %>%
    filter(ServeNumber %in% c(1, 2)) %>%
    mutate(
      location_bin = paste0("W", ServeWidth, "_D", ServeDepth),
      ServerName = tolower(ifelse(ServeIndicator == 1, player1, player2)),
      ReturnerName = tolower(ifelse(ServeIndicator == 1, player2, player1)),
      is_ace = ifelse(ServeIndicator == 1, P1Ace, P2Ace),
      is_df = ifelse(ServeIndicator == 1, P1DoubleFault, P2DoubleFault),
      server_won = as.integer(ifelse(ServeIndicator == 1, PointWinner == 1, PointWinner == 2)),
      is_efficient = as.integer(server_won & (RallyCount <= 3))
    )

  serve1_profiles <- get_serve_profiles(df_clean, serve_number_label = 1)
  serve2_profiles <- get_serve_profiles(df_clean, serve_number_label = 2)

  serve1_profiles_z <- scale_cols(serve1_profiles)
  serve2_profiles_z <- scale_cols(serve2_profiles)

  m1_df <- df_clean %>%
    filter(ServeNumber == 1) %>%
    inner_join(serve1_profiles_z %>% select(ServerName, avg_speed_z, sd_speed_z, location_entropy_z, modal_location),
               by = "ServerName") %>%
    select(server_won, is_efficient, ServerName, ReturnerName,
           avg_speed_z, sd_speed_z, location_entropy_z, modal_location)

  m2_df <- df_clean %>%
    filter(ServeNumber == 2) %>%
    inner_join(serve2_profiles_z %>% select(ServerName, avg_speed_z, sd_speed_z, location_entropy_z, modal_location),
               by = "ServerName") %>%
    select(server_won, is_efficient, ServerName, ReturnerName,
           avg_speed_z, sd_speed_z, location_entropy_z, modal_location)

  m1 <- glmer(
    is_efficient ~ avg_speed_z + sd_speed_z + location_entropy_z + modal_location +
      (1 | ServerName) + (1 | ReturnerName),
    data = m1_df,
    family = binomial()
  )

  m2 <- glmer(
    is_efficient ~ avg_speed_z + sd_speed_z + location_entropy_z + modal_location +
      (1 | ServerName) + (1 | ReturnerName),
    data = m2_df,
    family = binomial()
  )

  sqs_first  <- build_sqs_with_ci(m1, serve1_profiles_z, server_re_name = "ServerName", level = 0.95)
  sqs_second <- build_sqs_with_ci(m2, serve2_profiles_z, server_re_name = "ServerName", level = 0.95)

  sqs_first_out <- sqs_first %>%
    mutate(ServerName = str_to_title(ServerName)) %>%
    arrange(desc(SQS_logodds))
  sqs_second_out <- sqs_second %>%
    mutate(ServerName = str_to_title(ServerName)) %>%
    arrange(desc(SQS_logodds))

  .codex_write_csv_gz(sqs_first_out, file.path(rankings_dir, "first.csv.gz"), row.names = FALSE)
  .codex_write_csv_gz(sqs_second_out, file.path(rankings_dir, "second.csv.gz"), row.names = FALSE)

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

  eval_by_serve_type(df_test_clean, sqs_first_out, evaluation_dir, serve_num = 1, tag_label = "first")
  eval_by_serve_type(df_test_clean, sqs_second_out, evaluation_dir, serve_num = 2, tag_label = "second")
}

for (t in tournaments) {
  for (g in genders) {
    process_tournament_gender(t, g)
  }
}

