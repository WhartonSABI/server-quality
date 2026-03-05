rm(list = ls())

library(tidyverse)
library(data.table)
library(lme4)

train_years <- c(2018, 2019, 2021, 2022)
test_years  <- c(2023, 2024)

tournaments <- c("wimb", "us")
genders     <- c("men", "women")

#-------------------------------------------------------------------------------
# Helpers (inlined from 03/05/06)

combine_years <- function(tournament, years, gender) {
  files <- paste0("data/processed/subset/", years, "_", tournament, "_", gender, ".csv")
  files <- files[file.exists(files)]
  if (length(files) == 0) return(NULL)
  combined <- rbindlist(lapply(files, fread), use.names = TRUE, fill = TRUE)
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

zscore <- function(x) (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)

#-------------------------------------------------------------------------------
# Evaluation (mirrors 06_oos-eval.R)

run_temporal_eval <- function(df_test_clean, df_sqs, st = c("first", "second")) {
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
    filter(!is.na(SQS_logodds), !is.na(welo_mean_test)) %>%
    mutate(
      SQS_z        = zscore(SQS_logodds),
      welo_z       = zscore(welo_mean_test),
      win_pct_test = wins_total / n_serves_test,
      eff_test     = wins_rally_le3 / n_serves_test
    )

  n_servers <- nrow(eval_df)
  if (n_servers < 3) {
    message("  Too few servers for evaluation (", st, "); skipping.")
    return(NULL)
  }

  m_win_sqs  <- glm(cbind(wins_total, n_serves_test - wins_total) ~ SQS_z,
                     family = binomial, data = eval_df)
  m_win_welo <- glm(cbind(wins_total, n_serves_test - wins_total) ~ welo_z,
                     family = binomial, data = eval_df)
  m_eff_sqs  <- glm(cbind(wins_rally_le3, n_serves_test - wins_rally_le3) ~ SQS_z,
                     family = binomial, data = eval_df)
  m_eff_welo <- glm(cbind(wins_rally_le3, n_serves_test - wins_rally_le3) ~ welo_z,
                     family = binomial, data = eval_df)

  cor_results <- tibble(
    model = c(paste0("win_sqs_", st), paste0("win_welo_", st),
              paste0("eff_sqs_", st), paste0("eff_welo_", st)),
    term  = c("SQS_z", "welo_z", "SQS_z", "welo_z"),
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

  bind_rows(
    extract_coefs(m_win_sqs,  paste0("win_sqs_",  st)),
    extract_coefs(m_win_welo, paste0("win_welo_", st)),
    extract_coefs(m_eff_sqs,  paste0("eff_sqs_",  st)),
    extract_coefs(m_eff_welo, paste0("eff_welo_", st))
  ) %>%
    mutate(serve_type = st, n_servers = n_servers) %>%
    left_join(cor_results, by = c("model", "term", "serve_type"))
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
  results <- map_dfr(c("first", "second"), ~ run_temporal_eval(df_test_clean, df_sqs, .x))

  # --- Write results ---
  out_dir <- file.path("data/results", tag, "evaluation")
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  write.csv(results, file.path(out_dir, "temporal_combined.csv"), row.names = FALSE)
  message("  Results written to ", file.path(out_dir, "temporal_combined.csv"))
}

for (t in tournaments) {
  for (g in genders) {
    process_temporal(t, g)
  }
}
