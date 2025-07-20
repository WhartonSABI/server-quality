##############################################################################
## 0.  SETUP ─ libraries & data ----------------------------------------------
##############################################################################
rm(list = ls())
summary_log <- character()      # ← collects summaries as we go


library(tidyverse)   # dplyr, purrr, ggplot2, …
library(data.table)  # fread()
library(ggplot2)
library(car)
library(splines)
library(broom)       # tidy model output (optional)

## --- Load Wimbledon training subsets (scaled version you used for modeling)
subset_m <- fread("../data/output/scaled-results/usopen_m_train_scaled.csv")
subset_f <- fread("../data/output/scaled-results/usopen_f_train_scaled.csv")

##############################################################################
## 1.  PLOTTER (unchanged) ----------------------------------------------------
##############################################################################
plot_linear_model <- function(df, model, speed_col, title, save_path) {
  
  # a) prediction grid --------------------------------------------------------
  speed_vals <- seq(min(df[[speed_col]], na.rm = TRUE),
                    max(df[[speed_col]], na.rm = TRUE),
                    length.out = 200)
  
  ref_dat <- df |>
    summarise(across(c(p_server_beats_returner_z,
                       ElapsedSeconds_fixed_z,
                       importance_z,
                       df_pct_server_z), \(x) mean(x, na.rm = TRUE))) |>
    slice(rep(1, length(speed_vals)))        # 200 identical rows
  ref_dat[[speed_col]] <- speed_vals
  
  # modal factor levels
  ref_dat$ServeWidth <- names(which.max(table(df$ServeWidth)))[1]
  ref_dat$ServeDepth <- names(which.max(table(df$ServeDepth)))[1]
  
  pred_prob <- predict(model, newdata = ref_dat, type = "response")
  
  pred_df <- tibble(
    Speed       = speed_vals,
    Probability = pred_prob,
    Source      = "Linear Prediction"
  )
  
  # b) weighted empirical -----------------------------------------------------
  df <- df |>
    mutate(server = if_else(PointServer == 1, player1_name, player2_name)) |>
    add_count(server, name = "n_points") |>
    mutate(weight = 1 / n_points)
  
  empirical_df <- df |>
    mutate(speed_bin = cut(.data[[speed_col]],
                           breaks = seq(floor(min(.data[[speed_col]], na.rm = TRUE)),
                                        ceiling(max(.data[[speed_col]], na.rm = TRUE)),
                                        by = ifelse(speed_col == "Speed_MPH", 5, 0.025)))) |>
    group_by(speed_bin) |>
    summarise(
      Speed       = weighted.mean(.data[[speed_col]], w = weight, na.rm = TRUE),
      Probability = weighted.mean(serving_player_won == 1, w = weight, na.rm = TRUE),
      .groups     = "drop"
    ) |>
    mutate(Source = "Empirical Win Rate (Weighted)")
  
  # c) combine & plot ---------------------------------------------------------
  plot_df <- bind_rows(empirical_df, pred_df)
  
  ggplot(plot_df, aes(Speed, Probability, colour = Source)) +
    geom_line(size = 1.1) +
    labs(
      title  = title,
      x      = ifelse(speed_col == "Speed_MPH", "Serve Speed (MPH)", "Speed Ratio"),
      y      = "Probability Server Wins",
      colour = NULL
    ) +
    theme_minimal() +
    scale_color_manual(values = c("black", "red"))
  
  ggsave(save_path, bg = "white", width = 8, height = 6, units = "in")
}

##############################################################################
## 2.  SPLIT INTO FOUR SERVE GROUPS ------------------------------------------
##############################################################################
m_first  <- subset_m[ServeNumber == 1]
m_second <- subset_m[ServeNumber == 2]
f_first  <- subset_f[ServeNumber == 1]
f_second <- subset_f[ServeNumber == 2]

##############################################################################
## 3.  BUILD-FORMULA & MODEL-FITTING HELPERS ----------------------------------
##############################################################################
# (a) build the list of RHS strings incrementally -----------------------------
build_formulas <- function(speed_var) {
  predictors <- c(
    speed_var,                       # ① base term
    "p_server_beats_returner_z",       # ② add one at a time
    "ElapsedSeconds_fixed_z",          # ③
    "importance_z",                    # ④
    "df_pct_server_z",                 # ⑤
    "factor(ServeWidth)",            # ⑥
    "factor(ServeDepth)"             # ⑦
  )
  
  map(seq_along(predictors), function(i) {
    rhs <- paste(predictors[1:i], collapse = " + ")
    as.formula(paste("serving_player_won ~", rhs))
  })
}

# (b) fit every formula & print a summary -------------------------------------
fit_and_print <- function(df, formulas, label_prefix) {
  
  models <- vector("list", length(formulas))
  
  for (i in seq_along(formulas)) {
    
    m <- glm(formulas[[i]], data = df, family = binomial)
    
    header <- paste0(
      "\n===============================================================\n",
      label_prefix, " — Model ", i, "\n",
      "Formula: ", deparse(formulas[[i]]), "\n",
      "---------------------------------------------------------------"
    )
    
    ## ---- print to console ----
    cat(header, "\n")
    print(summary(m))
    
    ## ---- capture for the log ----
    summary_text <- c(header, capture.output(summary(m)), "")  # "" = blank line
    summary_log  <<- c(summary_log, summary_text)               # append globally
    
    models[[i]] <- m
  }
  
  names(models) <- paste0(label_prefix, "_m", seq_along(models))
  models
}

##############################################################################
## 4.  MAIN DRIVER FOR EACH GROUP --------------------------------------------
##############################################################################
run_group_models <- function(df, group_id, group_name) {
  
  ## drop zero-speed serves ---------------------------------------------------
  df <- df %>% filter(Speed_MPH > 0)
  
  ## (i) SPEED IN MPH ---------------------------------------------------------
  mph_forms  <- build_formulas("Speed_MPH_z")
  mph_models <- fit_and_print(df, mph_forms,
                              paste0(group_id, "_speed"))
  # full model = last element
  full_mph   <- mph_models[[length(mph_models)]]
  
  ## (ii) SPEED RATIO ---------------------------------------------------------
  ratio_forms  <- build_formulas("speed_ratio_z")
  ratio_models <- fit_and_print(df, ratio_forms,
                                paste0(group_id, "_ratio"))
  full_ratio   <- ratio_models[[length(ratio_models)]]
  
  ## keep final models in global env (same names you used before) -------------
  assign(paste0(group_id, "_linear_speed"),  full_mph,   envir = .GlobalEnv)
  assign(paste0(group_id, "_linear_ratio"),  full_ratio, envir = .GlobalEnv)
  
  ## save the familiar plots --------------------------------------------------
  plot_linear_model(
    df, full_mph,   "Speed_MPH_z",
    title     = paste("Linear vs. Empirical (Speed MPH) —", group_name),
    save_path = paste0("../images/", group_id, "_linear_speed.png")
  )
  
  plot_linear_model(
    df, full_ratio, "speed_ratio_z",
    title     = paste("Linear vs. Empirical (Speed Ratio) —", group_name),
    save_path = paste0("../images/", group_id, "_linear_ratio.png")
  )
  
  invisible(list(speed = mph_models, ratio = ratio_models))
}

##############################################################################
## 5.  RUN ALL FOUR GROUPS ----------------------------------------------------
##############################################################################
run_group_models(m_first,  "m_first",  "Males – First Serve")
run_group_models(m_second, "m_second", "Males – Second Serve")
run_group_models(f_first,  "f_first",  "Females – First Serve")
run_group_models(f_second, "f_second", "Females – Second Serve")

##############################################################################
## 6.  (optional) QUICK LOOK AT THE “FULL” MODELS -----------------------------
##############################################################################
final_model_names <- c(
  "m_first_linear_speed",  "m_first_linear_ratio",
  "m_second_linear_speed", "m_second_linear_ratio",
  "f_first_linear_speed",  "f_first_linear_ratio",
  "f_second_linear_speed", "f_second_linear_ratio"
)

cat("\n###############################\n")
cat("### FINAL (FULL) MODEL SUMMARIES\n")
cat("###############################\n")
for (m in final_model_names) {
  cat("\n==============================\n")
  cat("Model Summary:", m, "\n")
  cat("==============================\n")
  print(summary(get(m)))
}

## ------------------------------------------------------------------------
##  WRITE *ALL* MODEL SUMMARIES (sequential + full) TO DISK  ---------------
## ------------------------------------------------------------------------
writeLines(summary_log, "usopen_logit_model_summaries_linear.txt")
cat("\nSaved every model summary to 'all_model_summaries.txt'\n")

## ------------------------------------------------------------------------
# spline
## ------------------------------------------------------------------------

##############################################################################
## 0.  SETUP ─ libraries & data ----------------------------------------------
##############################################################################
rm(list = ls())
summary_log <- character()                # collects summaries as we go

library(tidyverse)    # dplyr, purrr, ggplot2, …
library(data.table)   # fread()
library(ggplot2)
library(car)
library(splines)      # bs()
library(broom)        # tidy model output (optional)

## --- Load Wimbledon training subsets (scaled version)
subset_m <- fread("../data/output/scaled-results/usopen_m_train_scaled.csv")
subset_f <- fread("../data/output/scaled-results/usopen_f_train_scaled.csv")

##############################################################################
## 1.  PLOTTER (unchanged: model may be spline or linear) --------------------
##############################################################################
plot_linear_model <- function(df, model, speed_col, title, save_path) {
  
  speed_vals <- seq(min(df[[speed_col]], na.rm = TRUE),
                    max(df[[speed_col]], na.rm = TRUE),
                    length.out = 200)
  
  ref_dat <- df |>
    summarise(across(c(p_server_beats_returner_z,
                       ElapsedSeconds_fixed_z,
                       importance_z,
                       df_pct_server_z), \(x) mean(x, na.rm = TRUE))) |>
    slice(rep(1, length(speed_vals)))
  ref_dat[[speed_col]] <- speed_vals
  
  # modal factor levels
  ref_dat$ServeWidth <- names(which.max(table(df$ServeWidth)))[1]
  ref_dat$ServeDepth <- names(which.max(table(df$ServeDepth)))[1]
  
  pred_prob <- predict(model, newdata = ref_dat, type = "response")
  
  pred_df <- tibble(
    Speed       = speed_vals,
    Probability = pred_prob,
    Source      = "Spline Prediction"
  )
  
  df <- df |>
    mutate(server = if_else(PointServer == 1, player1_name, player2_name)) |>
    add_count(server, name = "n_points") |>
    mutate(weight = 1 / n_points)
  
  empirical_df <- df |>
    mutate(speed_bin = cut(.data[[speed_col]],
                           breaks = seq(floor(min(.data[[speed_col]], na.rm = TRUE)),
                                        ceiling(max(.data[[speed_col]], na.rm = TRUE)),
                                        by = ifelse(speed_col == "Speed_MPH", 5, 0.025)))) |>
    group_by(speed_bin) |>
    summarise(
      Speed       = weighted.mean(.data[[speed_col]], w = weight, na.rm = TRUE),
      Probability = weighted.mean(serving_player_won == 1, w = weight, na.rm = TRUE),
      .groups     = "drop"
    ) |>
    mutate(Source = "Empirical Win Rate (Weighted)")
  
  ggplot(bind_rows(empirical_df, pred_df),
         aes(Speed, Probability, colour = Source)) +
    geom_line(size = 1.1) +
    labs(
      title  = title,
      x      = ifelse(speed_col == "Speed_MPH", "Serve Speed (MPH)", "Speed Ratio"),
      y      = "Probability Server Wins",
      colour = NULL
    ) +
    theme_minimal() +
    scale_color_manual(values = c("black", "red"))
  
  ggsave(save_path, bg = "white", width = 8, height = 6, units = "in")
}

##############################################################################
## 2.  SPLIT INTO FOUR SERVE GROUPS ------------------------------------------
##############################################################################
m_first  <- subset_m[ServeNumber == 1]
m_second <- subset_m[ServeNumber == 2]
f_first  <- subset_f[ServeNumber == 1]
f_second <- subset_f[ServeNumber == 2]

##############################################################################
## 3.  BUILD-FORMULA & MODEL-FITTING HELPERS ---------------------------------
##############################################################################
# (a) incremental RHS builder including spline term --------------------------
build_formulas <- function(speed_var) {
  spline_term <- paste0("bs(", speed_var, ", df = 5, degree = 3)")
  
  predictors <- c(
    spline_term,                     # ① spline of speed
    "p_server_beats_returner_z",     # ② add one at a time
    "ElapsedSeconds_fixed_z",        # ③
    "importance_z",                  # ④
    "df_pct_server_z",               # ⑤
    "factor(ServeWidth)",            # ⑥
    "factor(ServeDepth)"             # ⑦
  )
  
  map(seq_along(predictors), function(i) {
    rhs <- paste(predictors[1:i], collapse = " + ")
    as.formula(paste("serving_player_won ~", rhs))
  })
}

# (b) fit every formula & log summaries --------------------------------------
fit_and_print <- function(df, formulas, label_prefix) {
  
  models <- vector("list", length(formulas))
  
  for (i in seq_along(formulas)) {
    
    m <- glm(formulas[[i]], data = df, family = binomial)
    
    header <- paste0(
      "\n===============================================================\n",
      label_prefix, " — Model ", i, "\n",
      "Formula: ", deparse(formulas[[i]]), "\n",
      "---------------------------------------------------------------"
    )
    
    cat(header, "\n")
    print(summary(m))
    
    summary_text <- c(header, capture.output(summary(m)), "")
    summary_log  <<- c(summary_log, summary_text)    # append globally
    
    models[[i]] <- m
  }
  
  names(models) <- paste0(label_prefix, "_m", seq_along(models))
  models
}

##############################################################################
## 4.  MAIN DRIVER FOR EACH GROUP --------------------------------------------
##############################################################################
run_group_models <- function(df, group_id, group_name) {
  
  df <- df %>% filter(Speed_MPH > 0)      # drop zero-speed serves
  
  ## (i) SPLINE ON SPEED IN MPH ----------------------------------------------
  mph_forms  <- build_formulas("Speed_MPH_z")
  mph_models <- fit_and_print(df, mph_forms,
                              paste0(group_id, "_speed"))
  full_mph   <- mph_models[[length(mph_models)]]
  
  ## (ii) SPLINE ON SPEED RATIO ----------------------------------------------
  ratio_forms  <- build_formulas("speed_ratio_z")
  ratio_models <- fit_and_print(df, ratio_forms,
                                paste0(group_id, "_ratio"))
  full_ratio   <- ratio_models[[length(ratio_models)]]
  
  ## keep full models in global env ------------------------------------------
  assign(paste0(group_id, "_linear_speed"), full_mph,   envir = .GlobalEnv)
  assign(paste0(group_id, "_linear_ratio"), full_ratio, envir = .GlobalEnv)
  
  ## save plots ---------------------------------------------------------------
  plot_linear_model(
    df, full_mph,   "Speed_MPH_z",
    title     = paste("Spline vs. Empirical (Speed MPH) —", group_name),
    save_path = paste0("../images/", group_id, "_spline_speed.png")
  )
  
  plot_linear_model(
    df, full_ratio, "speed_ratio_z",
    title     = paste("Spline vs. Empirical (Speed Ratio) —", group_name),
    save_path = paste0("../images/", group_id, "_spline_ratio.png")
  )
  
  invisible(list(speed = mph_models, ratio = ratio_models))
}

##############################################################################
## 5.  RUN ALL FOUR GROUPS ----------------------------------------------------
##############################################################################
run_group_models(m_first,  "m_first",  "Males – First Serve")
run_group_models(m_second, "m_second", "Males – Second Serve")
run_group_models(f_first,  "f_first",  "Females – First Serve")
run_group_models(f_second, "f_second", "Females – Second Serve")

##############################################################################
## 6.  (optional) QUICK LOOK AT THE “FULL” SPLINE MODELS ----------------------
##############################################################################
final_model_names <- c(
  "m_first_linear_speed",  "m_first_linear_ratio",
  "m_second_linear_speed", "m_second_linear_ratio",
  "f_first_linear_speed",  "f_first_linear_ratio",
  "f_second_linear_speed", "f_second_linear_ratio"
)

cat("\n###############################\n")
cat("### FINAL (FULL) SPLINE MODEL SUMMARIES\n")
cat("###############################\n")
for (m in final_model_names) {
  cat("\n==============================\n")
  cat("Model Summary:", m, "\n")
  cat("==============================\n")
  print(summary(get(m)))
}

##############################################################################
## 7.  WRITE ALL MODEL SUMMARIES TO DISK --------------------------------------
##############################################################################
writeLines(summary_log, "usopen_logit_model_summaries_spline.txt")
cat("\nSaved every model summary to 'wimbledon_logit_model_summaries_spline.txt'\n")
