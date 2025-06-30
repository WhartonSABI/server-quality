rm(list=ls())

## -------- 0. FILE LOCATIONS -------------------------------------------
path_train_w_m <- "wimbledon_subset_m.csv" # 2021-2024 data, in the "data" folder
path_train_w_f <- "wimbledon_subset_f.csv"
path_train_u_m <- "usopen_subset_m.csv"
path_train_u_f <- "usopen_subset_f.csv"

path_oos_w_m   <- "oos_test_wimbledon_subset_m.csv" # 2018-2019 data, in the "new_code" folder
path_oos_w_f   <- "oos_test_wimbledon_subset_f.csv"
path_oos_u_m   <- "oos_test_usopen_subset_m.csv"
path_oos_u_f   <- "oos_test_usopen_subset_f.csv"

library(tidyverse)   # dplyr / readr / ggplot2 …
library(data.table)  # fread
library(splines)     # bs()
library(yardstick)   # log_loss_vec()
library(purrr) 

# training sets
train_sets <- list(
  wimbledon_m = fread(path_train_w_m),
  wimbledon_f = fread(path_train_w_f),
  usopen_m    = fread(path_train_u_m),
  usopen_f    = fread(path_train_u_f)
)

# out-of-sample test sets
oos_sets <- list(
  wimbledon_m = fread(path_oos_w_m),
  wimbledon_f = fread(path_oos_w_f),
  usopen_m    = fread(path_oos_u_m),
  usopen_f    = fread(path_oos_u_f)
)

## helper – guarantee `ElapsedSeconds_fixed` exists ----------------------
add_elapsed_fixed <- function(df) {
  if (!"ElapsedSeconds_fixed" %in% names(df))
    df$ElapsedSeconds_fixed <- df$ElapsedSeconds
  df
}
train_sets <- map(train_sets, add_elapsed_fixed)
oos_sets   <- map(oos_sets,   add_elapsed_fixed)

# ── 1. variables to standardise ────────────────────────────────────────
num_vars <- c("Speed_MPH",
              "speed_ratio",
              "importance",
              "ElapsedSeconds_fixed",
              "p_server_beats_returner")

std_in_place <- function(df, vars) {
  present <- intersect(vars, names(df))          # only those that exist
  df <- df %>%
    mutate(across(all_of(present),
                  ~ as.numeric(scale(., center = TRUE, scale = TRUE))))
  df
}

train_sets <- map(train_sets, std_in_place, vars = num_vars)
oos_sets   <- map(oos_sets,   std_in_place, vars = num_vars)

library(fs)          # for dir_create()
dir_create("scaled") # put everything in a new folder called “scaled”

# helper ---------------------------------------------------------------
save_scaled <- function(lst, suffix) {
  purrr::iwalk(lst, function(df, nm) {
    fn <- file.path("scaled", paste0(nm, "_", suffix, ".csv"))
    data.table::fwrite(df, fn)
  })
}

# write files ----------------------------------------------------------
save_scaled(train_sets, "train_scaled")  # e.g. scaled/wimbledon_m_train_scaled.csv
save_scaled(oos_sets,   "test_scaled")   # e.g. scaled/usopen_f_test_scaled.csv

## model formulas
form_speed <- serving_player_won ~ p_server_beats_returner +
  ElapsedSeconds_fixed + importance +
  bs(Speed_MPH, degree = 3, df = 5) +
  factor(ServeWidth) + factor(ServeDepth)

form_ratio <- serving_player_won ~ p_server_beats_returner +
  ElapsedSeconds_fixed + importance +
  bs(speed_ratio, degree = 3, df = 5) +
  factor(ServeWidth) + factor(ServeDepth)

models <- list()

for (ds in names(train_sets)) {
  gender   <- ifelse(grepl("_m$", ds), "m", "f")
  tourn    <- ifelse(grepl("^wimbledon", ds), "wimbledon", "usopen")
  df_train <- train_sets[[ds]]
  
  first  <- df_train[df_train$ServeNumber == 1]
  second <- df_train[df_train$ServeNumber == 2]
  
  models[[paste(tourn, gender, "first_spline_speed", sep = "_")]]  <-
    glm(form_speed,  data = first,  family = binomial)
  models[[paste(tourn, gender, "first_spline_ratio", sep = "_")]]  <-
    glm(form_ratio,  data = first,  family = binomial)
  models[[paste(tourn, gender, "second_spline_speed", sep = "_")]] <-
    glm(form_speed,  data = second, family = binomial)
  models[[paste(tourn, gender, "second_spline_ratio", sep = "_")]] <-
    glm(form_ratio,  data = second, family = binomial)
}

score_model <- function(model, df, label, dataset_name) {
  if (nrow(df) == 0) return(NULL)
  probs  <- predict(model, newdata = df, type = "response")
  truth  <- df$serving_player_won
  
  acc <- mean((probs > 0.5) == truth)
  
  eps <- 1e-15
  probs_clipped <- pmin(pmax(probs, eps), 1 - eps)
  ll <- -mean(truth * log(probs_clipped) + (1 - truth) * log(1 - probs_clipped))
  
  tibble(model   = label,
         dataset = dataset_name,
         n       = length(truth),
         accuracy = round(acc, 4),
         log_loss = round(ll, 4))
}

# build evaluation grid
eval_grid <- expand_grid(
  tournament = c("wimbledon", "usopen"),
  gender     = c("m", "f"),
  serve_n    = c(1, 2),
  type       = c("speed", "ratio")
) %>%
  mutate(
    model_name = paste(tournament, gender,
                       ifelse(serve_n == 1, "first", "second"),
                       "spline", type, sep = "_"),
    label      = paste(tournament, gender,
                       ifelse(serve_n == 1, "first", "second"),
                       type, sep = "_"),
    dataset_id = paste(tournament, gender, sep = "_")
  )

# evaluate
oos_results <- map_dfr(1:nrow(eval_grid), function(i) {
  row    <- eval_grid[i, ]
  oos_df <- oos_sets[[row$dataset_id]] %>%
    filter(ServeNumber == row$serve_n)
  score_model(models[[row$model_name]],
              oos_df,
              row$label,
              row$dataset_id)
})

print(oos_results, n = Inf)

write.csv(oos_results, "oos_results_all.csv")
