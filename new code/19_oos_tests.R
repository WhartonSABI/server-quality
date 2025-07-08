rm(list=ls())

## -------- 0. FILE LOCATIONS -------------------------------------------
path_train_w_m <- "out_data/scaled/wimbledon_subset_m_training.csv" # 2021-2024 data
path_train_w_f <- "out_data/scaled/wimbledon_subset_f_training.csv"
path_train_u_m <- "out_data/scaled/usopen_subset_m_training.csv"
path_train_u_f <- "out_data/scaled/usopen_subset_f_training.csv"

path_oos_w_m   <- "out_data/scaled/wimbledon_subset_m_testing.csv" # 2018-2019 data
path_oos_w_f   <- "out_data/scaled/wimbledon_subset_f_testing.csv"
path_oos_u_m   <- "out_data/scaled/usopen_subset_m_testing.csv"
path_oos_u_f   <- "out_data/scaled/usopen_subset_f_testing.csv"

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

train_sets <- lapply(train_sets, function(dt) dt[Speed_MPH > 0])
oos_sets <- lapply(oos_sets, function(dt) dt[Speed_MPH > 0])

# ## helper – guarantee `ElapsedSeconds_fixed` exists ----------------------
# add_elapsed_fixed <- function(df) {
#   if (!"ElapsedSeconds_fixed" %in% names(df))
#     df$ElapsedSeconds_fixed <- df$ElapsedSeconds
#   df
# }
# train_sets <- map(train_sets, add_elapsed_fixed)
# oos_sets   <- map(oos_sets,   add_elapsed_fixed)

## model formulas--swap spline with regular speed variable as desired
form_speed <- serving_player_won ~ p_server_beats_returner_z +
  ElapsedSeconds_fixed_z + importance_z +
  Speed_MPH_z + df_pct_server_z + 
  # bs(Speed_MPH, degree = 3, df = 5) +
  factor(ServeWidth) + factor(ServeDepth)

form_ratio <- serving_player_won ~ p_server_beats_returner_z +
  ElapsedSeconds_fixed_z + importance_z +
  speed_ratio_z + df_pct_server_z +
  # bs(speed_ratio, degree = 3, df = 5) +
  factor(ServeWidth) + factor(ServeDepth)

models <- list()

for (ds in names(train_sets)) {
  gender   <- ifelse(grepl("_m$", ds), "m", "f")
  tourn    <- ifelse(grepl("^wimbledon", ds), "wimbledon", "usopen")
  df_train <- train_sets[[ds]]
  
  first  <- df_train[df_train$ServeNumber == 1]
  second <- df_train[df_train$ServeNumber == 2]
  
  models[[paste(tourn, gender, "first_speed", sep = "_")]]  <-
    glm(form_speed,  data = first,  family = binomial)
  models[[paste(tourn, gender, "first_ratio", sep = "_")]]  <-
    glm(form_ratio,  data = first,  family = binomial)
  models[[paste(tourn, gender, "second_speed", sep = "_")]] <-
    glm(form_speed,  data = second, family = binomial)
  models[[paste(tourn, gender, "second_ratio", sep = "_")]] <-
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
                       # "spline",    # removed if desired
                       type, sep = "_"),
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

write.csv(oos_results, "oos_results_all_linear.csv")

## -------- 0. FILE LOCATIONS -------------------------------------------
## rerun for usopen male

rm(list = ls())

## -------- 0. FILE LOCATIONS -------------------------------------------
path_train_u_m <- "out_data/scaled/usopen_subset_m_training.csv"
path_oos_u_m   <- "out_data/scaled/usopen_subset_m_testing.csv"

library(tidyverse)   # dplyr / readr / ggplot2 …
library(data.table)  # fread
library(splines)     # bs()
library(yardstick)   # log_loss_vec()
library(purrr) 

## -------- 1. Load & clean data -----------------------------------------
df_train <- fread(path_train_u_m) %>% filter(Speed_MPH > 0)
df_test  <- fread(path_oos_u_m)   %>% filter(Speed_MPH > 0)

## -------- 2. Model formulas (linear versions shown here) ---------------
form_speed <- serving_player_won ~ p_server_beats_returner_z +
  ElapsedSeconds_fixed_z + importance_z + Speed_MPH_z +
  df_pct_server_z + factor(ServeWidth) + factor(ServeDepth)

form_ratio <- serving_player_won ~ p_server_beats_returner_z +
  ElapsedSeconds_fixed_z + importance_z + speed_ratio_z +
  df_pct_server_z + factor(ServeWidth) + factor(ServeDepth)

## -------- 3. Fit models ------------------------------------------------
first_train  <- df_train[df_train$ServeNumber == 1]
second_train <- df_train[df_train$ServeNumber == 2]

models <- list(
  first_speed  = glm(form_speed, data = first_train,  family = binomial),
  first_ratio  = glm(form_ratio, data = first_train,  family = binomial),
  second_speed = glm(form_speed, data = second_train, family = binomial),
  second_ratio = glm(form_ratio, data = second_train, family = binomial)
)

## -------- 4. Scoring helper --------------------------------------------
prepare_newdata <- function(model, df) {
  for (fac in names(model$xlevels)) {
    if (fac %in% names(df)) {
      df[[fac]] <- factor(df[[fac]], levels = model$xlevels[[fac]])
    }
  }
  df
}

score_model <- function(model, df, label) {
  if (nrow(df) == 0) return(NULL)
  df <- prepare_newdata(model, df)
  probs <- predict(model, newdata = df, type = "response")
  
  keep  <- !is.na(probs) & !is.na(df$serving_player_won)
  if (sum(keep) == 0) return(tibble(model = label, n = 0, accuracy = NA, log_loss = NA))
  
  probs <- probs[keep]
  truth <- df$serving_player_won[keep]
  
  acc <- mean((probs > 0.5) == truth)
  
  eps <- 1e-15
  probs_clipped <- pmin(pmax(probs, eps), 1 - eps)
  ll <- -mean(truth * log(probs_clipped) + (1 - truth) * log(1 - probs_clipped))
  
  tibble(model = label, n = length(truth),
         accuracy = round(acc, 4),
         log_loss = round(ll, 4))
}

## -------- 5. Evaluate on US Open Male test set --------------------------
first_test  <- df_test[df_test$ServeNumber == 1]
second_test <- df_test[df_test$ServeNumber == 2]

results <- bind_rows(
  score_model(models$first_speed,  first_test,  "usopen_m_first_speed"),
  score_model(models$first_ratio,  first_test,  "usopen_m_first_ratio"),
  score_model(models$second_speed, second_test, "usopen_m_second_speed"),
  score_model(models$second_ratio, second_test, "usopen_m_second_ratio")
)

print(results)

write.csv(results, "oos_results_usopen_m_linear.csv", row.names = FALSE)
