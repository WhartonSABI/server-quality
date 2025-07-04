rm(list = ls())

## -------- 0. FILE LOCATIONS -------------------------------------------
path_train_w_m_scaled <- "scaled/wimbledon_m_train_scaled.csv" # 2021–2024 data
path_train_w_f_scaled <- "scaled/wimbledon_f_train_scaled.csv"
path_train_u_m_scaled <- "scaled/usopen_m_train_scaled.csv"
path_train_u_f_scaled <- "scaled/usopen_f_train_scaled.csv"

path_oos_w_m_scaled   <- "scaled/wimbledon_m_test_scaled.csv"  # 2018–2019 data
path_oos_w_f_scaled   <- "scaled/wimbledon_f_test_scaled.csv"
path_oos_u_m_scaled   <- "scaled/usopen_m_test_scaled.csv"
path_oos_u_f_scaled   <- "scaled/usopen_f_test_scaled.csv"

## -------- 1. LIBRARIES -------------------------------------------
library(tidyverse)   # dplyr / readr / ggplot2 / tibble
library(data.table)  # fread
library(splines)     # bs()
library(xgboost)     # gradient boosting
library(ggplot2)     # plotting
library(tibble)
library(purrr)

## -------- 2. READ & PREP TRAINING DATA -------------------------
train_sets <- list(
  wimbledon_m = fread(path_train_w_m_scaled),
  wimbledon_f = fread(path_train_w_f_scaled),
  usopen_m    = fread(path_train_u_m_scaled),
  usopen_f    = fread(path_train_u_f_scaled)
)

# ensure factors are truly factors (and set an explicit level order if you like)
make_factors <- function(df) {
  df$ServeWidth <- factor(df$ServeWidth)   # e.g. levels = c("T", "Body", "Wide")
  df$ServeDepth <- factor(df$ServeDepth)   # e.g. levels = c("Short", "Medium", "Long")
  df
}

train_sets <- map(train_sets, ~{
  if (!"ElapsedSeconds_fixed" %in% names(.x)) .x$ElapsedSeconds_fixed <- .x$ElapsedSeconds
  make_factors(.x)
})

## --- update the modeling formula ---

form_speed <- serving_player_won ~
  p_server_beats_returner +
  ElapsedSeconds_fixed +
  importance +
  Speed_MPH +          # numeric
  ServeWidth +           # now factors → one-hot via model.matrix()
  ServeDepth             # same


## -------- 4. HELPER TO BUILD XGB DMatrix -----------------------
make_dtrain <- function(df) {
  X <- model.matrix(form_speed, data = df)[, -1]
  y <- df$serving_player_won
  xgb.DMatrix(data = as.matrix(X), label = y)
}

## -------- 5. BUILD & TRAIN XGB MODELS --------------------------
# First serves
dtrain_wm1 <- make_dtrain(train_sets$wimbledon_m %>% filter(ServeNumber == 1))
dtrain_wf1 <- make_dtrain(train_sets$wimbledon_f %>% filter(ServeNumber == 1))
dtrain_um1 <- make_dtrain(train_sets$usopen_m    %>% filter(ServeNumber == 1))
dtrain_uf1 <- make_dtrain(train_sets$usopen_f    %>% filter(ServeNumber == 1))

# Second serves
dtrain_wm2 <- make_dtrain(train_sets$wimbledon_m %>% filter(ServeNumber == 2))
dtrain_wf2 <- make_dtrain(train_sets$wimbledon_f %>% filter(ServeNumber == 2))
dtrain_um2 <- make_dtrain(train_sets$usopen_m    %>% filter(ServeNumber == 2))
dtrain_uf2 <- make_dtrain(train_sets$usopen_f    %>% filter(ServeNumber == 2))

params <- list(
  objective   = "binary:logistic",
  eval_metric = c("logloss", "auc")
)

set.seed(123)
xgb_wm1 <- xgb.train(params = params, data = dtrain_wm1, nrounds = 100, verbose = 0)
xgb_wf1 <- xgb.train(params = params, data = dtrain_wf1, nrounds = 100, verbose = 0)
xgb_um1 <- xgb.train(params = params, data = dtrain_um1, nrounds = 100, verbose = 0)
xgb_uf1 <- xgb.train(params = params, data = dtrain_uf1, nrounds = 100, verbose = 0)

xgb_wm2 <- xgb.train(params = params, data = dtrain_wm2, nrounds = 100, verbose = 0)
xgb_wf2 <- xgb.train(params = params, data = dtrain_wf2, nrounds = 100, verbose = 0)
xgb_um2 <- xgb.train(params = params, data = dtrain_um2, nrounds = 100, verbose = 0)
xgb_uf2 <- xgb.train(params = params, data = dtrain_uf2, nrounds = 100, verbose = 0)

## -------- 6. READ & PREP TEST DATA -----------------------------
test_sets <- list(
  wimbledon_m = fread(path_oos_w_m_scaled),
  wimbledon_f = fread(path_oos_w_f_scaled),
  usopen_m    = fread(path_oos_u_m_scaled),
  usopen_f    = fread(path_oos_u_f_scaled)
)

# same for test sets (do this right after you read them in):
test_sets <- map(test_sets, ~{
  if (!"ElapsedSeconds_fixed" %in% names(.x)) .x$ElapsedSeconds_fixed <- .x$ElapsedSeconds
  make_factors(.x)
})

# ## -------- 7. HELPER TO COMPUTE ACCURACY ------------------------
# compute_acc <- function(model, df) {
#   X <- model.matrix(form_speed, data = df)[, -1] %>% as.matrix()
#   p <- predict(model, X)
#   mean((p >= 0.5) == df$serving_player_won)
# }
# 
# ## -------- 8. PREDICT & PRINT ACCURACY FOR ALL 8 MODELS ---------
# # Wimbledon Men
# cat("Wimbledon Men 1st-serve XGB accuracy:", round(compute_acc(xgb_wm1, test_sets$wimbledon_m %>% filter(ServeNumber == 1)), 4), "\n")
# cat("Wimbledon Men 2nd-serve XGB accuracy:", round(compute_acc(xgb_wm2, test_sets$wimbledon_m %>% filter(ServeNumber == 2)), 4), "\n")
# 
# # Wimbledon Women
# cat("Wimbledon Women 1st-serve XGB accuracy:", round(compute_acc(xgb_wf1, test_sets$wimbledon_f %>% filter(ServeNumber == 1)), 4), "\n")
# cat("Wimbledon Women 2nd-serve XGB accuracy:", round(compute_acc(xgb_wf2, test_sets$wimbledon_f %>% filter(ServeNumber == 2)), 4), "\n")
# 
# # US Open Men
# cat("US Open Men 1st-serve XGB accuracy:", round(compute_acc(xgb_um1, test_sets$usopen_m %>% filter(ServeNumber == 1)), 4), "\n")
# cat("US Open Men 2nd-serve XGB accuracy:", round(compute_acc(xgb_um2, test_sets$usopen_m %>% filter(ServeNumber == 2)), 4), "\n")
# 
# # US Open Women
# cat("US Open Women 1st-serve XGB accuracy:", round(compute_acc(xgb_uf1, test_sets$usopen_f %>% filter(ServeNumber == 1)), 4), "\n")
# cat("US Open Women 2nd-serve XGB accuracy:", round(compute_acc(xgb_uf2, test_sets$usopen_f %>% filter(ServeNumber == 2)), 4), "\n")

## -------- 9.  LOG-LOSS + ACCURACY HELPERS ----------------------
log_loss_bin <- function(actual, prob, eps = 1e-15) {
  prob <- pmin(pmax(prob, eps), 1 - eps)
  -mean(actual * log(prob) + (1 - actual) * log(1 - prob))
}

compute_metrics <- function(model, df) {
  X  <- model.matrix(form_speed, data = df)[, -1] |> as.matrix()
  pr <- predict(model, X)                       # win-probability
  y  <- df$serving_player_won
  tibble(
    accuracy = mean((pr >= 0.5) == y),
    log_loss = log_loss_bin(y, pr)
  )
}

## -------- 10.  EVALUATE ALL 8 MODELS ---------------------------
results <- list(
  "WIM men 1st"   = compute_metrics(xgb_wm1, test_sets$wimbledon_m |> filter(ServeNumber == 1)),
  "WIM men 2nd"   = compute_metrics(xgb_wm2, test_sets$wimbledon_m |> filter(ServeNumber == 2)),
  "WIM women 1st" = compute_metrics(xgb_wf1, test_sets$wimbledon_f |> filter(ServeNumber == 1)),
  "WIM women 2nd" = compute_metrics(xgb_wf2, test_sets$wimbledon_f |> filter(ServeNumber == 2)),
  
  "USO men 1st"   = compute_metrics(xgb_um1, test_sets$usopen_m    |> filter(ServeNumber == 1)),
  "USO men 2nd"   = compute_metrics(xgb_um2, test_sets$usopen_m    |> filter(ServeNumber == 2)),
  "USO women 1st" = compute_metrics(xgb_uf1, test_sets$usopen_f    |> filter(ServeNumber == 1)),
  "USO women 2nd" = compute_metrics(xgb_uf2, test_sets$usopen_f    |> filter(ServeNumber == 2))
)

## -------- 11.  COMBINE INTO A DATA-FRAME -----------------------

metrics_df <- imap_dfr(
  results,
  ~ tibble(event = .y, accuracy = .x$accuracy, log_loss = .x$log_loss)
)

print(metrics_df)

write.csv(metrics_df, "xgb_accuracy_results.csv", row.names = F)
