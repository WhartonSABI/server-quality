rm(list = ls())

## -------- 0. FILE LOCATIONS -------------------------------------------
path_train_w_m_scaled <- "wimbledon_m_train_scaled.csv" # 2021–2024 data
path_train_w_f_scaled <- "wimbledon_f_train_scaled.csv"
path_train_u_m_scaled <- "usopen_m_train_scaled.csv"
path_train_u_f_scaled <- "usopen_f_train_scaled.csv"

path_oos_w_m_scaled   <- "wimbledon_m_test_scaled.csv"  # 2018–2019 data
path_oos_w_f_scaled   <- "wimbledon_f_test_scaled.csv"
path_oos_u_m_scaled   <- "usopen_m_test_scaled.csv"
path_oos_u_f_scaled   <- "usopen_f_test_scaled.csv"

## -------- 1. LIBRARIES -------------------------------------------
library(tidyverse)   # dplyr / readr / ggplot2 / tibble
library(data.table)  # fread
library(splines)     # bs()
library(xgboost)     # gradient boosting
library(ggplot2)     # plotting

## -------- 2. READ & PREP TRAINING DATA -------------------------
train_sets <- list(
  wimbledon_m = fread(path_train_w_m_scaled),
  wimbledon_f = fread(path_train_w_f_scaled),
  usopen_m    = fread(path_train_u_m_scaled),
  usopen_f    = fread(path_train_u_f_scaled)
)

# ensure ElapsedSeconds_fixed exists
train_sets <- map(train_sets, ~{
  if (!"ElapsedSeconds_fixed" %in% names(.x)) {
    .x$ElapsedSeconds_fixed <- .x$ElapsedSeconds
  }
  .x
})

## -------- 3. DEFINE FORMULA -------------------------------------
form_speed <- serving_player_won ~
  p_server_beats_returner +
  ElapsedSeconds_fixed +
  importance +
  Speed_MPH

## -------- 4. BUILD TRAINING MATRIX & LABEL ---------------------
df_train <- train_sets$wimbledon_m %>% filter(ServeNumber == 1)

X_train <- model.matrix(form_speed, data = df_train)[, -1]          # drop intercept
y_train <- df_train$serving_player_won

dtrain <- xgb.DMatrix(data = as.matrix(X_train), label = y_train)

## -------- 5. TRAIN XGBOOST MODEL --------------------------------
params <- list(
  objective   = "binary:logistic",
  eval_metric = c("logloss", "auc")
)

set.seed(123)
xgb_mod <- xgb.train(
  params    = params,
  data      = dtrain,
  nrounds   = 100,
  verbose   = 0
)


## -------- 7. READ & PREP TEST DATA -----------------------------
test_sets <- list(
  wimbledon_m = fread(path_oos_w_m_scaled),
  wimbledon_f = fread(path_oos_w_f_scaled),
  usopen_m    = fread(path_oos_u_m_scaled),
  usopen_f    = fread(path_oos_u_f_scaled)
)

test_sets <- map(test_sets, ~{
  if (!"ElapsedSeconds_fixed" %in% names(.x)) {
    .x$ElapsedSeconds_fixed <- .x$ElapsedSeconds
  }
  .x
})

## -------- 8. PREDICT ON TEST SET --------------------------------
df_test <- test_sets$wimbledon_m %>% filter(ServeNumber == 1)

X_test <- model.matrix(form_speed, data = df_test)[, -1] %>%
  as.matrix()

pred_prob <- predict(xgb_mod, X_test)

df_test <- df_test %>% mutate(p_win = pred_prob)

## -------- 9. INSPECT PREDICTIONS ---------------------------------
head(df_test %>% select(ServeNumber, Speed_MPH, serving_player_won, p_win))

## -------- 10. OPTIONAL: SMALL DF & ACCURACY ---------------------
df_small <- df_test %>%
  select(
    player1,
    player2,
    Speed_MPH,
    PointServer,
    ServeNumber,
    serving_player_won,
    p_win,
    PointWinner
  )

df_test <- df_test %>%
  mutate(pred_win = ifelse(p_win >= 0.5, 1L, 0L))

accuracy <- mean(df_test$pred_win == df_test$serving_player_won)
print(accuracy)


## -------- 6. FEATURE IMPORTANCE ---------------------------------
imp <- xgb.importance(model = xgb_mod)
imp_df <- as_tibble(imp) %>%
  arrange(desc(Gain))

ggplot(imp_df, aes(x = reorder(Feature, Gain), y = Gain)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "XGBoost Feature Importance\nWimbledon Men — First Serve",
    x     = NULL,
    y     = "Gain"
  ) +
  theme_minimal()
