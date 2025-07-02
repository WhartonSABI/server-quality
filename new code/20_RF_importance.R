rm(list=ls())
## -------- 0. FILE LOCATIONS -------------------------------------------
path_train_w_m_scaled <- "wimbledon_m_train_scaled.csv" # 2021–2024 data
path_train_w_f_scaled <- "wimbledon_f_train_scaled.csv"
path_train_u_m_scaled <- "usopen_m_train_scaled.csv"
path_train_u_f_scaled <- "usopen_f_train_scaled.csv"

path_oos_w_m_scaled   <- "wimbledon_m_test_scaled.csv" # 2018–2019 data
path_oos_w_f_scaled   <- "wimbledon_f_test_scaled.csv"
path_oos_u_m_scaled   <- "usopen_m_test_scaled.csv"
path_oos_u_f_scaled   <- "usopen_f_test_scaled.csv"

library(tidyverse)   # dplyr / readr / ggplot2 / tibble …
library(data.table)  # fread
library(splines)     # bs()
library(ranger)      # random forest
library(ggplot2)     # plotting

# 1) read in your training sets
train_sets <- list(
  wimbledon_m = fread(path_train_w_m_scaled),
  wimbledon_f = fread(path_train_w_f_scaled),
  usopen_m    = fread(path_train_u_m_scaled),
  usopen_f    = fread(path_train_u_f_scaled)
)

# 2) ensure ElapsedSeconds_fixed exists
train_sets <- map(train_sets, function(df) {
  if (!"ElapsedSeconds_fixed" %in% names(df))
    df$ElapsedSeconds_fixed <- df$ElapsedSeconds
  df
})

# 3) define the “speed” formula
form_speed <- serving_player_won ~
  p_server_beats_returner +
  ElapsedSeconds_fixed +
  importance +
  Speed_MPH 

# 4) pick one slice, e.g. Wimbledon men first serves
df <- train_sets$wimbledon_m %>%
  filter(ServeNumber == 1)

# 5) build design matrix & response
X <- model.matrix(form_speed, data = df)[, -1]      # drop intercept column
y <- df$serving_player_won

# 6) fit a probability forest with impurity‐based importances
rf_mod <- ranger(
  x           = X,
  y           = y,
  probability = TRUE,
  importance  = "impurity",
  num.trees   = 500
)

# 7) extract importances into a tibble
imp_df <- enframe(rf_mod$variable.importance, name = "variable", value = "importance") %>%
  arrange(desc(importance))

# 8) plot
ggplot(imp_df, aes(x = reorder(variable, importance), y = importance)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "RF Variable Importance\nWimbledon Men — First Serve (speed)",
    x     = NULL,
    y     = "Importance"
  ) +
  theme_minimal()


#################### TEST
test_sets <- list(
  wimbledon_m = fread(path_oos_w_m_scaled),
  wimbledon_f = fread(path_oos_w_f_scaled),
  usopen_m    = fread(path_oos_u_m_scaled),
  usopen_f    = fread(path_oos_u_f_scaled)
)

# ensure ElapsedSeconds_fixed exists
test_sets <- map(test_sets, function(df) {
  if (!"ElapsedSeconds_fixed" %in% names(df)) {
    df$ElapsedSeconds_fixed <- df$ElapsedSeconds
  }
  df
})

## -------- 6. PREDICT ON WIMBLEDON MEN FIRST SERVES TEST -------
df_test <- test_sets$wimbledon_m %>%
  filter(ServeNumber == 1)

X_test <- model.matrix(form_speed, data = df_test)[, -1] %>%
  as.data.frame()

pred_mat <- predict(rf_mod, data = X_test)$predictions

# column 1 = P(FALSE), column 2 = P(TRUE)
df_test <- df_test %>%
  mutate(
    p_win = pred_mat[, 2]
  )

## -------- 7. INSPECT PREDICTIONS ---------------------------------
head(df_test %>% select(ServeNumber, Speed_MPH, serving_player_won, p_win))
library(dplyr)

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
  mutate(
    pred_win = ifelse(p_win >= 0.5, 1L, 0L)
  )

# 2) compute accuracy as the fraction of correct predictions
accuracy <- mean(df_test$pred_win == df_test$serving_player_won)

# 3) print it
print(accuracy)

# 4) plot the distribution of predicted probabilities
