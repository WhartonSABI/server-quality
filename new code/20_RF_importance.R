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

# 4) pick one slice, e.g. Wimbledon men first & second serves and Wimbledon women
df_m1  <- train_sets$wimbledon_m %>% filter(ServeNumber == 1)
df_f1  <- train_sets$wimbledon_f %>% filter(ServeNumber == 1)
df_m2  <- train_sets$wimbledon_m %>% filter(ServeNumber == 2)
df_f2  <- train_sets$wimbledon_f %>% filter(ServeNumber == 2)

Xm1 <- model.matrix(form_speed, data = df_m1)[, -1];  y_m1 <- df_m1$serving_player_won
Xf1 <- model.matrix(form_speed, data = df_f1)[, -1];  y_f1 <- df_f1$serving_player_won
Xm2 <- model.matrix(form_speed, data = df_m2)[, -1];  y_m2 <- df_m2$serving_player_won
Xf2 <- model.matrix(form_speed, data = df_f2)[, -1];  y_f2 <- df_f2$serving_player_won

rf_mod_m1 <- ranger(x = Xm1, y = y_m1, probability = TRUE, importance = "impurity", num.trees = 500)
rf_mod_f1 <- ranger(x = Xf1, y = y_f1, probability = TRUE, importance = "impurity", num.trees = 500)
rf_mod_m2 <- ranger(x = Xm2, y = y_m2, probability = TRUE, importance = "impurity", num.trees = 500)
rf_mod_f2 <- ranger(x = Xf2, y = y_f2, probability = TRUE, importance = "impurity", num.trees = 500)

# 7) optional: plot one importance
imp_df <- enframe(rf_mod_m1$variable.importance, name = "variable", value = "importance") %>%
  arrange(desc(importance))
ggplot(imp_df, aes(x = reorder(variable, importance), y = importance)) +
  geom_col() + coord_flip() +
  labs(title = "RF Variable Importance\nWimbledon Men — 1st Serve", x = NULL, y = "Importance") +
  theme_minimal()

#################### TEST ####################

test_sets <- list(
  wimbledon_m = fread(path_oos_w_m_scaled),
  wimbledon_f = fread(path_oos_w_f_scaled),
  usopen_m    = fread(path_oos_u_m_scaled),
  usopen_f    = fread(path_oos_u_f_scaled)
)

# ensure ElapsedSeconds_fixed exists
test_sets <- map(test_sets, function(df) {
  if (!"ElapsedSeconds_fixed" %in% names(df))
    df$ElapsedSeconds_fixed <- df$ElapsedSeconds
  df
})

# ─── ACCURACY COMPUTATION FOR ALL FOUR EVENTS ───

compute_acc <- function(model, df) {
  X <- model.matrix(form_speed, data = df)[, -1] %>% as.data.frame()
  p <- predict(model, data = X)$predictions[,2]
  mean((p >= 0.5) == df$serving_player_won)
}

# Wimbledon
cat("Wimbledon Men 1st-serve accuracy:",    round(compute_acc(rf_mod_m1, test_sets$wimbledon_m %>% filter(ServeNumber == 1)), 4), "\n")
cat("Wimbledon Women 1st-serve accuracy:",  round(compute_acc(rf_mod_f1, test_sets$wimbledon_f %>% filter(ServeNumber == 1)), 4), "\n")
cat("Wimbledon Men 2nd-serve accuracy:",    round(compute_acc(rf_mod_m2, test_sets$wimbledon_m %>% filter(ServeNumber == 2)), 4), "\n")
cat("Wimbledon Women 2nd-serve accuracy:",  round(compute_acc(rf_mod_f2, test_sets$wimbledon_f %>% filter(ServeNumber == 2)), 4), "\n")

# US Open
cat("US Open Men 1st-serve accuracy:",      round(compute_acc(rf_mod_m1, test_sets$usopen_m  %>% filter(ServeNumber == 1)), 4), "\n")
cat("US Open Women 1st-serve accuracy:",    round(compute_acc(rf_mod_f1, test_sets$usopen_f  %>% filter(ServeNumber == 1)), 4), "\n")
cat("US Open Men 2nd-serve accuracy:",      round(compute_acc(rf_mod_m2, test_sets$usopen_m  %>% filter(ServeNumber == 2)), 4), "\n")
cat("US Open Women 2nd-serve accuracy:",    round(compute_acc(rf_mod_f2, test_sets$usopen_f  %>% filter(ServeNumber == 2)), 4), "\n")
