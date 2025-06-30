# --- Load libraries ---
rm(list=ls())
library(tidyverse)
library(data.table)
library(ggplot2)
library(car)
library(splines)
library(dplyr)
library(rsample) # initial_split(), training(), testing()
library(purrr) # map_dfr()
library(yardstick)

set.seed(123)

# --- Load data ---
subset_m <- fread("scaled/wimbledon_m_train_scaled.csv")
subset_f <- fread("scaled/wimbledon_f_train_scaled.csv")

# --- Split subsets ---
m_first <- subset_m[ServeNumber == 1]
m_second <- subset_m[ServeNumber == 2]
f_first <- subset_f[ServeNumber == 1]
f_second <- subset_f[ServeNumber == 2]

## helper --------------------------------------------------------------
evaluate_split <- function(df, formula) {
  # 1) 80/20 split, stratify on outcome to keep win-rate balance
  split  <- initial_split(df, prop = 0.80, strata = serving_player_won)
  train  <- training(split)
  test   <- testing(split)
  
  # 2) fit model on training data
  model  <- glm(formula, data = train, family = binomial)
  
  # 3) predict on test data
  p_hat  <- predict(model, newdata = test, type = "response")
  y      <- test$serving_player_won
  
  # 4) metrics ---------------------------------------------------------
  class_pred <- ifelse(p_hat > 0.5, 1, 0)
  accuracy   <- mean(class_pred == y)
  
  ## clip probs to avoid log(0)
  eps        <- 1e-15
  p_hat_clip <- pmin(pmax(p_hat, eps), 1 - eps)
  logloss    <- -mean(y * log(p_hat_clip) + (1 - y) * log(1 - p_hat_clip))
  
  list(model = model, accuracy = accuracy, logloss = logloss)
}

## formulas ------------------------------------------------------------
form_speed <- as.formula(
  serving_player_won ~ p_server_beats_returner + ElapsedSeconds_fixed + importance +
    bs(Speed_MPH, degree = 3, df = 5) + factor(ServeWidth) + factor(ServeDepth)
)
form_ratio <- as.formula(
  serving_player_won ~ p_server_beats_returner + ElapsedSeconds_fixed + importance +
    bs(speed_ratio, degree = 3, df = 5) + factor(ServeWidth) + factor(ServeDepth)
)

## data groups ---------------------------------------------------------
groups <- list(
  m_first  = m_first,
  m_second = m_second,
  f_first  = f_first,
  f_second = f_second
)

## run all eight evaluations ------------------------------------------
results_df <- map_dfr(names(groups), function(g) {
  df <- groups[[g]]
  
  map_dfr(c(speed = form_speed, ratio = form_ratio), function(fml) {
    res <- evaluate_split(df, fml)
    tibble(
      group      = g,
      model_type = if (identical(fml, form_speed)) "speed" else "ratio",
      accuracy   = round(res$accuracy, 4),
      log_loss   = round(res$logloss, 4)
    )
  })
})

print(results_df)

write.csv(results_df, "results_wimbledon_spline_insample_test.csv", row.names = FALSE)

#-----------------------------------------------------------------------
#-----------------------------------------------------------------------
# linear instead of spline

form_speed <- as.formula(
  serving_player_won ~ p_server_beats_returner +
    ElapsedSeconds_fixed +
    importance +
    Speed_MPH + 
    factor(ServeWidth) +
    factor(ServeDepth)
)

form_ratio <- as.formula(
  serving_player_won ~ p_server_beats_returner +
    ElapsedSeconds_fixed +
    importance +
    speed_ratio + 
    factor(ServeWidth) +
    factor(ServeDepth)
)

results_df <- map_dfr(names(groups), function(g) {
  df <- groups[[g]]
  
  map_dfr(c(speed = form_speed, ratio = form_ratio), function(fml) {
    res <- evaluate_split(df, fml)
    tibble(
      group      = g,
      model_type = if (identical(fml, form_speed)) "speed" else "ratio",
      accuracy   = round(res$accuracy, 4),
      log_loss   = round(res$logloss, 4)
    )
  })
})

print(results_df)
write.csv(results_df, "results_wimbledon_linear_insample_test.csv", row.names = FALSE)
