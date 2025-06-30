# --- Libraries and Setup ---
rm(list=ls())
library(tidyverse)
library(data.table)
library(ggplot2)
library(car)
library(splines)
library(dplyr)
library(rsample)
library(purrr)
library(yardstick)

set.seed(123)

# --- Load data ---
subset_m <- fread("scaled/usopen_m_train_scaled.csv")
subset_f <- fread("scaled/usopen_f_train_scaled.csv")

# --- Split subsets ---
m_first <- subset_m[ServeNumber == 1]
m_second <- subset_m[ServeNumber == 2]
f_first <- subset_f[ServeNumber == 1]
f_second <- subset_f[ServeNumber == 2]

# --- Helper: Evaluate model on train/test split ---
evaluate_split <- function(df, formula) {
  split <- initial_split(df, prop = 0.80, strata = serving_player_won)
  train <- training(split)
  test  <- testing(split)
  
  model <- glm(formula, data = train, family = binomial)
  p_hat <- predict(model, newdata = test, type = "response")
  y     <- test$serving_player_won
  
  class_pred <- ifelse(p_hat > 0.5, 1, 0)
  accuracy   <- mean(class_pred == y)
  
  eps <- 1e-15
  p_hat_clip <- pmin(pmax(p_hat, eps), 1 - eps)
  logloss <- -mean(y * log(p_hat_clip) + (1 - y) * log(1 - p_hat_clip))
  
  list(model = model, accuracy = accuracy, logloss = logloss)
}

# --- Helper: Build formulas by adding one variable at a time ---
build_formulas <- function(speed_var) {
  predictors <- c(
    speed_var,
    "p_server_beats_returner",
    "ElapsedSeconds_fixed",
    "importance",
    "factor(ServeWidth)",
    "factor(ServeDepth)"
  )
  
  formulas <- map(seq_along(predictors), function(i) {
    rhs <- paste(predictors[1:i], collapse = " + ")
    as.formula(paste("serving_player_won ~", rhs))
  })
  
  return(formulas)
}

# --- Model groups and predictors ---
groups <- list(
  m_first  = m_first,
  m_second = m_second,
  f_first  = f_first,
  f_second = f_second
)

# --- Run model evaluations for both speed and ratio versions ---
results_df <- map_dfr(names(groups), function(group_name) {
  df <- groups[[group_name]]
  
  map_dfr(c("Speed_MPH", "speed_ratio"), function(var) {
    formulas <- build_formulas(var)
    
    map_dfr(seq_along(formulas), function(i) {
      res <- evaluate_split(df, formulas[[i]])
      tibble(
        group = group_name,
        model_type = var,
        step = i,
        formula = paste(deparse(formulas[[i]]), collapse = ""),
        accuracy = round(res$accuracy, 4),
        log_loss = round(res$logloss, 4)
      )
    })
  })
})

# --- Output results ---
print(results_df)
write.csv(results_df, "results_usopen_stepwise_insample.csv", row.names = FALSE)
