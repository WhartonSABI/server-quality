rm(list=ls())
set.seed(123)
## -------- 0. FILE LOCATIONS -------------------------------------------
path_train_w_m_scaled <- "out_data/scaled/wimbledon_subset_m_training.csv" # 2021–2024 data
path_train_w_f_scaled <- "out_data/scaled/wimbledon_subset_f_training.csv"
path_train_u_m_scaled <- "out_data/scaled/usopen_subset_m_training.csv"
path_train_u_f_scaled <- "out_data/scaled/usopen_subset_f_training.csv"

path_oos_w_m_scaled   <- "out_data/scaled/wimbledon_subset_m_testing.csv" # 2018–2019 data
path_oos_w_f_scaled   <- "out_data/scaled/wimbledon_subset_f_testing.csv"
path_oos_u_m_scaled   <- "out_data/scaled/usopen_subset_m_testing.csv"
path_oos_u_f_scaled   <- "out_data/scaled/usopen_subset_f_testing.csv"

library(tidyverse)   # dplyr / readr / ggplot2 / tibble …
library(data.table)  # fread
library(splines)     # bs()
library(ranger)      # random forest
library(ggplot2)     # plotting
library(tibble)

# 1) read in your training sets
train_sets <- list(
  wimbledon_m = fread(path_train_w_m_scaled),
  wimbledon_f = fread(path_train_w_f_scaled),
  usopen_m    = fread(path_train_u_m_scaled),
  usopen_f    = fread(path_train_u_f_scaled)
)

## ensure factors are truly factors (and set an explicit level order if you like)
make_factors <- function(df) {
  df$ServeWidth <- factor(df$ServeWidth)   # e.g. levels = c("T", "Body", "Wide")
  df$ServeDepth <- factor(df$ServeDepth)   # e.g. levels = c("Short", "Medium", "Long")
  df
}

train_sets <- map(train_sets, ~{
  if (!"ElapsedSeconds_fixed" %in% names(.x)) .x$ElapsedSeconds_fixed <- .x$ElapsedSeconds
  make_factors(.x)
})


# 3) define the “speed” formula
form_speed <- serving_player_won ~
  p_server_beats_returner_z +
  ElapsedSeconds_fixed_z +
  importance_z +
  df_pct_server_z +
  Speed_MPH_z + ## swap out Speed_MPH and speed_ratio as desired 
  ServeWidth +           # now factors → one-hot via model.matrix()
  ServeDepth             # same

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
  labs(title = "RF Variable Importance (using Speed_MPH)\nWimbledon Men — 1st Serve", x = NULL, y = "Importance") +
  theme_minimal()

#################### TEST ####################

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

# # ─── ACCURACY COMPUTATION FOR ALL FOUR EVENTS ───
# 
# compute_acc <- function(model, df) {
#   X <- model.matrix(form_speed, data = df)[, -1] %>% as.data.frame()
#   p <- predict(model, data = X)$predictions[,2]
#   mean((p >= 0.5) == df$serving_player_won)
# }
# 
# # Wimbledon
# cat("Wimbledon Men 1st-serve accuracy:",    round(compute_acc(rf_mod_m1, test_sets$wimbledon_m %>% filter(ServeNumber == 1)), 4), "\n")
# cat("Wimbledon Women 1st-serve accuracy:",  round(compute_acc(rf_mod_f1, test_sets$wimbledon_f %>% filter(ServeNumber == 1)), 4), "\n")
# cat("Wimbledon Men 2nd-serve accuracy:",    round(compute_acc(rf_mod_m2, test_sets$wimbledon_m %>% filter(ServeNumber == 2)), 4), "\n")
# cat("Wimbledon Women 2nd-serve accuracy:",  round(compute_acc(rf_mod_f2, test_sets$wimbledon_f %>% filter(ServeNumber == 2)), 4), "\n")
# 
# # US Open
# cat("US Open Men 1st-serve accuracy:",      round(compute_acc(rf_mod_m1, test_sets$usopen_m  %>% filter(ServeNumber == 1)), 4), "\n")
# cat("US Open Women 1st-serve accuracy:",    round(compute_acc(rf_mod_f1, test_sets$usopen_f  %>% filter(ServeNumber == 1)), 4), "\n")
# cat("US Open Men 2nd-serve accuracy:",      round(compute_acc(rf_mod_m2, test_sets$usopen_m  %>% filter(ServeNumber == 2)), 4), "\n")
# cat("US Open Women 2nd-serve accuracy:",    round(compute_acc(rf_mod_f2, test_sets$usopen_f  %>% filter(ServeNumber == 2)), 4), "\n")

# ─── Helper: binary log-loss ───
log_loss_bin <- function(actual, prob, eps = 1e-15) {
  prob <- pmin(pmax(prob, eps), 1 - eps)   # guard against log(0)
  -mean(actual * log(prob) + (1 - actual) * log(1 - prob))
}

# ─── Helper: compute accuracy *and* log-loss for one model/data frame ───
compute_metrics <- function(model, df) {
  X  <- model.matrix(form_speed, data = df)[, -1] |> as.data.frame()
  pr <- predict(model, data = X)$predictions[, 2]          # P(win)
  y  <- df$serving_player_won
  acc <- mean((pr >= 0.5) == y)
  ll  <- log_loss_bin(y, pr)
  c(accuracy = acc, log_loss = ll)
}

# ─── Evaluate on all 8 combinations ───
results <- list(
  "WIM men 1st" = compute_metrics(rf_mod_m1, test_sets$wimbledon_m |> filter(ServeNumber == 1)),
  "WIM women 1st" = compute_metrics(rf_mod_f1, test_sets$wimbledon_f |> filter(ServeNumber == 1)),
  "WIM men 2nd" = compute_metrics(rf_mod_m2, test_sets$wimbledon_m |> filter(ServeNumber == 2)),
  "WIM women 2nd" = compute_metrics(rf_mod_f2, test_sets$wimbledon_f |> filter(ServeNumber == 2)),
  
  "USO men 1st" = compute_metrics(rf_mod_m1, test_sets$usopen_m   |> filter(ServeNumber == 1)),
  "USO women 1st" = compute_metrics(rf_mod_f1, test_sets$usopen_f |> filter(ServeNumber == 1)),
  "USO men 2nd" = compute_metrics(rf_mod_m2, test_sets$usopen_m   |> filter(ServeNumber == 2)),
  "USO women 2nd" = compute_metrics(rf_mod_f2, test_sets$usopen_f |> filter(ServeNumber == 2))
)

# pretty print
purrr::iwalk(results, \(v, name) {
  cat(sprintf("%-15s  accuracy = %.4f   log-loss = %.4f\n",
              name, v["accuracy"], v["log_loss"]))
})

metrics_df <- purrr::imap_dfr(
  results,
  ~ tibble(
    event     = .y,
    accuracy  = unname(.x["accuracy"]),
    log_loss  = unname(.x["log_loss"])
  )
)

metrics_df

write.csv(metrics_df, "rf_accuracy_results.csv", row.names = FALSE)
