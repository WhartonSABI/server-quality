# rm(list = ls())
# 
# library(data.table)
# library(tidyverse)      # dplyr, purrr, tibble, ggplot2
# library(e1071)          # svm()
# library(yardstick)      # accuracy_vec(), mn_log_loss_vec()
# library(viridisLite)    # nice continuous palettes (scale_fill_viridis_c)
# 
# ## ---------------------------------------------------------------------
# 
# wimbledon_m_train <- as.data.table(fread("../data/output/scaled-results/wimbledon_m_train_scaled.csv"))
# wimbledon_m_test <- as.data.table(fread("../data/output/scaled-results/wimbledon_m_test_scaled.csv"))
# 
# names(wimbledon_m_train)
# 
# # filter ServeNumber == 2 from wimbledon_m_train
# wimbledon_m_train <- wimbledon_m_train[ServeNumber == 2]
# wimbledon_m_train <- wimbledon_m_test[ServeNumber == 2]
# 
# 
# # predictors we will use
# vars_num   <- c("importance",
#                 "p_server_beats_returner",
#                 "Speed_MPH",
#                 "ElapsedSeconds_fixed")
# vars_fact  <- c("ServeWidth", "ServeDepth")
# 
# # ensure factors have identical levels in both sets
# wimbledon_m_train[, (vars_fact) := lapply(.SD, factor), .SDcols = vars_fact]
# 
# for (v in vars_fact) {
#   wimbledon_m_test[ , (v) := factor(get(v), levels = levels(wimbledon_m_train[[v]])) ]
# }
# 
# # response as a factor
# wimbledon_m_train[ , serving_player_won := factor(serving_player_won,
#                                                   levels = c(0, 1))]
# wimbledon_m_test [ , serving_player_won := factor(serving_player_won,
#                                                   levels = c(0, 1)) ]
# 
# # build modelling tables
# train_dat <- wimbledon_m_train[ , c(vars_num, vars_fact, "serving_player_won"), with = FALSE ]
# test_dat  <- wimbledon_m_test [ , c(vars_num, vars_fact                 ), with = FALSE ]
# 
# ## 3  Tune radial SVM
# ### try to make it run faster
# set.seed(42)
# # sample at most 10 000 rows (or whatever fits in RAM quickly)
# n_tune <- min(10000, nrow(train_dat))
# tune_idx <- sample(seq_len(nrow(train_dat)), n_tune)
# train_tune <- train_dat[tune_idx]
# 
# ## much smaller grid & 3-fold CV
# grid_small <- list(
#   cost  = c(0.1, 1, 10),          # coarse search
#   gamma = c(0.01, 0.1, 1)
# )
# 
# tuned <- tune(
#   svm,
#   serving_player_won ~ .,
#   data    = train_tune,
#   kernel  = "linear",
#   ranges  = grid_small,
#   tunecontrol = tune.control(cross = 3)      # 3-fold instead of 5
# )
# 
# best_par <- tuned$best.parameters
# 
# best_svm <- svm(
#   serving_player_won ~ .,
#   data  = train_dat,               # *all* rows now
#   kernel = "linear",
#   cost   = best_par$cost,
#   gamma  = best_par$gamma,
#   probability = TRUE
# )
# 
# ## ------------------------------------------------------------------
# # ## using whole training dataset
# # set.seed(123)   # reproducibility
# # 
# # tuned <- tune(
# #   svm,
# #   serving_player_won ~ .,
# #   data    = train_dat,
# #   kernel  = "radial",
# #   ranges  = list(
# #     cost  = c(0.01, 0.1, 1, 10, 50),
# #     gamma = c(0.01, 0.1, 0.5, 1, 2)
# #   ),
# #   tunecontrol = tune.control(cross = 5)   # 5-fold CV
# # )
# # 
# # best_svm <- tuned$best.model
# # print(best_svm)
# 
# ## ------------------------------------------------------------------
# ## 4  Predict on held-out test data
# ## ------------------------------------------------------------------
# # class labels
# svm_pred_class <- predict(best_svm, newdata = test_dat)
# 
# # class probabilities
# svm_pred_prob  <- attr(
#   predict(best_svm, newdata = test_dat, probability = TRUE),
#   "probabilities"
# )
# 
# ## 5  evaluate accuracy (if y present in test set)
# ## ------------------------------------------------------------------
# if ("serving_player_won" %in% names(wimbledon_m_test)) {
#   test_truth <- wimbledon_m_test$serving_player_won
#   acc <- mean(svm_pred_class == test_truth)
#   cat(sprintf("\nMale, Wimbledon, second serves, Speed_MPH:\nTest accuracy: %.3f\n", acc))
#   
#   # other metrics
#   library(tibble)
#   library(yardstick)
#   res <- tibble(
#     truth = test_truth,
#     pred  = svm_pred_class,
#     prob1 = svm_pred_prob[ , "1"]  # prob of class "1"
#   )
#   
#   # logloss <- mn_log_loss(res, truth, prob1)$.estimate
#   # cat(sprintf("Log-loss: %.4f\n", logloss))
#   
#   fast_logloss <- function(truth, prob1, eps = 1e-15) {
#     # squeeze probabilities away from 0/1 to avoid log(0)
#     prob1 <- pmin(pmax(prob1, eps), 1 - eps)
#     y <- as.numeric(truth) - 1          # factor {0,1} → {0,1}
#     -mean( y * log(prob1) + (1 - y) * log(1 - prob1) )
#   }
#   
#   logloss <- fast_logloss(test_truth, svm_pred_prob[ , "1"])
#   cat(sprintf("Log-loss: %.4f\n", logloss))
#   # 
# 
# }
# 


rm(list=ls())

############################################################
## 0.  Packages & helper
############################################################
library(data.table)
library(e1071)      # svm(), tune()
library(tidyverse)  # dplyr, purrr, tibble …

## fast vectorised log-loss
fast_logloss <- function(truth, prob1, eps = 1e-15) {
  prob1 <- pmin(pmax(prob1, eps), 1 - eps)
  y     <- as.numeric(truth) - 1    # factor {0,1} ➔ 0/1
  -mean(y * log(prob1) + (1 - y) * log(1 - prob1))
}

############################################################
## 1.  Load data
############################################################
paths_train <- list(M = "../data/output/scaled-results/wimbledon_m_train_scaled.csv",
                    F = "../data/output/scaled-results/wimbledon_f_train_scaled.csv")
paths_test  <- list(M = "../data/output/scaled-results/wimbledon_m_test_scaled.csv",
                    F = "../data/output/scaled-results/wimbledon_f_test_scaled.csv")

train_list <- map(paths_train, ~ as.data.table(fread(.x))[Speed_MPH > 0])
test_list  <- map(paths_test,  ~ as.data.table(fread(.x))[Speed_MPH > 0])

############################################################
## 2.  Settings
############################################################
base_num   <- c("importance_z", "p_server_beats_returner_z", "ElapsedSeconds_fixed_z", "df_pct_server_z")
vars_fact  <- c("ServeWidth", "ServeDepth")

grid_small <- list(                           # quick 3×3 grid
  cost  = c(0.1, 1, 10),
  gamma = c(0.01, 0.1, 1)
)

set.seed(42)                                  # global reproducibility
results <- list()                             # will grow row-wise

############################################################
## 3.  Main loop: gender × serve# × speed var
############################################################
for (g in names(train_list)) {
  train0 <- train_list[[g]]
  test0  <- test_list [[g]]
  
  for (serve in c(1, 2)) {
    ## keep only first- or second-serves
    train_sub <- train0[ServeNumber == serve]
    test_sub  <- test0 [ServeNumber == serve]
    
    ## harmonise factor levels
    train_sub[, (vars_fact) := lapply(.SD, factor), .SDcols = vars_fact]
    for (v in vars_fact)
      test_sub[, (v) := factor(get(v), levels = levels(train_sub[[v]]))]
    
    ## response as factor
    train_sub[, serving_player_won := factor(serving_player_won, levels = c(0, 1))]
    test_sub [, serving_player_won := factor(serving_player_won, levels = c(0, 1))]
    
    for (speed_var in c("Speed_MPH_z", "speed_ratio_z")) {
      vars_num <- c(base_num, speed_var)
      
      ## modelling frames
      train_dat <- train_sub[, c(vars_num, vars_fact, "serving_player_won"), with = FALSE]
      test_dat  <- test_sub [, c(vars_num, vars_fact), with = FALSE]
      
      ## -------- 3-fold tune on ≤10 000 rows ---------------------------
      n_tune   <- min(10000, nrow(train_dat))
      tune_dat <- train_dat[sample(nrow(train_dat), n_tune)]
      
      tuned <- tune(
        svm,
        serving_player_won ~ .,
        data        = tune_dat,
        kernel      = "linear",
        ranges      = grid_small,
        tunecontrol = tune.control(cross = 3)
      )
      bp <- tuned$best.parameters
      
      ## -------- Fit best model on full subset -------------------------
      fit <- svm(
        serving_player_won ~ .,
        data        = train_dat,
        kernel      = "linear",
        cost        = bp$cost,
        gamma       = bp$gamma,
        probability = TRUE
      )
      
      ## -------- Evaluate on test subset -------------------------------
      pred_class <- predict(fit, newdata = test_dat)
      pred_prob  <- attr(predict(fit, newdata = test_dat, probability = TRUE),
                         "probabilities")[, "1"]
      
      acc <- mean(pred_class == test_sub$serving_player_won)
      ll  <- fast_logloss(test_sub$serving_player_won, pred_prob)
      
      ## -------- Store result ------------------------------------------
      results[[length(results) + 1]] <- list(
        gender         = g,
        serve_number   = serve,
        speed_variable = speed_var,
        test_accuracy  = acc,
        log_loss       = ll
      )
    }
  }
}

############################################################
## 4.  Results data-frame
############################################################
results_df <- bind_rows(results) %>% 
  arrange(gender, serve_number, speed_variable)

print(results_df)
# Save results to CSV
write.csv(results_df, "svm_results_wimbledon.csv", row.names = FALSE)

