############################################################
## nn_usopen.R ― Neural-network benchmark for US-Open data
##  - Loops over gender, serve #, and speed variable
##  - 3-fold CV grid-search for (size, decay)
##  - Computes test accuracy & log loss
############################################################

## 0.  Packages & helpers ----------------------------------------------
library(data.table)
library(tidyverse)   # dplyr, purrr, tibble, …
library(nnet)        # feed-forward neural nets

## Fast log-loss (binary)
fast_logloss <- function(truth, prob1, eps = 1e-15) {
  prob1 <- pmin(pmax(prob1, eps), 1 - eps)
  y     <- as.numeric(truth) - 1           # factor {0,1} → 0/1
  -mean(y * log(prob1) + (1 - y) * log(1 - prob1))
}

## Scale numeric columns in train & apply same transform to test
scale_like <- function(train_df, test_df, num_vars) {
  means <- sapply(train_df[, num_vars], mean)
  sds   <- sapply(train_df[, num_vars], sd)
  train_df[, (num_vars) := Map(function(x, m, s) (x - m)/s,
                               .SD, means, sds),
           .SDcols = num_vars]
  test_df [, (num_vars) := Map(function(x, m, s) (x - m)/s,
                               .SD, means, sds),
           .SDcols = num_vars]
  list(train = train_df, test = test_df)
}

set.seed(42)  # global reproducibility

############################################################
## 1.  Load data --------------------------------------------------------
paths_train <- list(M = "scaled/wimbledon_m_train_scaled.csv",
                    F = "scaled/wimbledon_f_train_scaled.csv")
paths_test  <- list(M = "scaled/wimbledon_m_test_scaled.csv",
                    F = "scaled/wimbledon_f_test_scaled.csv")

train_list <- map(paths_train, ~ as.data.table(fread(.x)))
test_list  <- map(paths_test , ~ as.data.table(fread(.x)))

############################################################
## 2.  Settings ---------------------------------------------------------
base_num  <- c("importance", "p_server_beats_returner", "ElapsedSeconds_fixed")
vars_fact <- c("ServeWidth", "ServeDepth")

## small tuning grid: hidden-units × weight-decay
grid_small <- expand_grid(
  size  = c(3, 5, 7),
  decay = c(0, 1e-4, 1e-3)
)

results <- list()   # will gather rows

############################################################
## 3.  Main loop --------------------------------------------------------
for (g in names(train_list)) {
  train0 <- train_list[[g]]
  test0  <- test_list [[g]]
  
  for (serve in c(1, 2)) {
    ## filter first- or second-serves
    train_sub <- train0[ServeNumber == serve]
    test_sub  <- test0 [ServeNumber == serve]
    
    ## align factor levels
    train_sub[, (vars_fact) := lapply(.SD, factor), .SDcols = vars_fact]
    for (v in vars_fact)
      test_sub[, (v) := factor(get(v), levels = levels(train_sub[[v]]))]
    
    ## ensure binary factor response
    train_sub[, serving_player_won := factor(serving_player_won, levels = c(0, 1))]
    test_sub [, serving_player_won := factor(serving_player_won, levels = c(0, 1))]
    
    for (speed_var in c("Speed_MPH", "speed_ratio")) {
      vars_num <- c(base_num, speed_var)
      
      ## modelling frames
      train_dat <- train_sub[, c(vars_num, vars_fact, "serving_player_won"), with = FALSE]
      test_dat  <- test_sub [, c(vars_num, vars_fact, "serving_player_won"), with = FALSE]
      
      ## numeric scaling
      scaled <- scale_like(copy(train_dat), copy(test_dat), num_vars = vars_num)
      train_scaled <- scaled$train
      test_scaled  <- scaled$test
      
      ## ---- 3-fold CV on ≤10 000 rows ---------------------------------
      n_tune   <- min(10000, nrow(train_scaled))
      tune_idx <- sample(nrow(train_scaled), n_tune)
      tune_dat <- train_scaled[tune_idx]
      folds    <- cut(seq_len(n_tune), breaks = 3, labels = FALSE)
      
      cv_scores <- grid_small %>% mutate(mean_ll = NA_real_)
      
      for (grid_i in seq_len(nrow(grid_small))) {
        s  <- grid_small$size [grid_i]
        d  <- grid_small$decay[grid_i]
        ll_vec <- numeric(3)
        
        for (fold in 1:3) {
          val_idx   <- which(folds == fold)
          train_idx <- setdiff(seq_len(n_tune), val_idx)
          
          nn_fit <- nnet(
            serving_player_won ~ .,
            data  = tune_dat[train_idx],
            size  = s,
            decay = d,
            maxit = 200,
            trace = FALSE
          )
          
          probs <- as.numeric(predict(nn_fit, tune_dat[val_idx], type = "raw"))
          ll_vec[fold] <- fast_logloss(tune_dat$serving_player_won[val_idx], probs)
        }
        cv_scores$mean_ll[grid_i] <- mean(ll_vec)
      }
      
      best <- cv_scores %>% slice_min(mean_ll, n = 1)
      
      ## ---- Fit best model on full subset -----------------------------
      nn_fit_final <- nnet(
        serving_player_won ~ .,
        data  = train_scaled,
        size  = best$size,
        decay = best$decay,
        maxit = 500,
        trace = FALSE
      )
      
      ## ---- Evaluate on held-out test set -----------------------------
      probs_test <- as.numeric(predict(nn_fit_final, test_scaled, type = "raw"))
      pred_class <- factor(ifelse(probs_test >= 0.5, 1, 0), levels = c(0, 1))
      
      acc <- mean(pred_class == test_scaled$serving_player_won)
      ll  <- fast_logloss(test_scaled$serving_player_won, probs_test)
      
      ## ---- Record result ---------------------------------------------
      results[[length(results) + 1]] <- list(
        gender         = g,
        serve_number   = serve,
        speed_variable = speed_var,
        hidden_size    = best$size,
        weight_decay   = best$decay,
        test_accuracy  = acc,
        log_loss       = ll
      )
    }
  }
}

############################################################
## 4.  Save + display results ------------------------------------------
results_df <- bind_rows(results) %>%
  arrange(gender, serve_number, speed_variable)

print(results_df)
write.csv(results_df, "nn_results_wimbledon.csv", row.names = FALSE)
