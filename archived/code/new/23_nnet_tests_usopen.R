############################################################
## nn_usopen.R ― Neural-network benchmark for US-Open data
##  - Loops over gender, serve #, and speed variable
##  - 3-fold CV grid-search for (size, decay)
##  - Computes test accuracy & log loss
############################################################
rm(list=ls())

## 0.  Packages & helpers ----------------------------------------------
library(data.table)
library(tidyverse)   # dplyr, purrr, tibble, …
library(nnet)        # feed-forward neural nets
library(tibble)
library(purrr)

## Fast log-loss (binary)
fast_logloss <- function(truth, prob1, eps = 1e-15) {
  prob1 <- pmin(pmax(prob1, eps), 1 - eps)
  y     <- as.numeric(truth) - 1           # factor {0,1} → 0/1
  -mean(y * log(prob1) + (1 - y) * log(1 - prob1))
}

## Scale numeric columns in train & apply same transform to test
scale_like <- function(train_df, test_df, num_vars) {
  means <- sapply(train_df[, ..num_vars], mean)
  sds   <- sapply(train_df[, ..num_vars], sd)
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
paths_train <- list(M = "../data/output/scaled-results/usopen_m_train_scaled.csv",
                    F = "../data/output/scaled-results/usopen_f_train_scaled.csv")
paths_test  <- list(M = "../data/output/scaled-results/usopen_subset_m_testing.csv",
                    F = "../data/output/scaled-results/usopen_subset_f_testing.csv")

train_list <- map(paths_train, ~ as.data.table(fread(.x))[Speed_MPH > 0])
test_list  <- map(paths_test,  ~ as.data.table(fread(.x))[Speed_MPH > 0])

############################################################
## 2.  Settings ---------------------------------------------------------
base_num   <- c("importance_z", "p_server_beats_returner_z", "ElapsedSeconds_fixed_z", "df_pct_server_z")
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
    
    for (speed_var in c("Speed_MPH_z", "speed_ratio_z")) {
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
write.csv(results_df, "nn_results_usopen.csv", row.names = FALSE)


############################################################
## rerun for us open men
############################################################
############################################################

rm(list = ls())
set.seed(42)

## 0.  Packages & helpers -----------------------------------------------------
library(data.table)
library(tidyverse)   # dplyr, purrr, tibble …
library(nnet)        # neural nets

# fast binary log-loss
fast_logloss <- function(truth, prob1, eps = 1e-15) {
  prob1 <- pmin(pmax(prob1, eps), 1 - eps)
  y     <- as.numeric(truth) - 1            # factor {0,1} → 0/1
  -mean(y * log(prob1) + (1 - y) * log(1 - prob1))
}

# scale numeric columns in train, apply same transform to test
scale_like <- function(train_df, test_df, num_vars) {
  m <- sapply(train_df[, ..num_vars], mean)
  s <- sapply(train_df[, ..num_vars], sd)
  train_df[, (num_vars) :=
             Map(function(x, mu, sd) (x - mu) / sd, .SD, m, s),
           .SDcols = num_vars]
  test_df[,  (num_vars) :=
            Map(function(x, mu, sd) (x - mu) / sd, .SD, m, s),
          .SDcols = num_vars]
  list(train = train_df, test = test_df)
}

## 1.  Load men’s data, drop zero-speed rows ----------------------------------
path_train <- "../data/output/scaled-results/usopen_m_train_scaled.csv"  # 2021-24
path_test  <- "../data/output/scaled-results/usopen_subset_m_testing.csv"   # 2018-19

train0 <- as.data.table(fread(path_train))[Speed_MPH > 0]
test0  <- as.data.table(fread(path_test ))[Speed_MPH > 0]

colSums(is.na(test0))  # check for NAs
# summary(train0)
# find all unique values in ServeWidth column
# unique(train0$ServeDepth)  # check factor levels

# remove NAs from test0 for all columns
test0 <- na.omit(test0)


## 2.  Predictor sets & tuning grid -------------------------------------------
base_num  <- c("importance_z",
               "p_server_beats_returner_z",
               "ElapsedSeconds_fixed_z",
               "df_pct_server_z")
vars_fact <- c("ServeWidth", "ServeDepth")

grid_small <- expand_grid(
  size  = c(3, 5, 7),         # hidden-units
  decay = c(0, 1e-4, 1e-3)    # L2 weight-decay
)

results <- list()

## 3.  Loop over serve number & speed variable --------------------------------
for (serve in c(1, 2)) {
  train_sub <- train0[ServeNumber == serve]
  test_sub  <- test0 [ServeNumber == serve]
  
  # harmonise factor levels
  train_sub[, (vars_fact) := lapply(.SD, factor), .SDcols = vars_fact]
  for (v in vars_fact)
    test_sub[, (v) := factor(get(v), levels = levels(train_sub[[v]]))]
  
  # ensure binary factor response
  train_sub[, serving_player_won := factor(serving_player_won, levels = c(0, 1))]
  test_sub [, serving_player_won := factor(serving_player_won, levels = c(0, 1))]
  
  for (speed_var in c("Speed_MPH_z", "speed_ratio_z")) {
    
    vars_num <- c(base_num, speed_var)
    
    train_dat <- train_sub[, c(vars_num, vars_fact, "serving_player_won"), with = FALSE]
    test_dat  <- test_sub [, c(vars_num, vars_fact, "serving_player_won"), with = FALSE]
    
    # numeric scaling ---------------------------------------------------------
    scaled <- scale_like(copy(train_dat), copy(test_dat), vars_num)
    train_scaled <- scaled$train
    test_scaled  <- scaled$test
    
    # 3-fold CV on ≤10 000 rows ----------------------------------------------
    n_cv   <- min(10000, nrow(train_scaled))
    cv_idx <- sample(nrow(train_scaled), n_cv)
    cv_dat <- train_scaled[cv_idx]
    folds  <- cut(seq_len(n_cv), breaks = 3, labels = FALSE)
    
    cv_scores <- grid_small %>% mutate(mean_ll = NA_real_)
    
    for (g in seq_len(nrow(grid_small))) {
      s <- grid_small$size [g]
      d <- grid_small$decay[g]
      ll <- numeric(3)
      
      for (fold in 1:3) {
        val  <- which(folds == fold)
        trn  <- setdiff(seq_len(n_cv), val)
        
        nn <- nnet(serving_player_won ~ ., data = cv_dat[trn],
                   size = s, decay = d, maxit = 200, trace = FALSE)
        
        p  <- as.numeric(predict(nn, cv_dat[val], type = "raw"))
        ll[fold] <- fast_logloss(cv_dat$serving_player_won[val], p)
      }
      cv_scores$mean_ll[g] <- mean(ll)
    }
    
    best <- cv_scores %>% slice_min(mean_ll, n = 1)
    
    # fit best model on full training subset ---------------------------------
    nn_final <- nnet(serving_player_won ~ ., data  = train_scaled,
                     size = best$size, decay = best$decay,
                     maxit = 500, trace = FALSE)
    
    # # evaluate on test subset -------------------------------------------------
    # p_test <- as.numeric(predict(nn_final, test_scaled, type = "raw"))
    # cls    <- factor(ifelse(p_test >= 0.5, 1, 0), levels = c(0, 1))
    
    # ── evaluate on test subset -------------------------------------------
    p_test <- as.numeric(predict(nn_final, test_scaled, type = "raw"))
    
    ## ── NEW: diagnostics --------------------------------------------------
    pred_tbl <- data.table(
      row_id = seq_len(nrow(test_scaled)),
      prob   = p_test,
      truth  = as.numeric(as.character(test_scaled$serving_player_won))  # 0/1
    )
    
    cat("\nFirst 20 predictions (row_id, prob, truth):\n")
    print(head(pred_tbl, 20))
    
    if (anyNA(pred_tbl$prob)) {
      na_rows <- pred_tbl[is.na(prob)]$row_id
      cat("\nNumber of NA predictions:", length(na_rows), "\n")
      cat("Showing the first few offending rows and their factor levels:\n")
      print(
        test_scaled[na_rows,
                    c(vars_fact, vars_num),   # vars_fact / vars_num from your script
                    with = FALSE][1:10]
      )
    }
    ## ----------------------------------------------------------------------
    
    cls <- factor(ifelse(p_test >= 0.5, 1, 0), levels = c(0, 1))
    acc <- mean(cls == test_scaled$serving_player_won, na.rm = TRUE)
    ll  <- fast_logloss(test_scaled$serving_player_won, p_test)
    
    # acc <- mean(cls == test_scaled$serving_player_won)
    # ll  <- fast_logloss(test_scaled$serving_player_won, p_test)
    
    results[[length(results) + 1]] <- list(
      serve_number   = serve,
      speed_variable = speed_var,
      hidden_size    = best$size,
      weight_decay   = best$decay,
      test_accuracy  = acc,
      log_loss       = ll
    )
  }
}



## 4.  Save & display results -------------------------------------------------
results_df <- bind_rows(results) %>% arrange(serve_number, speed_variable)

print(results_df)
# write.csv(results_df, "nn_results_usopen_m.csv", row.names = FALSE)
