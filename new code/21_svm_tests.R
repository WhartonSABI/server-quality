rm(list = ls())

library(data.table)
library(tidyverse)      # dplyr, purrr, tibble, ggplot2
library(e1071)          # svm()
library(yardstick)      # accuracy_vec(), mn_log_loss_vec()
library(viridisLite)    # nice continuous palettes (scale_fill_viridis_c)

## ---------------------------------------------------------------------
## 1.  paths ------------------------------------------------------------
paths <- list(
  wimb_m_train = "scaled/wimbledon_m_train_scaled.csv",
  wimb_f_train = "scaled/wimbledon_f_train_scaled.csv",
  uso_m_train  = "scaled/usopen_m_train_scaled.csv",
  uso_f_train  = "scaled/usopen_f_train_scaled.csv",
  
  wimb_m_test  = "scaled/wimbledon_m_test_scaled.csv",
  wimb_f_test  = "scaled/wimbledon_f_test_scaled.csv",
  uso_m_test   = "scaled/usopen_m_test_scaled.csv",
  uso_f_test   = "scaled/usopen_f_test_scaled.csv"
)

## ---------------------------------------------------------------------
## 2.  helper: build & save a 2-D decision plot ------------------------
plot_svm_2d <- function(best_mod, train_df, speed_var,
                        plot_dir, file_stub,
                        grid_pts = 100) {
  
  plot_var <- "importance"
  
  # sequences for 2-D grid
  sx <- seq(min(train_df[[speed_var]], na.rm = TRUE),
            max(train_df[[speed_var]], na.rm = TRUE),
            length.out = grid_pts)
  sy <- seq(min(train_df[[plot_var]], na.rm = TRUE),
            max(train_df[[plot_var]], na.rm = TRUE),
            length.out = grid_pts)
  grid <- expand.grid(sx, sy)
  names(grid) <- c(speed_var, plot_var)
  
  ## ---- hold all other predictors at typical values ------------------
  grid$ElapsedSeconds_fixed     <- median(train_df$ElapsedSeconds_fixed, na.rm = TRUE)
  grid$p_server_beats_returner  <- median(train_df$p_server_beats_returner, na.rm = TRUE)
  grid$ServeWidth               <- factor(levels(train_df$ServeWidth)[1],
                                          levels = levels(train_df$ServeWidth))
  grid$ServeDepth               <- factor(levels(train_df$ServeDepth)[1],
                                          levels = levels(train_df$ServeDepth))
  
  ## ---- predict probability -----------------------------------------
  grid$prob <- attr(predict(best_mod, grid, probability = TRUE),
                    "probabilities")[, "1"]
  
  ## ---- sample some training pts for overlay -------------------------
  pts <- train_df %>%
    select(all_of(c(speed_var, plot_var, "server_win"))) %>%
    sample_n(min(1000, n()))
  
  p <- ggplot(grid, aes_string(speed_var, plot_var)) +
    geom_raster(aes(fill = prob), interpolate = TRUE) +
    scale_fill_viridis_c(name = "P(win)", option = "D") +
    geom_point(data = pts,
               aes_string(speed_var, plot_var,
                          shape = "factor(server_win)"),
               alpha = 0.5, size = 1) +
    scale_shape_manual(values = c(1, 17), name = "Actual") +
    labs(x = speed_var, y = "importance",
         title = file_stub) +
    theme_minimal(base_size = 10)
  
  ggsave(filename = file.path(plot_dir, paste0(file_stub, ".png")),
         plot     = p,
         width    = 5, height = 4, dpi = 300)
}

## ---------------------------------------------------------------------
# ## 3.  helper: run tuned SVM & return metrics --------------------------
# run_svm <- function(train_df, test_df,
#                     y           = "server_win",
#                     speed_vars  = c("Speed_MPH", "speed_ratio"),
#                     other_vars,
#                     cost_grid   = c(0.0001, 0.001, 0.01, 0.1, 1, 2, 5, 10, 25, 50, 100),
#                     gamma_grid  = c(0.01, 0.1, 0.25, 0.5, 1, 2, 3, 4, 5, 10),
#                     plot_dir, file_prefix) {
#   
#   map_dfr(speed_vars, function(spd) {
#     form <- as.formula(
#       paste(y, "~", paste(c(spd, other_vars), collapse = " + "))
#     )
#     
#     set.seed(2025)
#     tuned <- tune(
#       svm, form,
#       data   = train_df,
#       kernel = "radial",
#       ranges = list(cost = cost_grid, gamma = gamma_grid),
#       probability = TRUE,
#       tunecontrol = tune.control(cross = 5)
#     )
#     
#     best <- tuned$best.model
#     
#     ## ---- evaluation on test set -------------------------------------
#     test_prob <- attr(predict(best, test_df, probability = TRUE),
#                       "probabilities")[, "1"]
#     test_pred <- factor(ifelse(test_prob > 0.5, 1, 0), levels = c(0, 1))
#     truth_f   <- factor(test_df[[y]], levels = c(0, 1))
#     
#     ## ---- save decision-surface plot ---------------------------------
#     plot_svm_2d(best, train_df, spd,
#                 plot_dir,
#                 paste0(file_prefix, "_", spd))
#     
#     tibble(
#       speed_var = spd,
#       cost      = tuned$best.parameters$cost,
#       gamma     = tuned$best.parameters$gamma,
#       accuracy  = accuracy_vec(truth_f, test_pred),
#       log_loss  = mn_log_loss_vec(truth_f, test_prob)
#     )
#   })
# }
run_svm_fast <- function(train_df, test_df,
                         y           = "server_win",
                         speed_vars  = c("Speed_MPH", "speed_ratio"),
                         other_vars,
                         cost_grid   = 2 ^ seq(-5, 9, by = 2),   # 6 values
                         gamma_grid  = 2 ^ seq(-8, 2, by = 2),   # 6 values
                         subsample   = 0.15,                     # A: 15 % sample
                         plot_dir,
                         file_prefix) {
  
  map_dfr(speed_vars, function(spd) {
    
    ## ------------------------------ 0. subsample for tuning (A)
    idx_tune <- sample.int(nrow(train_df),
                           size = ceiling(nrow(train_df) * subsample))
    tune_df  <- train_df[idx_tune, ]
    
    fml <- as.formula(paste(y, "~", paste(c(spd, other_vars), collapse = " + ")))
    
    ## ------------------------------ 1. coarse 3 × 3 grid (B)
    coarse <- tune(
      svm, fml,
      data   = tune_df,
      kernel = "radial",
      ranges = list(
        cost  = cost_grid [c(1, 4, 6)],   # 2^-5 , 2^3 , 2^9
        gamma = gamma_grid[c(1, 3, 6)]    # 2^-8 , 2^-4, 2^2
      ),
      cross  = 3,
      probability = TRUE,                 # ← keep probabilities
      # uncomment next line for parallel CV (C)
      # tunecontrol = tune.control(cross = 3, allowParallel = TRUE)
    )
    
    ## ------------------------------ 2. fine 3 × 3 grid around winner (B)
    fine <- tune(
      svm, fml,
      data   = tune_df,
      kernel = "radial",
      ranges = list(
        cost  = coarse$best.parameters$cost  * c(0.25, 1, 4),
        gamma = coarse$best.parameters$gamma * c(0.25, 1, 4)
      ),
      cross  = 3,
      probability = TRUE
    )
    
    ## ------------------------------ 3. refit once on full training set (A)
    best <- svm(
      fml, data = train_df,
      kernel      = "radial",
      cost        = fine$best.parameters$cost,
      gamma       = fine$best.parameters$gamma,
      probability = TRUE
    )
    
    ## ------------------------------ 4. evaluate
    test_prob <- attr(predict(best, test_df, probability = TRUE),
                      "probabilities")[, "1"]
    test_pred <- factor(test_prob > 0.5, levels = c(FALSE, TRUE))
    truth_f   <- factor(test_df[[y]] > 0,  levels = c(FALSE, TRUE))
    
    ## (optional) decision-surface plot on the smaller tune_df
    plot_svm_2d(best, train_df = tune_df,
                speed_var = spd,
                plot_dir  = plot_dir,
                file_stub = paste0(file_prefix, "_", spd))
    
    tibble(
      speed_var = spd,
      cost      = best$cost,
      gamma     = attr(best, "gamma"),
      accuracy  = accuracy_vec(truth_f, test_pred),
      log_loss  = mn_log_loss_vec(truth_f, test_prob)
    )
  })
}

## ---------------------------------------------------------------------
## 4.  common predictors -----------------------------------------------
other_predictors <- c(
  "importance",
  "p_server_beats_returner",
  "ServeWidth",
  "ServeDepth",
  "ElapsedSeconds_fixed"
)

## ---------------------------------------------------------------------
## 5.  experiment design (8 splits) ------------------------------------
exp_grid <- tribble(
  ~set_name,  ~train_path,           ~test_path,            ~serve_type,
  "wimb_m_1", paths$wimb_m_train,    paths$wimb_m_test,     1,
  "wimb_m_2", paths$wimb_m_train,    paths$wimb_m_test,     0,
  "wimb_f_1", paths$wimb_f_train,    paths$wimb_f_test,     1,
  "wimb_f_2", paths$wimb_f_train,    paths$wimb_f_test,     0,
  "uso_m_1",  paths$uso_m_train,     paths$uso_m_test,      1,
  "uso_m_2",  paths$uso_m_train,     paths$uso_m_test,      0,
  "uso_f_1",  paths$uso_f_train,     paths$uso_f_test,      1,
  "uso_f_2",  paths$uso_f_train,     paths$uso_f_test,      0
)

## ---------------------------------------------------------------------
## 6.  make a folder for plots -----------------------------------------
plot_dir <- "svm_plots"
dir.create(plot_dir, showWarnings = FALSE)

## ---------------------------------------------------------------------
## 7.  run everything ---------------------------------------------------
results_df <- pmap_dfr(
  exp_grid,
  function(set_name, train_path, test_path, serve_type) {
    
    train_raw <- fread(train_path)
    test_raw  <- fread(test_path)
    
    train <- train_raw %>% filter(ServeNumber == serve_type)
    test  <- test_raw  %>% filter(ServeNumber == serve_type)
    
    run_svm_fast(
      train_df   = train,
      test_df    = test,
      y          = "serving_player_won",
      speed_vars = c("Speed_MPH", "speed_ratio"),
      other_vars = other_predictors,
      plot_dir   = plot_dir,
      file_prefix = paste0(set_name,
                           ifelse(serve_type == 1, "_first", "_second"))
    ) %>%
      mutate(data_set   = set_name,
             serve_type = ifelse(serve_type == 1, "first", "second"),
             .before = 1)
  }
)

print(results_df, n = Inf)

## ---------------------------------------------------------------------
## 8.  (optional) save metrics -----------------------------------------
# fwrite(results_df, "svm_test_results.csv")
