# rm(list=ls())
# 
# ## -------- 0. FILE LOCATIONS -------------------------------------------
path_train_w_m_scaled <- "scaled/wimbledon_m_train_scaled.csv" # 2021–2024 data
path_train_w_f_scaled <- "scaled/wimbledon_f_train_scaled.csv"
path_train_u_m_scaled <- "scaled/usopen_m_train_scaled.csv"
path_train_u_f_scaled <- "scaled/usopen_f_train_scaled.csv"

path_oos_w_m_scaled   <- "scaled/wimbledon_m_test_scaled.csv" # 2018–2019 data
path_oos_w_f_scaled   <- "scaled/wimbledon_f_test_scaled.csv"
path_oos_u_m_scaled   <- "scaled/usopen_m_test_scaled.csv"
path_oos_u_f_scaled   <- "scaled/usopen_f_test_scaled.csv"
# 
# library(tidyverse)   # dplyr / readr / ggplot2 / tibble …
# library(data.table)  # fread
# library(splines)     # bs()
# library(ranger)      # random forest
# library(ggplot2)     # plotting
# 
# # 1) read in your training sets
# train_sets <- list(
#   wimbledon_m = fread(path_train_w_m_scaled),
#   wimbledon_f = fread(path_train_w_f_scaled),
#   usopen_m    = fread(path_train_u_m_scaled),
#   usopen_f    = fread(path_train_u_f_scaled)
# )
# 
# # 2) ensure ElapsedSeconds_fixed exists
# train_sets <- map(train_sets, function(df) {
#   if (!"ElapsedSeconds_fixed" %in% names(df))
#     df$ElapsedSeconds_fixed <- df$ElapsedSeconds
#   df
# })
# 
# # 3) define the “speed” formula
# form_speed <- serving_player_won ~
#   p_server_beats_returner +
#   ElapsedSeconds_fixed +
#   importance +
#   bs(Speed_MPH, degree = 3, df = 5) +
#   factor(ServeWidth) +
#   factor(ServeDepth)
# 
# # 4) pick one slice, e.g. Wimbledon men first serves
# df <- train_sets$wimbledon_m %>%
#   filter(ServeNumber == 1)
# 
# # 5) build design matrix & response
# X <- model.matrix(form_speed, data = df)[, -1]      # drop intercept column
# y <- df$serving_player_won
# 
# # 6) fit a probability forest with impurity‐based importances
# rf_mod <- ranger(
#   x           = X,
#   y           = y,
#   probability = TRUE,
#   importance  = "impurity",
#   num.trees   = 500
# )
# 
# # 7) extract importances into a tibble
# imp_df <- enframe(rf_mod$variable.importance, name = "variable", value = "importance") %>%
#   arrange(desc(importance))
# 
# # 8) plot
# ggplot(imp_df, aes(x = reorder(variable, importance), y = importance)) +
#   geom_col() +
#   coord_flip() +
#   labs(
#     title = "RF Variable Importance\nWimbledon Men — First Serve (speed)",
#     x     = NULL,
#     y     = "Importance"
#   ) +
#   theme_minimal()
# 
# # 9) save the plot
# ggsave("../images/variable_importance_plot_speed_mph.png", width = 8, height = 6, units = "in", bg = "white")
# 
# ########################
# # same thing but for speed_ratio
# 
# # 3) define the “speed” formula
# form_speed <- serving_player_won ~
#   p_server_beats_returner +
#   ElapsedSeconds_fixed +
#   importance +
#   bs(speed_ratio, degree = 3, df = 5) +
#   factor(ServeWidth) +
#   factor(ServeDepth)
# 
# # 4) pick one slice, e.g. Wimbledon men first serves
# df <- train_sets$wimbledon_m %>%
#   filter(ServeNumber == 1)
# 
# # 5) build design matrix & response
# X <- model.matrix(form_speed, data = df)[, -1]      # drop intercept column
# y <- df$serving_player_won
# 
# # 6) fit a probability forest with impurity‐based importances
# rf_mod <- ranger(
#   x           = X,
#   y           = y,
#   probability = TRUE,
#   importance  = "impurity",
#   num.trees   = 500
# )
# 
# # 7) extract importances into a tibble
# imp_df <- enframe(rf_mod$variable.importance, name = "variable", value = "importance") %>%
#   arrange(desc(importance))
# 
# # 8) plot
# ggplot(imp_df, aes(x = reorder(variable, importance), y = importance)) +
#   geom_col() +
#   coord_flip() +
#   labs(
#     title = "RF Variable Importance\nWimbledon Men — First Serve (speed_ratio)",
#     x     = NULL,
#     y     = "Importance"
#   ) +
#   theme_minimal()
# 
# # 9) save the plot
# ggsave("../images/variable_importance_plot_speed_ratio.png", width = 8, height = 6, units = "in", bg = "white")

rm(list = ls())

## -------- 0.  FILE LOCATIONS -------------------------------------------
paths <- list(
  wimbledon_m = "scaled/wimbledon_m_train_scaled.csv",  # 2021-24
  wimbledon_f = "scaled/wimbledon_f_train_scaled.csv",
  usopen_m    = "scaled/usopen_m_train_scaled.csv",
  usopen_f    = "scaled/usopen_f_train_scaled.csv"
)

## -------- 1.  LIBRARIES -----------------------------------------------
library(tidyverse)   # readr / dplyr / ggplot2 / purrr …
library(data.table)  # fread
library(splines)     # bs()
library(ranger)      # probability forest

## -------- 2.  READ & PREPARE DATA -------------------------------------
train_sets <- imap(paths, \(p, nm) {
  df <- fread(p)
  
  # ensure ElapsedSeconds_fixed exists
  if (!"ElapsedSeconds_fixed" %in% names(df))
    df[, ElapsedSeconds_fixed := ElapsedSeconds]
  
  # store the data-frame with its key for later use
  attr(df, "key") <- nm
  df
})

## -------- 3.  HELPER FUNCTIONS ----------------------------------------
human_label <- function(key) {
  str_replace_all(key, c("_" = " ")) |>               # underscores → spaces
    str_to_title(locale = "en") |>                    # title case
    str_replace("M$", "Men") |>                       # m → Men
    str_replace("F$", "Women") |>                     # f → Women
    str_replace("Usopen", "US Open")                  # prettier
}

build_formula <- function(speed_var) {
  as.formula(
    paste(
      "serving_player_won ~ p_server_beats_returner +",
      "ElapsedSeconds_fixed + importance +",
      sprintf("bs(%s, degree = 3, df = 5)", speed_var),
      "+ factor(ServeWidth) + factor(ServeDepth)"
    )
  )
}

fit_forest_and_plot <- function(df, key, serve_no, speed_var) {
  
  # Skip gracefully if the speed variable is missing
  if (!speed_var %in% names(df)) return(invisible(NULL))
  
  slice <- df[ServeNumber == serve_no]
  
  # Not enough rows?  Skip.
  if (nrow(slice) < 50) return(invisible(NULL))
  
  X <- model.matrix(build_formula(speed_var), data = slice)[, -1]
  y <- slice$serving_player_won
  
  rf_mod <- ranger(
    x           = X,
    y           = y,
    probability = TRUE,
    importance  = "impurity",
    num.trees   = 500,
    seed        = 19
  )
  
  imp_df <- enframe(rf_mod$variable.importance,
                    name = "variable", value = "importance") |>
    arrange(desc(importance))
  
  title_txt <- sprintf(
    "RF Variable Importance\n%s — %s Serve (%s)",
    human_label(key),
    ifelse(serve_no == 1, "First", "Second"),
    speed_var
  )
  
  p <- ggplot(imp_df,
              aes(x = reorder(variable, importance),
                  y = importance)) +
    geom_col() +
    coord_flip() +
    labs(title = title_txt, x = NULL, y = "Importance") +
    theme_minimal(base_size = 12)
  
  file_out <- sprintf(
    "../images/vi_%s_serve%d_%s.png",
    key, serve_no, speed_var
  )
  
  ggsave(file_out, plot = p, width = 8, height = 6,
         units = "in", bg = "white")
  message("Saved: ", file_out)
}

## -------- 4.  LOOP OVER ALL COMBOS ------------------------------------
speed_vars  <- c("Speed_MPH", "speed_ratio")
serve_nums  <- c(1, 2)  # first / second

walk(train_sets, \(df) {
  key <- attr(df, "key")
  walk(serve_nums, \(sv_no) {
    walk(speed_vars, \(spd) {
      fit_forest_and_plot(df, key, sv_no, spd)
    })
  })
})
