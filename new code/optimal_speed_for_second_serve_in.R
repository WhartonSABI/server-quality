rm(list = ls())
library(tidyverse)
library(data.table)
library(splines)

# --- Load & clean data -------------------------------------------------------
subset_m <- fread("../data/wimbledon_subset_m.csv")
subset_f <- fread("../data/wimbledon_subset_f.csv")

# remove duplicate column from subset_f
setDT(subset_f)
subset_f <- subset_f[, .SD, .SDcols = !duplicated(names(subset_f))]


# --- Helper: prepare all 2nd-serve observations ------------------------------
prep_second_serves <- function(df) {
  df %>%
    filter(ServeNumber == 2,
           !is.na(speed_ratio),              # keep only rows where speeds recorded
           !is.na(Speed_MPH)) %>%
    mutate(
      serve_in = if_else(                      # 1 = in, 0 = double fault
        (PointServer == 1 & P1DoubleFault == 0) |
          (PointServer == 2 & P2DoubleFault == 0),
        1L, 0L),
      server = if_else(PointServer == 1, player1_name, player2_name)
    ) %>%
    add_count(server, name = "n_serves") %>%   # weighting: 1 / # serves by server
    mutate(weight = 1 / n_serves)
}

m_second_all <- prep_second_serves(subset_m)
f_second_all <- prep_second_serves(subset_f)

# --- Helper: fit spline model & pick optimal speed ---------------------------
fit_spline <- function(df, speed_var = c("speed_ratio", "Speed_MPH"), df_spline = 5) {
  speed_var <- match.arg(speed_var)
  form <- as.formula(paste0("serve_in ~ bs(", speed_var, ", df = ", df_spline, ")"))
  glm(form, data = df, family = binomial, weights = weight)
}

find_optimum <- function(model, df, speed_var, grid_len = 500) {
  rng  <- range(df[[speed_var]], na.rm = TRUE)
  grid <- seq(rng[1], rng[2], length.out = grid_len)
  
  X     <- model.matrix(~ bs(x, df = 5), data = data.frame(x = grid))
  preds <- plogis(X %*% coef(model))
  i_max <- which.max(preds)
  
  list(opt_speed = grid[i_max],
       max_prob  = preds[i_max],
       grid      = grid,
       prob      = preds)
}

# --- Fit models --------------------------------------------------------------
mods <- list(
  m_ratio = fit_spline(m_second_all, "speed_ratio"),
  m_mph   = fit_spline(m_second_all, "Speed_MPH"),
  f_ratio = fit_spline(f_second_all, "speed_ratio"),
  f_mph   = fit_spline(f_second_all, "Speed_MPH")
)

# --- Extract optimal speeds --------------------------------------------------
opts <- list(
  m_ratio = find_optimum(mods$m_ratio, m_second_all, "speed_ratio"),
  m_mph   = find_optimum(mods$m_mph,   m_second_all, "Speed_MPH"),
  f_ratio = find_optimum(mods$f_ratio, f_second_all, "speed_ratio"),
  f_mph   = find_optimum(mods$f_mph,   f_second_all, "Speed_MPH")
)

# --- Quick report ------------------------------------------------------------
cat("\n*** Optimal 2nd-Serve Speeds for Getting the Ball In ***\n")
purrr::iwalk(opts, \(x, n) {
  cat(sprintf("%-8s  %.3f  (P = %.3f)\n", n, x$opt_speed, x$max_prob))
})

# --- Optional: diagnostic plot function --------------------------------------
plot_in_prob <- function(opt, title, xlab) {
  tibble(speed = opt$grid, prob = opt$prob) %>%
    ggplot(aes(speed, prob)) +
    geom_line(size = 1.2, colour = "red") +
    geom_point(aes(opt$opt_speed, opt$max_prob),
               colour = "blue", size = 3, shape = 17) +
    geom_text(aes(opt$opt_speed, opt$max_prob,
                  label = paste0("Opt = ", round(opt$opt_speed, 2))),
              vjust = -1, fontface = "bold", colour = "blue") +
    labs(title = title,
         x     = xlab,
         y     = "P(Second-Serve In)") +
    theme_minimal()
}

# Example (viewer only):
plot_in_prob(opts$m_ratio,
             "Males: P(2nd-Serve In) vs Speed Ratio",
             "Speed Ratio")
