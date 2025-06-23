rm(list=ls())
# install.packages("welo")
library(welo)
library(tidyverse)
library(data.table)
library(ggplot2)
# install.packages("car")
library(car)

#-----------------------------------------------------------------------------------------------------

subset_m <- as.data.table(read.csv("../data/wimbledon_subset_m.csv"))
subset_f <- as.data.table(read.csv("../data/wimbledon_subset_f.csv"))

names(subset_m)

#-----------------------------------------------------------------------------------------------------

# # Convert ELO to logistic (Bradley-Terry scale)
# subset_m[, welo_p1_bt := 0.0057565 * player1_avg_welo]
# subset_m[, welo_p2_bt := 0.0057565 * player2_avg_welo]
# 
# ## male
# subset_m <- subset_m %>%
#   mutate(p_server_beats_returner <- ifelse(PointServer == 1,
#                                            1 / (1 + exp(welo_p2_bt - welo_p1_bt)),
#                                            1 / (1 + exp(welo_p1_bt - welo_p2_bt))))
# 
# setnames(subset_m, old = c("... <- NULL"),
#          new = c("p_server_beats_returner"))
# 
# write.csv(subset_m, "../data/wimbledon_subset_m.csv", row.names = FALSE)

#-----------------------------------------------------------------------------------------------------

# # Convert ELO to logistic (Bradley-Terry scale)
# subset_f[, welo_p1_bt := 0.0057565 * player1_avg_welo]
# subset_f[, welo_p2_bt := 0.0057565 * player2_avg_welo]
# 
# ## female
# subset_f <- subset_f %>%
#   mutate(p_server_beats_returner <- ifelse(PointServer == 1,
#                                            1 / (1 + exp(welo_p2_bt - welo_p1_bt)),
#                                            1 / (1 + exp(welo_p1_bt - welo_p2_bt))))
# 
# # rename column in subset_f
# setnames(subset_f, old = c("... <- NULL"),
#          new = c("p_server_beats_returner"))
# 
# write.csv(subset_f, "../data/wimbledon_subset_f.csv", row.names = FALSE)

# -----------------------------------------------------------------------------------------------------

#-----------------------------------------------------------------------------------------------------

# new models with just second serves
subset_m_second <- subset_m[ServeNumber == 2]
subset_f_second <- subset_f[ServeNumber == 2]

## logistic regression for serving_player_won vs. p_server_beats_returner
logit_model_m <- glm(serving_player_won ~ p_server_beats_returner, 
                     data = subset_m_second, family = "binomial")
summary(logit_model_m) ## p_server_beats_returner significant *** (pos coef)

logit_model_f <- glm(serving_player_won ~ p_server_beats_returner, 
                     data = subset_f_second, family = "binomial")
summary(logit_model_f) ## p_server_beats_returner significant *** (pos coef)

#-----------------------------------------------------------------------------------------------------

## add covariates

logit_model_2_m <- glm(serving_player_won ~ p_server_beats_returner + ElapsedSeconds_fixed, 
                       data = subset_m_second, family = "binomial")
summary(logit_model_2_m) ## p_server_beats_returner significant *** (pos coef)

logit_model_2_f <- glm(serving_player_won ~ p_server_beats_returner + ElapsedSeconds_fixed, 
                       data = subset_f_second, family = "binomial")
summary(logit_model_2_f) ## p_server_beats_returner significant *** (pos coef)

#-----------------------------------------------------------------------------------------------------

## add second serve speed ratio
logit_model_3_m <- glm(serving_player_won ~ p_server_beats_returner + ElapsedSeconds_fixed + speed_ratio, 
                       data = subset_m_second, family = "binomial")
summary(logit_model_3_m) ## p_server_beats_returner significant *** (pos coef), speed_ratio significant *** (pos coef)

logit_model_3_f <- glm(serving_player_won ~ p_server_beats_returner + ElapsedSeconds_fixed + speed_ratio, 
                       data = subset_f_second, family = "binomial")
summary(logit_model_3_f) ## p_server_beats_returner significant *** (pos coef), speed_ratio significant *** (pos coef)

#-----------------------------------------------------------------------------------------------------

## add score importance
logit_model_4_m <- glm(serving_player_won ~ p_server_beats_returner + ElapsedSeconds_fixed + speed_ratio + importance, 
                       data = subset_m_second, family = "binomial")
summary(logit_model_4_m) ## p_server_beats_returner significant *** (pos coef), speed_ratio significant *** (pos coef), importance significant *** (pos coef)

logit_model_4_f <- glm(serving_player_won ~ p_server_beats_returner + ElapsedSeconds_fixed + speed_ratio + importance,  
                       data = subset_f_second, family = "binomial")
summary(logit_model_4_f) ## p_server_beats_returner significant *** (pos coef), speed_ratio significant *** (pos coef), importance significant *** (pos coef)

#-----------------------------------------------------------------------------------------------------

## add location of serve
logit_model_5_m <- glm(serving_player_won ~ p_server_beats_returner + ElapsedSeconds_fixed + importance + speed_ratio + factor(ServeWidth) + factor(ServeDepth), 
                       data = subset_m_second, family = "binomial")
summary(logit_model_5_m) # p_server_beats_returner significant *** (pos coef), speed_ratio significant *** (pos coef), importance significant *** (pos coef), Servewidth W *** (pos coef), Servedepth NCTL *** (neg coef)
logit_model_5_f <- glm(serving_player_won ~ p_server_beats_returner + ElapsedSeconds_fixed + importance + speed_ratio + factor(ServeWidth) + factor(ServeDepth), 
                       data = subset_f_second, family = "binomial")
summary(logit_model_5_f) # p_server_beats_returner significant *** (pos coef), speed_ratio significant *** (pos coef), importance significant *** (pos coef), Servewidth W ** (pos coef), Servedepth NCTL ** (neg coef)

vif(logit_model_5_m) # acceptable, all less than 1.3ish
vif(logit_model_5_f)

#--- STEP 1: Extract coefficients and set typical values ---#
coefs_m <- coef(logit_model_5_m)

fixed_lp_m <- coefs_m["(Intercept)"] +
  coefs_m["p_server_beats_returner"] * mean(subset_m_second$p_server_beats_returner, na.rm = TRUE) +
  coefs_m["ElapsedSeconds_fixed"] * mean(subset_m_second$ElapsedSeconds_fixed, na.rm = TRUE) +
  coefs_m["importance"] * mean(subset_m_second$importance, na.rm = TRUE) +
  coefs_m["factor(ServeWidth)W"] * 1 +  # assume "W" for typical width
  coefs_m["factor(ServeDepth)NCTL"] * 1  # assume "NCTL" for typical depth

#--- STEP 2: Grid of speed_ratio values and linear predictions ---#
speed_vals_m <- seq(min(subset_m_second$speed_ratio, na.rm = TRUE),
                    max(subset_m_second$speed_ratio, na.rm = TRUE),
                    length.out = 200)

lp_m <- fixed_lp_m + coefs_m["speed_ratio"] * speed_vals_m
predicted_probs_m <- plogis(lp_m)

pred_df_m <- data.frame(
  speed_ratio = speed_vals_m,
  Probability = predicted_probs_m,
  Source = "Linear Prediction"
)

#--- STEP 3: Empirical win rate ---#
empirical_df_m <- subset_m_second %>%
  filter(!is.na(speed_ratio)) %>%
  mutate(speed_bin = cut(speed_ratio, breaks = seq(floor(min(speed_ratio)),
                                                   ceiling(max(speed_ratio)), by = 0.025))) %>%
  group_by(speed_bin) %>%
  summarise(
    speed_ratio = mean(speed_ratio, na.rm = TRUE),
    Probability = mean(serving_player_won == 1, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(Source = "Empirical Win Rate")

#--- STEP 4: Combine and plot ---#
plot_df_m <- bind_rows(empirical_df_m, pred_df_m)

ggplot(plot_df_m, aes(x = speed_ratio, y = Probability, color = Source)) +
  geom_line(size = 1.2) +
  labs(
    title = "Linear Prediction vs. Empirical Win Rate (Males, Second Serve)",
    x = "Speed Ratio (binned every 0.025 units)",
    y = "Probability Server Wins",
    color = "Source"
  ) +
  theme_minimal() +
  scale_color_manual(values = c("Empirical Win Rate" = "black", "Linear Prediction" = "blue"))

ggsave("../images/male_linear_second_ratio.png", bg = "white", width = 8, height = 6, units = "in")

#--- STEP 1: Extract coefficients and set typical values ---#
coefs_f <- coef(logit_model_5_f)

fixed_lp_f <- coefs_f["(Intercept)"] +
  coefs_f["p_server_beats_returner"] * mean(subset_f_second$p_server_beats_returner, na.rm = TRUE) +
  coefs_f["ElapsedSeconds_fixed"] * mean(subset_f_second$ElapsedSeconds_fixed, na.rm = TRUE) +
  coefs_f["importance"] * mean(subset_f_second$importance, na.rm = TRUE) +
  coefs_f["factor(ServeWidth)W"] * 1 +  # assume "W" for typical width
  coefs_f["factor(ServeDepth)NCTL"] * 1  # assume "NCTL" for typical depth

#--- STEP 2: Grid of speed_ratio values and linear predictions ---#
speed_vals_f <- seq(min(subset_f_second$speed_ratio, na.rm = TRUE),
                    max(subset_f_second$speed_ratio, na.rm = TRUE),
                    length.out = 200)

lp_f <- fixed_lp_f + coefs_f["speed_ratio"] * speed_vals_f
predicted_probs_f <- plogis(lp_f)

pred_df_f <- data.frame(
  speed_ratio = speed_vals_f,
  Probability = predicted_probs_f,
  Source = "Linear Prediction"
)

#--- STEP 3: Empirical win rate ---#
empirical_df_f <- subset_f_second %>%
  filter(!is.na(speed_ratio)) %>%
  mutate(speed_bin = cut(speed_ratio, breaks = seq(floor(min(speed_ratio)),
                                                   ceiling(max(speed_ratio)), by = 0.025))) %>%
  group_by(speed_bin) %>%
  summarise(
    speed_ratio = mean(speed_ratio, na.rm = TRUE),
    Probability = mean(serving_player_won == 1, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(Source = "Empirical Win Rate")

#--- STEP 4: Combine and plot ---#
plot_df_f <- bind_rows(empirical_df_f, pred_df_f)

ggplot(plot_df_f, aes(x = speed_ratio, y = Probability, color = Source)) +
  geom_line(size = 1.2) +
  labs(
    title = "Linear Prediction vs. Empirical Win Rate (Females, Second Serve)",
    x = "Speed Ratio (binned every 0.025 units)",
    y = "Probability Server Wins",
    color = "Source"
  ) +
  theme_minimal() +
  scale_color_manual(values = c("Empirical Win Rate" = "black", "Linear Prediction" = "blue"))

ggsave("../images/female_linear_second_ratio.png", bg = "white", width = 8, height = 6, units = "in")

#-----------------------------------------------------------------------------------------------------

## use serve speed instead of serve ratio

logit_model_6_m <- glm(serving_player_won ~ p_server_beats_returner + ElapsedSeconds_fixed + importance + Speed_MPH + factor(ServeWidth) + factor(ServeDepth), 
                       data = subset_m_second, family = "binomial")
summary(logit_model_6_m) # p_server_beats_returner significant *** (pos coef), importance significant *** (pos coef), Speed_MPH significant *** (pos coef), Servewidth W *** (pos coef), Servedepth NCTL *** (neg coef)
logit_model_6_f <- glm(serving_player_won ~ p_server_beats_returner + ElapsedSeconds_fixed + importance + Speed_MPH + factor(ServeWidth) + factor(ServeDepth), 
                       data = subset_f_second, family = "binomial")
summary(logit_model_6_f) # p_server_beats_returner significant *** (pos coef), importance significant *** (pos coef), Speed_MPH significant *** (pos coef), Servewidth W ** (pos coef), Servedepth NCTL ** (neg coef)

# logit_model_7_m <- glm(serving_player_won ~ p_server_beats_returner + ElapsedSeconds_fixed + importance + Speed_MPH + I(Speed_MPH**2) + factor(ServeWidth) + factor(ServeDepth), 
#                        data = subset_m_second, family = "binomial")
# summary(logit_model_7_m) # p_server_beats_returner significant *** (pos coef), importance significant *** (pos coef), Speed_MPH significant ** (neg coef), Speed_MPH^2 significant *** (pos coef), Servewidth W *** (pos coef), Servedepth NCTL *** (neg coef)
# logit_model_7_f <- glm(serving_player_won ~ p_server_beats_returner + ElapsedSeconds_fixed + importance + Speed_MPH + I(Speed_MPH**2) + factor(ServeWidth) + factor(ServeDepth), 
#                        data = subset_f_second, family = "binomial")
# summary(logit_model_7_f) # p_server_beats_returner significant *** (pos coef), importance significant *** (pos coef), Servewidth W ** (pos coef), Servedepth NCTL ** (neg coef)

logit_model_8_m <- glm(serving_player_won ~ p_server_beats_returner + ElapsedSeconds_fixed + importance + splines::bs(Speed_MPH, degree = 3, df = 5) + factor(ServeWidth) + factor(ServeDepth), 
                       data = subset_m_second, family = "binomial")
summary(logit_model_8_m) # p_server_beats_returner significant *** (pos coef), importance significant *** (pos coef), splines::bs(Speed_MPH, degree = 3, df = 5)5 * (pos coef), Servewidth W *** (pos coef), Servedepth NCTL *** (neg coef)
logit_model_8_f <- glm(serving_player_won ~ p_server_beats_returner + ElapsedSeconds_fixed + importance + splines::bs(Speed_MPH, degree = 3, df = 5) + factor(ServeWidth) + factor(ServeDepth), 
                       data = subset_f_second, family = "binomial")
summary(logit_model_8_f) # p_server_beats_returner significant *** (pos coef), importance significant *** (pos coef), splines::bs(Speed_MPH, degree = 3, df = 5)5 ** (pos coef), Servewidth W ** (pos coef), Servedepth NCTL ** (neg coef)

#-----------------------------------------------------------------------------------------------------

## quadratic and spline on speed_ratio
# logit_model_9_m <- glm(serving_player_won ~ p_server_beats_returner + ElapsedSeconds_fixed + importance + speed_ratio + I(speed_ratio**2) + factor(ServeWidth) + factor(ServeDepth), 
#                        data = subset_m_second, family = "binomial")
# summary(logit_model_9_m) # p_server_beats_returner significant *** (pos coef), importance significant *** (pos coef), speed_ratio significant ** (neg coef), speed_ratio^2 significant *** (pos coef), Servewidth W *** (pos coef), Servedepth NCTL *** (neg coef)
# logit_model_9_f <- glm(serving_player_won ~ p_server_beats_returner + ElapsedSeconds_fixed + importance + speed_ratio + I(speed_ratio**2) + factor(ServeWidth) + factor(ServeDepth), 
#                        data = subset_f_second, family = "binomial")
# summary(logit_model_9_f) # p_server_beats_returner significant *** (pos coef), importance significant *** (pos coef), Servewidth W ** (pos coef), Servedepth NCTL ** (neg coef)

logit_model_10_m <- glm(serving_player_won ~ p_server_beats_returner + ElapsedSeconds_fixed + importance + splines::bs(speed_ratio, degree = 3, df = 5) + factor(ServeWidth) + factor(ServeDepth), 
                       data = subset_m_second, family = "binomial")
summary(logit_model_10_m) # p_server_beats_returner significant *** (pos coef), importance significant *** (pos coef), Servewidth W *** (pos coef), Servedepth NCTL *** (neg coef)
logit_model_10_f <- glm(serving_player_won ~ p_server_beats_returner + ElapsedSeconds_fixed + importance + splines::bs(speed_ratio, degree = 3, df = 5) + factor(ServeWidth) + factor(ServeDepth), 
                       data = subset_f_second, family = "binomial")
summary(logit_model_10_f) # p_server_beats_returner significant *** (pos coef), importance significant *** (pos coef), Servewidth W *** (pos coef), Servedepth NCTL *** (neg coef)


#-----------------------------------------------------------------------------------------------------

# ## graph proportion of points won vs. speed_mph
# # Step 1: Bin Speed_MPH into intervals
# binned_data <- subset_f_second %>%
#   filter(!is.na(Speed_MPH)) %>%
#   mutate(speed_bin = cut(Speed_MPH, breaks = seq(floor(min(Speed_MPH)),
#                                                  ceiling(max(Speed_MPH)),
#                                                  by = 10))) %>%
#   group_by(speed_bin) %>%
#   summarise(
#     avg_speed = mean(Speed_MPH, na.rm = TRUE),
#     win_rate = mean(serving_player_won == 1, na.rm = TRUE),
#     n = n()
#   )
# 
# # Step 2: Plot
# ggplot(binned_data, aes(x = avg_speed, y = win_rate)) +
#   geom_point(size = 2) +
#   geom_line() +
#   labs(
#     title = "Empirical Win Rate vs. Second Serve Speed (Females)",
#     x = "Serve Speed (MPH)",
#     y = "Proportion of Points Won"
#   ) +
#   theme_minimal()
# ggsave("../images/female_second_serve_vs_win.png", bg = "white", 
#        width = 8, height = 6, units = "in")
# 
# ## males
# # Step 1: Bin Speed_MPH into intervals
# binned_data <- subset_m_second %>%
#   filter(!is.na(Speed_MPH)) %>%
#   mutate(speed_bin = cut(Speed_MPH, breaks = seq(floor(min(Speed_MPH)),
#                                                  ceiling(max(Speed_MPH)),
#                                                  by = 10))) %>%
#   group_by(speed_bin) %>%
#   summarise(
#     avg_speed = mean(Speed_MPH, na.rm = TRUE),
#     win_rate = mean(serving_player_won == 1, na.rm = TRUE),
#     n = n()
#   )
# 
# # Step 2: Plot
# ggplot(binned_data, aes(x = avg_speed, y = win_rate)) +
#   geom_point(size = 2) +
#   geom_line() +
#   labs(
#     title = "Empirical Win Rate vs. Second Serve Speed (Males)",
#     x = "Serve Speed (MPH)",
#     y = "Proportion of Points Won"
#   ) +
#   theme_minimal()
# ggsave("../images/male_second_serve_vs_win.png", bg = "white", 
#        width = 8, height = 6, units = "in")

#-----------------------------------------------------------------------------------------------------

# ## graph splines
# speed_vals <- seq(min(subset_f_second$Speed_MPH, na.rm = TRUE),
#                   max(subset_f_second$Speed_MPH, na.rm = TRUE),
#                   length.out = 200)
# 
# predict_df <- data.frame(
#   Speed_MPH = speed_vals,
#   p_server_beats_returner = mean(subset_f_second$p_server_beats_returner, na.rm = TRUE),
#   ElapsedSeconds_fixed = mean(subset_f_second$ElapsedSeconds_fixed, na.rm = TRUE),
#   importance = mean(subset_f_second$importance, na.rm = TRUE),
#   ServeWidth = "BC",  # choose a representative category
#   ServeDepth = "NCTL" # choose a representative category
# )
# 
# # Predict probabilities
# predict_df$prob_win <- predict(logit_model_8_f, newdata = predict_df, type = "response")
# 
# # Plot
# ggplot(predict_df, aes(x = Speed_MPH, y = prob_win)) +
#   geom_line(size = 1.2, color = "blue") +
#   labs(title = "Predicted Effect of Second Serve Speed on Probability of Winning Point (Females)",
#        x = "Serve Speed (MPH)",
#        y = "Predicted Probability") +
#   theme_minimal()
# ggsave("../images/female_second_serve_spline.png", bg = "white", 
#        width = 8, height = 6, units = "in")
# 
# ## graph splines for males
# speed_vals <- seq(min(subset_m_second$Speed_MPH, na.rm = TRUE),
#                   max(subset_m_second$Speed_MPH, na.rm = TRUE),
#                   length.out = 200)
# 
# predict_df <- data.frame(
#   Speed_MPH = speed_vals,
#   p_server_beats_returner = mean(subset_m_second$p_server_beats_returner, na.rm = TRUE),
#   ElapsedSeconds_fixed = mean(subset_m_second$ElapsedSeconds_fixed, na.rm = TRUE),
#   importance = mean(subset_m_second$importance, na.rm = TRUE),
#   ServeWidth = "BC",  # choose a representative category
#   ServeDepth = "NCTL" # choose a representative category
# )
# 
# # Predict probabilities
# predict_df$prob_win <- predict(logit_model_8_m, newdata = predict_df, type = "response")
# 
# # Plot
# ggplot(predict_df, aes(x = Speed_MPH, y = prob_win)) +
#   geom_line(size = 1.2, color = "blue") +
#   labs(title = "Predicted Effect of Second Serve Speed on Probability of Winning Point (Males)",
#        x = "Serve Speed (MPH)",
#        y = "Predicted Probability") +
#   theme_minimal()
# ggsave("../images/male_second_serve_spline.png", bg = "white", 
#        width = 8, height = 6, units = "in")
# 
# # Basis matrix for cubic spline
# basis_mat <- as.data.frame(splines::bs(speed_vals, degree = 3, df = 5))
# colnames(basis_mat) <- paste0("bs", 1:5)
# basis_mat$Speed_MPH <- speed_vals
# 
# basis_long <- pivot_longer(basis_mat, cols = starts_with("bs"), names_to = "basis", values_to = "value")
# 
# ggplot(basis_long, aes(x = Speed_MPH, y = value, color = basis)) +
#   geom_line(size = 1) +
#   labs(title = "Cubic Spline Basis Functions for Speed_MPH",
#        y = "Basis Function Value") +
#   theme_minimal()
# ggsave("../images/cubic_spline.png", bg = "white", 
#        width = 8, height = 6, units = "in")

#-----------------------------------------------------------------------------------------------------

## graph splines using coefficients from model, and overlay with graphs of proportion of points won vs. binned speed_mph
#--- STEP 1: Generate spline-only prediction from the model ---#
#--- STEP 1: Generate spline predictions ---#
speed_vals <- seq(min(subset_f_second$Speed_MPH, na.rm = TRUE),
                  max(subset_f_second$Speed_MPH, na.rm = TRUE),
                  length.out = 200)

spline_basis <- as.data.frame(splines::bs(speed_vals, degree = 3, df = 5))
colnames(spline_basis) <- paste0("bs", 1:5)
spline_basis$Speed_MPH <- speed_vals

spline_coefs <- coef(logit_model_8_f)[grep("bs\\(Speed_MPH", names(coef(logit_model_8_f)))]

spline_lp <- as.matrix(spline_basis[, paste0("bs", 1:5)]) %*% spline_coefs
spline_prob <- plogis(spline_lp)

spline_df <- data.frame(
  Speed_MPH = speed_vals,
  Probability = spline_prob,
  Source = "Spline Prediction"
)

#--- STEP 1.5: Find optimal speed ---#
optimal_point <- spline_df[which.max(spline_df$Probability), ]

#--- STEP 2: Empirical win rate ---#
empirical_df <- subset_f_second %>%
  filter(!is.na(Speed_MPH)) %>%
  mutate(speed_bin = cut(Speed_MPH, breaks = seq(floor(min(Speed_MPH)),
                                                 ceiling(max(Speed_MPH)),
                                                 by = 5))) %>%
  group_by(speed_bin) %>%
  summarise(
    Speed_MPH = mean(Speed_MPH, na.rm = TRUE),
    Probability = mean(serving_player_won == 1, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(Source = "Empirical Win Rate")

#--- STEP 3: Combine both for plotting ---#
plot_df <- bind_rows(empirical_df, spline_df)

#--- STEP 4: Plot ---#
ggplot(plot_df, aes(x = Speed_MPH, y = Probability, color = Source)) +
  geom_line(size = 1.2) +
  geom_point(data = optimal_point, aes(x = Speed_MPH, y = Probability), 
             color = "blue", size = 3, shape = 18) +
  geom_text(data = optimal_point, aes(x = Speed_MPH, y = Probability, 
                                      label = paste0("Max: ", round(Speed_MPH, 1), " MPH")), 
            vjust = -1, color = "blue", fontface = "bold") +
  labs(
    title = "Spline-Based vs. Empirical Win Rate (Females, Second Serve)",
    x = "Serve Speed (MPH) (binned every 5 MPH)",
    y = "Probability Server Wins",
    color = "Source"
  ) +
  theme_minimal() +
  scale_color_manual(values = c("Empirical Win Rate" = "black", "Spline Prediction" = "red"))

ggsave("../images/female_spline_second_speed.png", bg = "white", width = 8, height = 6, units = "in")

#-----------------------------------------------------------------------------------------------------

## same thing but for males
#--- STEP 1: Generate spline-only prediction from the model ---#
speed_vals <- seq(min(subset_m_second$Speed_MPH, na.rm = TRUE),
                  max(subset_m_second$Speed_MPH, na.rm = TRUE),
                  length.out = 200)

spline_basis <- as.data.frame(splines::bs(speed_vals, degree = 3, df = 5))
colnames(spline_basis) <- paste0("bs", 1:5)
spline_basis$Speed_MPH <- speed_vals

spline_coefs <- coef(logit_model_8_m)[grep("bs\\(Speed_MPH", names(coef(logit_model_8_m)))]

spline_lp <- as.matrix(spline_basis[, paste0("bs", 1:5)]) %*% spline_coefs
spline_prob <- plogis(spline_lp)

spline_df <- data.frame(
  Speed_MPH = speed_vals,
  Probability = spline_prob,
  Source = "Spline Prediction"
)

#--- STEP 1.5: Find optimal speed ---#
optimal_point <- spline_df[which.max(spline_df$Probability), ]

#--- STEP 2: Empirical win rate ---#
empirical_df <- subset_m_second %>%
  filter(!is.na(Speed_MPH)) %>%
  mutate(speed_bin = cut(Speed_MPH, breaks = seq(floor(min(Speed_MPH)),
                                                 ceiling(max(Speed_MPH)),
                                                 by = 5))) %>%
  group_by(speed_bin) %>%
  summarise(
    Speed_MPH = mean(Speed_MPH, na.rm = TRUE),
    Probability = mean(serving_player_won == 1, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(Source = "Empirical Win Rate")

#--- STEP 3: Combine both for plotting ---#
plot_df <- bind_rows(empirical_df, spline_df)

#--- STEP 4: Plot ---#
ggplot(plot_df, aes(x = Speed_MPH, y = Probability, color = Source)) +
  geom_line(size = 1.2) +
  geom_point(data = optimal_point, aes(x = Speed_MPH, y = Probability), 
             color = "blue", size = 3, shape = 18) +
  geom_text(data = optimal_point, aes(x = Speed_MPH, y = Probability, 
                                      label = paste0("Max: ", round(Speed_MPH, 1), " MPH")), 
            vjust = -1, color = "blue", fontface = "bold") +
  labs(
    title = "Spline-Based vs. Empirical Win Rate (Males, Second Serve)",
    x = "Serve Speed (MPH) (binned every 5 MPH)",
    y = "Probability Server Wins",
    color = "Source"
  ) +
  theme_minimal() +
  scale_color_manual(values = c("Empirical Win Rate" = "black", "Spline Prediction" = "red"))

ggsave("../images/male_spline_second_speed.png", bg = "white", width = 8, height = 6, units = "in")

#-----------------------------------------------------------------------------------------------------

## what we've haven't done: graph quadratic for speed_MPH, and graph splines for speed_ratio.

## graph quadratic for speed_ratio using coefficients from model, and overlay with graphs of proportion of points won vs. binned speed_ratio
#--- STEP 1: Get coefficients from model ---#
coefs <- coef(logit_model_9_f)

# Extract relevant coefficients
intercept <- coefs["(Intercept)"]
beta_speed <- coefs["speed_ratio"]
beta_speed2 <- coefs["I(speed_ratio^2)"]

# To isolate the effect of Speed_MPH only, set other covariates to typical values
fixed_lp <- intercept +
  coefs["p_server_beats_returner"] * mean(subset_f_second$p_server_beats_returner, na.rm = TRUE) +
  coefs["ElapsedSeconds_fixed"] * mean(subset_f_second$ElapsedSeconds_fixed, na.rm = TRUE) +
  coefs["importance"] * mean(subset_f_second$importance, na.rm = TRUE) +
  coefs["factor(ServeWidth)BC"] * 1 +  # set one-hot encoding for baseline category
  coefs["factor(ServeDepth)NCTL"] * 1 # assume typical depth

#--- STEP 2: Create grid of Speed_MPH values and compute prediction ---#
speed_vals <- seq(min(subset_f_second$speed_ratio, na.rm = TRUE),
                  max(subset_f_second$speed_ratio, na.rm = TRUE),
                  length.out = 200)

quad_lp <- fixed_lp + beta_speed * speed_vals + beta_speed2 * speed_vals^2
predicted_probs <- plogis(quad_lp)

quad_df <- data.frame(
  speed_ratio = speed_vals,
  Probability = predicted_probs,
  Source = "Quadratic Prediction"
)

#--- STEP 3: Empirical binned win rate ---#
empirical_df <- subset_f_second %>%
  filter(!is.na(speed_ratio)) %>%
  mutate(speed_bin = cut(speed_ratio, breaks = seq(floor(min(speed_ratio)),
                                                 ceiling(max(speed_ratio)),
                                                 by = 0.025))) %>%
  group_by(speed_bin) %>%
  summarise(
    speed_ratio = mean(speed_ratio, na.rm = TRUE),
    Probability = mean(serving_player_won == 1, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(Source = "Empirical Win Rate")

#--- STEP 4: Combine and plot ---#
plot_df <- bind_rows(empirical_df, quad_df)

ggplot(plot_df, aes(x = speed_ratio, y = Probability, color = Source)) +
  geom_line(size = 1.2) +
  labs(
    title = "Quadratic Prediction vs. Empirical Win Rate (Females, Second Serve)",
    x = "Speed Ratio (binned every 0.025 units)",
    y = "Probability Server Wins",
    color = "Source"
  ) +
  theme_minimal() +
  scale_color_manual(values = c("Empirical Win Rate" = "black", "Quadratic Prediction" = "red"))
ggsave("../images/female_quadratic_second_ratio.png", bg = "white", width = 8, height = 6, units = "in")

#-----------------------------------------------------------------------------------------------------

## same thing for males
##--- STEP 1: Get coefficients from model ---#
coefs <- coef(logit_model_9_m)

# Extract relevant coefficients
intercept <- coefs["(Intercept)"]
beta_speed <- coefs["speed_ratio"]
beta_speed2 <- coefs["I(speed_ratio^2)"]

# To isolate the effect of Speed_MPH only, set other covariates to typical values
fixed_lp <- intercept +
  coefs["p_server_beats_returner"] * mean(subset_m_second$p_server_beats_returner, na.rm = TRUE) +
  coefs["ElapsedSeconds_fixed"] * mean(subset_m_second$ElapsedSeconds_fixed, na.rm = TRUE) +
  coefs["importance"] * mean(subset_m_second$importance, na.rm = TRUE) +
  coefs["factor(ServeWidth)BC"] * 1 +  # set one-hot encoding for baseline category
  coefs["factor(ServeDepth)NCTL"] * 1 # assume typical depth

#--- STEP 2: Create grid of Speed_MPH values and compute prediction ---#
speed_vals <- seq(min(subset_m_second$speed_ratio, na.rm = TRUE),
                  max(subset_m_second$speed_ratio, na.rm = TRUE),
                  length.out = 200)

quad_lp <- fixed_lp + beta_speed * speed_vals + beta_speed2 * speed_vals^2
predicted_probs <- plogis(quad_lp)

quad_df <- data.frame(
  speed_ratio = speed_vals,
  Probability = predicted_probs,
  Source = "Quadratic Prediction"
)

#--- STEP 3: Empirical binned win rate ---#
empirical_df <- subset_m_second %>%
  filter(!is.na(speed_ratio)) %>%
  mutate(speed_bin = cut(speed_ratio, breaks = seq(floor(min(speed_ratio)),
                                                   ceiling(max(speed_ratio)),
                                                   by = 0.025))) %>%
  group_by(speed_bin) %>%
  summarise(
    speed_ratio = mean(speed_ratio, na.rm = TRUE),
    Probability = mean(serving_player_won == 1, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(Source = "Empirical Win Rate")

#--- STEP 4: Combine and plot ---#
plot_df <- bind_rows(empirical_df, quad_df)

ggplot(plot_df, aes(x = speed_ratio, y = Probability, color = Source)) +
  geom_line(size = 1.2) +
  labs(
    title = "Quadratic Prediction vs. Empirical Win Rate (Males, Second Serve)",
    x = "Speed Ratio (binned every 0.025 units)",
    y = "Probability Server Wins",
    color = "Source"
  ) +
  theme_minimal() +
  scale_color_manual(values = c("Empirical Win Rate" = "black", "Quadratic Prediction" = "red"))
ggsave("../images/male_quadratic_first_second_ratio.png", bg = "white", width = 8, height = 6, units = "in")
#-----------------------------------------------------------------------------------------------------

## spline males speed_ratio
#--- STEP 1: Generate spline-only prediction from the model ---#
speed_vals <- seq(min(subset_m_second$speed_ratio, na.rm = TRUE),
                  max(subset_m_second$speed_ratio, na.rm = TRUE),
                  length.out = 200)

spline_basis <- as.data.frame(splines::bs(speed_vals, degree = 3, df = 5))
colnames(spline_basis) <- paste0("bs", 1:5)
spline_basis$speed_ratio <- speed_vals

spline_coefs <- coef(logit_model_10_m)[grep("bs\\(speed_ratio", names(coef(logit_model_10_m)))]

spline_lp <- as.matrix(spline_basis[, paste0("bs", 1:5)]) %*% spline_coefs
spline_prob <- plogis(spline_lp)

spline_df <- data.frame(
  speed_ratio = speed_vals,
  Probability = spline_prob,
  Source = "Spline Prediction"
)

#--- STEP 1.5: Find optimal speed ---#
optimal_point <- spline_df[which.max(spline_df$Probability), ]

#--- STEP 2: Empirical win rate ---#
# STEP 1: Count number of points each server played
names(subset_m_second)
player_point_counts <- subset_m_second %>%
  count(server = if_else(PointServer == 1, player1_name, player2_name)) %>%
  rename(n_points = n)

# STEP 2: Join back to main data and calculate weight = 1 / n_points
subset_weighted <- subset_m_second %>%
  mutate(server = if_else(PointServer == 1, player1_name, player2_name)) %>%
  left_join(player_point_counts, by = "server") %>%
  mutate(weight = 1 / n_points)

# STEP 3: Compute weighted empirical win rate by speed_ratio bin
empirical_df <- subset_weighted %>%
  filter(!is.na(speed_ratio)) %>%
  mutate(speed_bin = cut(speed_ratio, 
                         breaks = seq(floor(min(speed_ratio, na.rm = TRUE)), 
                                      ceiling(max(speed_ratio, na.rm = TRUE)), 
                                      by = 0.025))) %>%
  group_by(speed_bin) %>%
  summarise(
    speed_ratio = weighted.mean(speed_ratio, w = weight, na.rm = TRUE),
    Probability = weighted.mean(serving_player_won == 1, w = weight, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(Source = "Empirical Win Rate (Weighted)")

#--- STEP 3: Combine both for plotting ---#
plot_df <- bind_rows(empirical_df, spline_df)

#--- STEP 4: Plot ---#
ggplot(plot_df, aes(x = speed_ratio, y = Probability, color = Source)) +
  geom_line(size = 1.2) +
  geom_point(data = optimal_point, aes(x = speed_ratio, y = Probability), 
             color = "blue", size = 3, shape = 18) +
  geom_text(data = optimal_point, aes(x = speed_ratio, y = Probability, 
                                      label = paste0("Max: ", round(speed_ratio, 1))), 
            vjust = -1, color = "blue", fontface = "bold") +
  labs(
    title = "Spline-Based vs. Empirical Win Rate (Males, Second Serve)",
    x = "Speed Ratio (binned every 0.025 units)",
    y = "Probability Server Wins",
    color = "Source"
  ) +
  theme_minimal() +
  scale_color_manual(values = c("Empirical Win Rate (Weighted)" = "black", "Spline Prediction" = "red"))

ggsave("../images/male_spline_second_speed_weighted.png", bg = "white", width = 8, height = 6, units = "in")

#--- STEP 1: Generate spline-only prediction from the model ---#
speed_vals <- seq(min(subset_m_second$speed_ratio, na.rm = TRUE),
                  max(subset_m_second$speed_ratio, na.rm = TRUE),
                  length.out = 200)

spline_basis <- as.data.frame(splines::bs(speed_vals, degree = 3, df = 5))
colnames(spline_basis) <- paste0("bs", 1:5)
spline_basis$speed_ratio <- speed_vals

spline_coefs <- coef(logit_model_10_m)[grep("bs\\(speed_ratio", names(coef(logit_model_10_m)))]

spline_lp <- as.matrix(spline_basis[, paste0("bs", 1:5)]) %*% spline_coefs
spline_prob <- plogis(spline_lp)

spline_df <- data.frame(
  speed_ratio = speed_vals,
  Probability = spline_prob,
  Source = "Spline Prediction"
)

#--- STEP 1.5: Find optimal speed ---#
optimal_point <- spline_df[which.max(spline_df$Probability), ]

#--- STEP 2: Empirical win rate ---#
# STEP 1: Assign server identity
subset_m_second <- subset_m_second %>%
  mutate(server = if_else(PointServer == 1, player1_name, player2_name))

# STEP 2: Bin speed_ratio
subset_m_binned <- subset_m_second %>%
  filter(!is.na(speed_ratio)) %>%
  mutate(speed_bin = cut(speed_ratio,
                         breaks = seq(floor(min(speed_ratio, na.rm = TRUE)),
                                      ceiling(max(speed_ratio, na.rm = TRUE)),
                                      by = 0.025)))

# STEP 3: Calculate per-player win rates per bin
per_player_df <- subset_m_binned %>%
  group_by(server, speed_bin) %>%
  summarise(
    speed_ratio = mean(speed_ratio, na.rm = TRUE),
    player_win_rate = mean(serving_player_won == 1, na.rm = TRUE),
    .groups = "drop"
  )

# STEP 4: Average across players per bin
empirical_df <- per_player_df %>%
  group_by(speed_bin) %>%
  summarise(
    speed_ratio = mean(speed_ratio, na.rm = TRUE),
    Probability = mean(player_win_rate, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(Source = "Empirical Win Rate (Player-Averaged)")

#--- STEP 3: Combine both for plotting ---#
plot_df <- bind_rows(empirical_df, spline_df)

#--- STEP 4: Plot ---#
ggplot(plot_df, aes(x = speed_ratio, y = Probability, color = Source)) +
  geom_line(size = 1.2) +
  geom_point(data = optimal_point, aes(x = speed_ratio, y = Probability), 
             color = "blue", size = 3, shape = 18) +
  geom_text(data = optimal_point, aes(x = speed_ratio, y = Probability, 
                                      label = paste0("Max: ", round(speed_ratio, 1))), 
            vjust = -1, color = "blue", fontface = "bold") +
  labs(
    title = "Spline-Based vs. Empirical Win Rate (Males, Second Serve)",
    x = "Speed Ratio (binned every 0.025 units)",
    y = "Probability Server Wins",
    color = "Source"
  ) +
  theme_minimal() +
  scale_color_manual(values = c("Empirical Win Rate (Player-Averaged)" = "black", "Spline Prediction" = "red"))

#-----------------------------------------------------------------------------------------------------


# player_names <- unique(c(subset_m$player1_name, subset_m$player2_name))
# 
# # Create a design matrix with columns: one column per player
# X <- matrix(0, nrow = nrow(subset_m), ncol = length(player_names))
# colnames(X) <- c(paste0("B_", player_names))
# 
# ## loop through each row in subset_m (which corresponds with each row in X)
# # put 1 in the column corresponding to player1 and -1 in the column corresponding to player2
# for (i in 1:nrow(subset_m)) {
#   row <- subset_m[i, ]
#   player1 <- row$player1_name
#   player2 <- row$player2_name
#   winner <- row$PointWinner
#   
#   # Set +1 for player1, -1 for player2
#   X[i, paste0("B_", player1)] <- 1
#   X[i, paste0("B_", player2)] <- -1
#   
#   # Store outcome (1 if player1 won, 0 if player2 won)
#   subset_m$winner[i] <- ifelse(winner == player1, 1, 0)
# }
