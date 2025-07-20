# --- Load libraries ---
rm(list = ls())
library(tidyverse)
library(data.table)
library(lme4)

# --- Load data --- 
# change wimbledon or usopen as desired
subset_m <- fread("../data/output/scaled-results/usopen_m_train_scaled.csv")
subset_f <- fread("../data/output/scaled-results/usopen_f_train_scaled.csv")

names(subset_m)
unique(subset_f$year)

# --- Add server_name column ---
subset_m <- subset_m %>%
  mutate(server_name = if_else(PointServer == 1, player1, player2))
subset_f <- subset_f %>%
  mutate(server_name = if_else(PointServer == 1, player1, player2))

# --- Split into serve groups ---
m_first  <- subset_m[ServeNumber == 1 & Speed_MPH > 0]
m_second <- subset_m[ServeNumber == 2 & Speed_MPH > 0]
f_first  <- subset_f[ServeNumber == 1 & Speed_MPH > 0]
f_second <- subset_f[ServeNumber == 2 & Speed_MPH > 0]

groups <- list(
  m_first  = m_first,
  m_second = m_second,
  f_first  = f_first,
  f_second = f_second
)

# --- Save model summaries to a text file ---
sink("../data/output/new-model-results/player-effects/usopen_model_summaries.txt")

# --- Fit mixed-effects models and print summaries ---
for (group_name in names(groups)) {
  df <- groups[[group_name]]
  
  cat("\n==============================\n")
  cat("Model Summary:", group_name, "(Speed_MPH_z)\n")
  cat("==============================\n")
  model_speed <- glmer(serving_player_won ~ p_server_beats_returner_z + Speed_MPH_z +
                         importance_z + df_pct_server_z + ElapsedSeconds_fixed_z +
                         factor(ServeWidth) + factor(ServeDepth) + (1 | server_name),
                       data = df, family = binomial)
  print(summary(model_speed))
  
  cat("\n==============================\n")
  cat("Model Summary:", group_name, "(speed_ratio_z)\n")
  cat("==============================\n")
  model_ratio <- glmer(serving_player_won ~ p_server_beats_returner_z + speed_ratio_z +
                         importance_z + df_pct_server_z + ElapsedSeconds_fixed_z +
                         factor(ServeWidth) + factor(ServeDepth) + (1 | server_name),
                       data = df, family = binomial)
  print(summary(model_ratio))
}
sink()

ranef(model_speed)$server_name



# --- new code to get images ---

# --- Load libraries ---
rm(list = ls())
library(tidyverse)
library(data.table)
library(lme4)
library(ggplot2)

# --- Load data ---
subset_m <- fread("../data/output/scaled-results/usopen_m_train_scaled.csv")
subset_f <- fread("../data/output/scaled-results/usopen_f_train_scaled.csv")

# --- Add server_name column ---
subset_m <- subset_m %>%
  mutate(server_name = if_else(PointServer == 1, player1, player2))
subset_f <- subset_f %>%
  mutate(server_name = if_else(PointServer == 1, player1, player2))

# --- Split into serve groups ---
m_first  <- subset_m[ServeNumber == 1 & Speed_MPH > 0]
m_second <- subset_m[ServeNumber == 2 & Speed_MPH > 0]
f_first  <- subset_f[ServeNumber == 1 & Speed_MPH > 0]
f_second <- subset_f[ServeNumber == 2 & Speed_MPH > 0]

groups <- list(
  m_first  = m_first,
  m_second = m_second,
  f_first  = f_first,
  f_second = f_second
)

# --- Prepare output list ---
all_performers <- list()

# --- Loop over serve groups and run models ---
for (group_name in names(groups)) {
  df <- groups[[group_name]]
  
  # Fit model with Speed_MPH_z
  model_speed <- glmer(serving_player_won ~ p_server_beats_returner_z + Speed_MPH_z +
                         importance_z + df_pct_server_z + ElapsedSeconds_fixed_z +
                         factor(ServeWidth) + factor(ServeDepth) + (1 | server_name),
                       data = df, family = binomial)
  
  # Extract random intercepts
  ranef_df <- ranef(model_speed)$server_name %>%
    rownames_to_column("server_name") %>%
    rename(random_intercept = `(Intercept)`) %>%
    mutate(group = group_name, model = "Speed_MPH_z")
  
  # Identify over/under-performers (top/bottom 10 by random intercept)
  ranef_df <- ranef_df %>%
    mutate(rank = rank(-random_intercept),
           label = case_when(
             rank <= 10 ~ "Over-performer",
             rank > (n() - 10) ~ "Under-performer",
             TRUE ~ "Average"
           ))
  
  # Save plot
  p <- ggplot(ranef_df, aes(x = reorder(server_name, random_intercept),
                            y = random_intercept,
                            fill = label)) +
    geom_col(show.legend = FALSE) +
    coord_flip() +
    labs(title = paste("Random Effects —", group_name, "(Speed_MPH_z)"),
         x = "Server", y = "Random Intercept (Baseline Adjustment)") +
    scale_fill_manual(values = c("Over-performer" = "darkgreen",
                                 "Under-performer" = "darkred",
                                 "Average" = "grey70")) +
    theme_minimal()
  
  ggsave(paste0("../images/player effects/", group_name, "_speed_ranef.png"), plot = p,
         width = 8, height = 10, units = "in", bg = "white")
  
  # Append to full list
  all_performers[[paste0(group_name, "_speed")]] <- ranef_df
  
  # Repeat for model with speed_ratio_z
  model_ratio <- glmer(serving_player_won ~ p_server_beats_returner_z + speed_ratio_z +
                         importance_z + df_pct_server_z + ElapsedSeconds_fixed_z +
                         factor(ServeWidth) + factor(ServeDepth) + (1 | server_name),
                       data = df, family = binomial)
  
  ranef_df_ratio <- ranef(model_ratio)$server_name %>%
    rownames_to_column("server_name") %>%
    rename(random_intercept = `(Intercept)`) %>%
    mutate(group = group_name, model = "speed_ratio_z")
  
  ranef_df_ratio <- ranef_df_ratio %>%
    mutate(rank = rank(-random_intercept),
           label = case_when(
             rank <= 10 ~ "Over-performer",
             rank > (n() - 10) ~ "Under-performer",
             TRUE ~ "Average"
           ))
  
  # Save plot
  p2 <- ggplot(ranef_df_ratio, aes(x = reorder(server_name, random_intercept),
                                   y = random_intercept,
                                   fill = label)) +
    geom_col(show.legend = FALSE) +
    coord_flip() +
    labs(title = paste("Random Effects —", group_name, "(speed_ratio_z)"),
         x = "Server", y = "Random Intercept (Baseline Adjustment)") +
    scale_fill_manual(values = c("Over-performer" = "darkgreen",
                                 "Under-performer" = "darkred",
                                 "Average" = "grey70")) +
    theme_minimal()
  
  ggsave(paste0("../images/player effects/", group_name, "_ratio_ranef.png"), plot = p2,
         width = 8, height = 10, units = "in", bg = "white")
  
  all_performers[[paste0(group_name, "_ratio")]] <- ranef_df_ratio
}

# --- Combine all tagged random effects ---
player_effects_all <- bind_rows(all_performers)

# View the result in the console if you want
View(player_effects_all)

# Optionally save to CSV
fwrite(player_effects_all, "../data/output/new-model-results/player-effects/usopen_player_random_effects.csv")

# Assign numeric scores to performance labels
score_df <- player_effects_all %>%
  mutate(score = case_when(
    label == "Over-performer" ~ 1,
    label == "Under-performer" ~ -1,
    TRUE ~ 0
  ))

# Compute average score per player
player_score_summary <- score_df %>%
  group_by(server_name) %>%
  summarise(
    n_models = n(),
    avg_score = mean(score),
    total_score = sum(score),
    .groups = "drop"
  ) %>%
  arrange(desc(avg_score))  # optional: sort best to worst

# View or save
View(player_score_summary)
fwrite(player_score_summary, "../data/output/new-model-results/player-effects/usopen_player_performance_scores.csv")


# --- use 2 sd's away from mean instead of top/bottom 10---


# --- new code to get images ---

# --- Load libraries ---
rm(list = ls())
library(tidyverse)
library(data.table)
library(lme4)
library(ggplot2)

# --- Load data ---
subset_m <- fread("../data/output/scaled-results/wimbledon_m_train_scaled.csv")
subset_f <- fread("../data/output/scaled-results/wimbledon_f_train_scaled.csv")

# --- Add server_name column ---
subset_m <- subset_m %>%
  mutate(server_name = if_else(PointServer == 1, player1, player2))
subset_f <- subset_f %>%
  mutate(server_name = if_else(PointServer == 1, player1, player2))

# --- Split into serve groups ---
m_first  <- subset_m[ServeNumber == 1 & Speed_MPH > 0]
m_second <- subset_m[ServeNumber == 2 & Speed_MPH > 0]
f_first  <- subset_f[ServeNumber == 1 & Speed_MPH > 0]
f_second <- subset_f[ServeNumber == 2 & Speed_MPH > 0]

groups <- list(
  m_first  = m_first,
  m_second = m_second,
  f_first  = f_first,
  f_second = f_second
)

# --- Prepare output list ---
all_performers <- list()

# --- Loop over serve groups and run models ---
for (group_name in names(groups)) {
  df <- groups[[group_name]]
  
  # Fit model with Speed_MPH_z
  model_speed <- glmer(serving_player_won ~ p_server_beats_returner_z + Speed_MPH_z +
                         importance_z + df_pct_server_z + ElapsedSeconds_fixed_z +
                         factor(ServeWidth) + factor(ServeDepth) + (1 | server_name),
                       data = df, family = binomial)
  
  # Extract random intercepts
  ranef_df <- ranef(model_speed)$server_name %>%
    rownames_to_column("server_name") %>%
    rename(random_intercept = `(Intercept)`) %>%
    mutate(group = group_name, model = "Speed_MPH_z")
  
  # Label over-/under-performers using 2 SD threshold
  ranef_df <- ranef_df %>%
    mutate(
      mu = mean(random_intercept),
      sd = sd(random_intercept),
      label = case_when(
        random_intercept >= mu + 2 * sd ~ "Over-performer",
        random_intercept <= mu - 2 * sd ~ "Under-performer",
        TRUE ~ "Average"
      )
    ) %>%
    select(-mu, -sd)
  
  # Save plot
  p <- ggplot(ranef_df, aes(x = reorder(server_name, random_intercept),
                            y = random_intercept,
                            fill = label)) +
    geom_col(show.legend = FALSE) +
    coord_flip() +
    labs(title = paste("Random Effects —", group_name, "(Speed_MPH_z)"),
         x = "Server", y = "Random Intercept (Baseline Adjustment)") +
    scale_fill_manual(values = c("Over-performer" = "darkgreen",
                                 "Under-performer" = "darkred",
                                 "Average" = "grey70")) +
    theme_minimal()
  
  ggsave(paste0("../images/player effects/", group_name, "_speed_ranef.png"), plot = p,
         width = 8, height = 10, units = "in", bg = "white")
  
  all_performers[[paste0(group_name, "_speed")]] <- ranef_df
  
  # Repeat for model with speed_ratio_z
  model_ratio <- glmer(serving_player_won ~ p_server_beats_returner_z + speed_ratio_z +
                         importance_z + df_pct_server_z + ElapsedSeconds_fixed_z +
                         factor(ServeWidth) + factor(ServeDepth) + (1 | server_name),
                       data = df, family = binomial)
  
  ranef_df_ratio <- ranef(model_ratio)$server_name %>%
    rownames_to_column("server_name") %>%
    rename(random_intercept = `(Intercept)`) %>%
    mutate(group = group_name, model = "speed_ratio_z")
  
  ranef_df_ratio <- ranef_df_ratio %>%
    mutate(
      mu = mean(random_intercept),
      sd = sd(random_intercept),
      label = case_when(
        random_intercept >= mu + 2 * sd ~ "Over-performer",
        random_intercept <= mu - 2 * sd ~ "Under-performer",
        TRUE ~ "Average"
      )
    ) %>%
    select(-mu, -sd)
  
  # Save plot
  p2 <- ggplot(ranef_df_ratio, aes(x = reorder(server_name, random_intercept),
                                   y = random_intercept,
                                   fill = label)) +
    geom_col(show.legend = FALSE) +
    coord_flip() +
    labs(title = paste("Random Effects —", group_name, "(speed_ratio_z)"),
         x = "Server", y = "Random Intercept (Baseline Adjustment)") +
    scale_fill_manual(values = c("Over-performer" = "darkgreen",
                                 "Under-performer" = "darkred",
                                 "Average" = "grey70")) +
    theme_minimal()
  
  ggsave(paste0("../images/player effects/", group_name, "_ratio_ranef.png"), plot = p2,
         width = 8, height = 10, units = "in", bg = "white")
  
  all_performers[[paste0(group_name, "_ratio")]] <- ranef_df_ratio
}

# --- Combine all tagged random effects ---
player_effects_all <- bind_rows(all_performers)

# View the result in the console
View(player_effects_all)

# Save to CSV
fwrite(player_effects_all, "../data/output/new-model-results/player-effects/wimbledon_player_random_effects.csv")

# --- Assign numeric scores to performance labels ---
score_df <- player_effects_all %>%
  mutate(score = case_when(
    label == "Over-performer" ~ 1,
    label == "Under-performer" ~ -1,
    TRUE ~ 0
  ))

# --- Compute average score per player ---
player_score_summary <- score_df %>%
  group_by(server_name) %>%
  summarise(
    n_models = n(),
    avg_score = mean(score),
    total_score = sum(score),
    .groups = "drop"
  ) %>%
  arrange(desc(avg_score))

# View or save
View(player_score_summary)
fwrite(player_score_summary, "../data/output/new-model-results/player-effects/wimbledon_player_performance_scores.csv")
