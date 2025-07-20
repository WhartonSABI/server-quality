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
groups <- list(
  m_first  = subset_m[ServeNumber == 1 & Speed_MPH > 0],
  m_second = subset_m[ServeNumber == 2 & Speed_MPH > 0],
  f_first  = subset_f[ServeNumber == 1 & Speed_MPH > 0],
  f_second = subset_f[ServeNumber == 2 & Speed_MPH > 0]
)

# --- Prepare storage ---
all_effects <- list()
top_bottom <- list()
extreme_sd <- list()

# --- Create output directory for plots ---
output_dir <- "../images/player effects/by_year"
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

# --- Loop over serve groups and years ---
for (group_name in names(groups)) {
  df_group <- groups[[group_name]]
  years <- unique(df_group$year)
  
  for (yr in years) {
    df_year <- df_group %>% filter(year == yr)
    model_id <- paste0(group_name, "_", yr)
    
    # Fit model
    model <- glmer(serving_player_won ~ p_server_beats_returner_z + Speed_MPH_z +
                     importance_z + df_pct_server_z + ElapsedSeconds_fixed_z +
                     factor(ServeWidth) + factor(ServeDepth) + (1 | server_name),
                   data = df_year, family = binomial)
    
    # Extract random intercepts
    ranef_df <- ranef(model)$server_name %>%
      rownames_to_column("server_name") %>%
      rename(random_intercept = `(Intercept)`) %>%
      mutate(model = model_id, year = yr, group = group_name)
    
    all_effects[[model_id]] <- ranef_df
    
    # --- Top / Bottom 10 ---
    top10 <- ranef_df %>%
      arrange(desc(random_intercept)) %>%
      slice(1:10) %>%
      mutate(rank_label = "Top10")
    
    bottom10 <- ranef_df %>%
      arrange(random_intercept) %>%
      slice(1:10) %>%
      mutate(rank_label = "Bottom10")
    
    top_bottom[[model_id]] <- bind_rows(top10, bottom10)
    
    # --- ±2 SD extremes ---
    mu <- mean(ranef_df$random_intercept)
    sd_val <- sd(ranef_df$random_intercept)
    
    extreme_df <- ranef_df %>%
      mutate(label = case_when(
        random_intercept >= mu + 2 * sd_val ~ "Above_2SD",
        random_intercept <= mu - 2 * sd_val ~ "Below_2SD",
        TRUE ~ NA_character_
      )) %>%
      filter(!is.na(label))
    
    extreme_sd[[model_id]] <- extreme_df
    
    # --- Plot ---
    plot_df <- ranef_df %>%
      mutate(label = case_when(
        server_name %in% top10$server_name ~ "Top10",
        server_name %in% bottom10$server_name ~ "Bottom10",
        TRUE ~ "Average"
      ))
    
    p <- ggplot(plot_df, aes(x = reorder(server_name, random_intercept),
                             y = random_intercept, fill = label)) +
      geom_col(show.legend = FALSE) +
      coord_flip() +
      labs(
        title = paste("Random Intercepts —", group_name, yr),
        x = "Server", y = "Random Intercept"
      ) +
      scale_fill_manual(values = c("Top10" = "darkgreen", "Bottom10" = "darkred", "Average" = "grey70")) +
      theme_minimal()
    
    ggsave(filename = file.path(output_dir, paste0(model_id, "_ranef.png")),
           plot = p, width = 8, height = 10, units = "in", bg = "white")
  }
}

# --- Combine and save all results ---
all_effects_df <- bind_rows(all_effects)
top_bottom_df <- bind_rows(top_bottom)
extreme_sd_df <- bind_rows(extreme_sd)

# Save to CSV
fwrite(all_effects_df, "new model results (with df data)/player effects/wimbledon_random_intercepts_by_year.csv")
fwrite(top_bottom_df, "new model results (with df data)/player effects/wimbledon_top_bottom_by_year.csv")
fwrite(extreme_sd_df, "new model results (with df data)/player effects/wimbledon_extreme_2sd_by_year.csv")
