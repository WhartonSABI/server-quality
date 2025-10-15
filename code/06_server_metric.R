# --- Setup ---
rm(list = ls())
library(tidyverse)
library(data.table)
library(cluster)
library(factoextra)
library(recipes)
library(pheatmap)
library(lme4)

# --- Config ---
tournament <- "wimbledon"  # "wimbledon" or "usopen"
gender <- "m"              # "m" or "f"
tag_prefix <- paste0(tournament, "_", ifelse(gender == "m", "males", "females"))

# --- Paths ---
training_path <- file.path("../data/processed/scaled", paste0(tournament, "_", gender, "_training.csv"))
testing_path <- file.path("../data/processed/scaled", paste0(tournament, "_", gender, "_testing.csv"))

output_dir <- file.path("../data/results/", tag_prefix)
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

# --- Load Data ---
df <- fread(training_path)

# --- Clean and prepare ---
df_clean <- df %>%
    filter(!is.na(ServeWidth), !is.na(ServeDepth), ServeWidth != "", ServeDepth != "") %>%
    filter(ServeNumber %in% c(1, 2)) %>%
    mutate(
        location_bin = paste0("W", ServeWidth, "_D", ServeDepth),
        ServerName = tolower(ifelse(ServeIndicator == 1, player1, player2)),
        is_ace = ifelse(ServeIndicator == 1, P1Ace, P2Ace),
        is_df = ifelse(ServeIndicator == 1, P1DoubleFault, P2DoubleFault),
        server_won = as.integer(ifelse(ServeIndicator == 1, PointWinner == 1, PointWinner == 2))
    )

# --- Helper functions ---
compute_entropy <- function(x) {
    p <- prop.table(table(x))
    -sum(p * log2(p))
}
get_mode <- function(x) {
    ux <- unique(x)
    ux[which.max(tabulate(match(x, ux)))]
}

get_serve_profiles <- function(df, serve_number_label) {
    df %>%
        filter(ServeNumber %in% serve_number_label) %>%
        group_by(ServerName) %>%
        summarise(
            avg_speed = mean(Speed_MPH, na.rm = TRUE),
            sd_speed = sd(Speed_MPH, na.rm = TRUE),
            ace_pct = mean(is_ace, na.rm = TRUE),
            location_entropy = compute_entropy(location_bin),
            modal_location = get_mode(location_bin),
            n_serves = n(),
            .groups = 'drop'
        ) %>%
        filter(n_serves > 20)
}

# --- get serve profiles ---
serve1_profiles <- get_serve_profiles(df_clean, serve_number_label = 1)
serve2_profiles <- get_serve_profiles(df_clean, serve_number_label = 2)

# standardize server profiles (features)
scale_cols <- function(d) {
  num_cols <- c("avg_speed", "sd_speed", "location_entropy")
  mu  <- sapply(d[num_cols], mean, na.rm = TRUE)
  sig <- sapply(d[num_cols], sd,   na.rm = TRUE)
  d %>%
    mutate(
      avg_speed_z        = (avg_speed        - mu["avg_speed"])        / sig["avg_speed"],
      sd_speed_z         = (sd_speed         - mu["sd_speed"])         / sig["sd_speed"],
      location_entropy_z = (location_entropy - mu["location_entropy"]) / sig["location_entropy"],
      modal_location     = factor(modal_location) 
    )
}

serve1_profiles_z <- scale_cols(serve1_profiles)
serve2_profiles_z <- scale_cols(serve2_profiles)

# join profiles back to point-level data for modeling (fit on point-level outcomes with player-level features)
m1_df <- df_clean %>%
  filter(ServeNumber == 1) %>%
  inner_join(serve1_profiles_z %>% select(ServerName, avg_speed_z, sd_speed_z, location_entropy_z, modal_location),
             by = "ServerName") %>%
  select(server_won, ServerName, avg_speed_z, sd_speed_z, location_entropy_z, modal_location)

m2_df <- df_clean %>%
  filter(ServeNumber == 2) %>%
  inner_join(serve2_profiles_z %>% select(ServerName, avg_speed_z, sd_speed_z, location_entropy_z, modal_location),
             by = "ServerName") %>%
  select(server_won, ServerName, avg_speed_z, sd_speed_z, location_entropy_z, modal_location)

############################
### Fit GLMMS, random intercept by server name
############################

# First serves
m1 <- glmer(
  server_won ~ avg_speed_z + sd_speed_z + location_entropy_z + modal_location + (1 | ServerName),
  data = m1_df,
  family = binomial()
)

# Second serves
m2 <- glmer(
  server_won ~ avg_speed_z + sd_speed_z + location_entropy_z + modal_location + (1 | ServerName),
  data = m2_df,
  family = binomial()
)

print(summary(m1))
print(summary(m2))

############################
### Server quality metric = measured skill (sum of coefficients * features) + random intercept of player
############################

build_sqs <- function(model, profiles_z) {
  b <- fixef(model) # fixed effects from glmm
  
  # random effects of servers
  re <- ranef(model)$ServerName
  u  <- re[,"(Intercept)"]
  names(u) <- rownames(re)
  
  #  model matrix for measured skill terms only
  mm <- model.matrix(
    ~ avg_speed_z + sd_speed_z + location_entropy_z + modal_location,
    data = profiles_z
  )
  
  # Keep only columns that exist in fixef
  cols <- intersect(colnames(mm), names(b))
  mm <- mm[, cols, drop = FALSE]
  bvec <- b[cols]
  
  measured <- as.numeric(mm %*% bvec) # measured skills (beta * features)
  
  # align random intercepts to profiles
  u_vec <- u[profiles_z$ServerName]
  u_vec[is.na(u_vec)] <- 0  # if any player was dropped, default to 0
  
  tibble(
    ServerName = profiles_z$ServerName,
    SQS_logodds = measured + u_vec, # SQS = server quality score
    MeasuredSkill = measured,
    UnmeasuredCraft = u_vec
  )
}

sqs_first  <- build_sqs(m1, serve1_profiles_z)
sqs_second <- build_sqs(m2, serve2_profiles_z)

# combine server quality scores (Sqs's) by weighted average based on number of first & second serves
# --- bring serve counts onto the SQS tables ---
sqs_first_w <- sqs_first %>%
  left_join(serve1_profiles %>% select(ServerName, n_serves_1 = n_serves), by = "ServerName")

sqs_second_w <- sqs_second %>%
  left_join(serve2_profiles %>% select(ServerName, n_serves_2 = n_serves), by = "ServerName")

### combine on log odds scale
sqs_combined <- full_join(
  sqs_first_w  %>% rename(
    SQS_logodds_1st     = SQS_logodds,
    MeasuredSkill_1st   = MeasuredSkill,
    UnmeasuredCraft_1st = UnmeasuredCraft
  ),
  sqs_second_w %>% rename(
    SQS_logodds_2nd     = SQS_logodds,
    MeasuredSkill_2nd   = MeasuredSkill,
    UnmeasuredCraft_2nd = UnmeasuredCraft
  ),
  by = "ServerName"
) %>%
  mutate(
    w1 = coalesce(n_serves_1, 0),
    w2 = coalesce(n_serves_2, 0),
    
    # Weighted average (fallback to whichever exists if the other is 0)
    SQS_logodds_combined =
      dplyr::case_when(
        w1 > 0 & w2 == 0 ~ SQS_logodds_1st,
        w2 > 0 & w1 == 0 ~ SQS_logodds_2nd,
        TRUE ~ (w1 * SQS_logodds_1st + w2 * SQS_logodds_2nd) / (w1 + w2)
      )
  )

# convert to probability scale
sqs_combined <- sqs_combined %>%
  mutate(SQS_prob_combined = plogis(SQS_logodds_combined))

################################
### out of sample testing
################################

# --- Load testing data and clean identically ---
df_test <- fread(testing_path)

df_test_clean <- df_test %>%
  filter(!is.na(ServeWidth), !is.na(ServeDepth), ServeWidth != "", ServeDepth != "") %>%
  filter(ServeNumber %in% c(1, 2)) %>%
  mutate(
    ServerName  = tolower(ifelse(ServeIndicator == 1, player1, player2)),
    server_won  = as.integer(ifelse(ServeIndicator == 1, PointWinner == 1, PointWinner == 2)),
    rally_le3   = if_else(RallyCount <= 3, 1L, 0L)
  )

# --- Aggregate test outcomes per server (all serves combined) ---
test_outcomes <- df_test_clean %>%
  group_by(ServerName) %>%
  summarise(
    n_serves_test         = n(),
    wins_total            = sum(server_won, na.rm = TRUE),
    wins_rally_le3        = sum(server_won * rally_le3, na.rm = TRUE),
    win_pct_test          = wins_total / n_serves_test,
    serve_efficiency_test = wins_rally_le3 / n_serves_test,
    .groups = "drop"
  )

# --- Join training-based predictions (SQS_prob_combined) to test outcomes ---
eval_df <- sqs_combined %>%
  select(ServerName, SQS_prob_combined) %>%
  inner_join(test_outcomes, by = "ServerName") %>%
  mutate(
    pred = SQS_prob_combined,
    obs_eff = serve_efficiency_test,
    obs_win = win_pct_test
  )

# --- Metric helpers ---
rmse_fun <- function(pred, obs) {
  sqrt(mean((pred - obs)^2, na.rm = TRUE))
}

corr_row <- function(pred, obs, outcome_name) {
  # Guard: need at least 3 paired non-NA points for cor.test
  keep <- is.finite(pred) & is.finite(obs)
  if (sum(keep) < 3) {
    tibble(
      outcome    = outcome_name,
      n_players  = sum(keep),
      rmse       = NA_real_,
      cor        = NA_real_,
      p_value    = NA_real_
    )
  } else {
    ct <- suppressWarnings(cor.test(pred[keep], obs[keep], method = "pearson"))
    tibble(
      outcome    = outcome_name,
      n_players  = sum(keep),
      rmse       = rmse_fun(pred[keep], obs[keep]),
      cor        = unname(ct$estimate),
      p_value    = ct$p.value
    )
  }
}

# --- Compute metrics for both outcomes ---
metrics_eff <- corr_row(eval_df$pred, eval_df$obs_eff, "serve_efficiency")
metrics_win <- corr_row(eval_df$pred, eval_df$obs_win,  "win_percentage")

metrics_out <- bind_rows(metrics_eff, metrics_win)

summary(eval_df$obs_eff)
sd(eval_df$obs_eff) # 0.09292598

summary(eval_df$obs_win)
sd(eval_df$obs_win) # 0.08372838

# --- Save to CSV ---
metrics_path <- file.path(output_dir, paste0(tag_prefix, "_glmm_oos_metrics.csv"))
write_csv(metrics_out, metrics_path)

### scsatterplots
# --- Serve efficiency plot ---
p_eff <- ggplot(eval_df, aes(x = SQS_prob_combined, y = serve_efficiency_test)) +
  geom_point(alpha = 0.7, size = 2) +
  geom_smooth(method = "lm", se = TRUE, color = "blue", linetype = "dashed") +
  labs(
    title = "Server Quality (Training) vs Serve Efficiency (Testing)",
    x = "Predicted Server Quality (SQS_prob_combined)",
    y = "Serve Efficiency (Proportion of Serve Points Won with Rally ≤ 3)"
  ) +
  theme_minimal(base_size = 13)

# --- Win percentage plot ---
p_win <- ggplot(eval_df, aes(x = SQS_prob_combined, y = win_pct_test)) +
  geom_point(alpha = 0.7, size = 2) +
  geom_smooth(method = "lm", se = TRUE, color = "darkgreen", linetype = "dashed") +
  labs(
    title = "Server Quality (Training) vs Win Percentage (Testing)",
    x = "Predicted Server Quality (SQS_prob_combined)",
    y = "Win Percentage (Proportion of Serve Points Won)"
  ) +
  theme_minimal(base_size = 13)

# --- Save plots ---
ggsave(file.path(output_dir, paste0(tag_prefix, "_SQS_vs_efficiency.png")),
       p_eff, width = 7, height = 5, dpi = 300)
ggsave(file.path(output_dir, paste0(tag_prefix, "_SQS_vs_winpct.png")),
       p_win, width = 7, height = 5, dpi = 300)

############################
### out of sample testing using baselines
############################

