# --- Setup ---
rm(list = ls())
library(tidyverse)
library(data.table)
library(cluster)
library(factoextra)
library(recipes)
library(pheatmap)
library(lme4)
# install.packages("webshot2")
library(kableExtra)
library(webshot2)
library(htmltools)

# --- Config ---
tournament <- "usopen"  # "wimbledon" or "usopen"
gender <- "m"              # "m" or "f"
tag_prefix <- paste0(tournament, "_", ifelse(gender == "m", "males", "females"))

# --- Paths ---
training_path <- file.path("../data/processed/scaled", paste0(tournament, "_", gender, "_training.csv"))
testing_path <- file.path("../data/processed/scaled", paste0(tournament, "_", gender, "_testing.csv"))

output_dir <- file.path("../data/results", tag_prefix)
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
        server_won = as.integer(ifelse(ServeIndicator == 1, PointWinner == 1, PointWinner == 2)),
        is_efficient = as.integer(server_won & (RallyCount <= 3))
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
  select(server_won, is_efficient, ServerName, avg_speed_z, sd_speed_z, location_entropy_z, modal_location)

m2_df <- df_clean %>%
  filter(ServeNumber == 2) %>%
  inner_join(serve2_profiles_z %>% select(ServerName, avg_speed_z, sd_speed_z, location_entropy_z, modal_location),
             by = "ServerName") %>%
  select(server_won, is_efficient, ServerName, avg_speed_z, sd_speed_z, location_entropy_z, modal_location)

############################
### Fit GLMMS, random intercept by server name
############################

# First serves
m1 <- glmer(
  is_efficient ~ avg_speed_z + sd_speed_z + location_entropy_z + modal_location + (1 | ServerName),
  data = m1_df,
  family = binomial()
)

# Second serves
m2 <- glmer(
  is_efficient ~ avg_speed_z + sd_speed_z + location_entropy_z + modal_location + (1 | ServerName),
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

# arrange by decreasing SQS_prob_combined
sqs_combined <- sqs_combined %>%
  arrange(desc(SQS_prob_combined))

# make servername capitalized
sqs_combined <- sqs_combined %>%
  mutate(ServerName = str_to_title(ServerName))

path <- file.path(output_dir, paste0(tag_prefix, "_metrics.csv"))
write.csv(sqs_combined, path)

# save top 25 servers as image for display
names(sqs_combined)
sqs_combined <- sqs_combined %>% 
  drop_na()
top_n <- 25
top_sqs <- sqs_combined %>%
  slice_head(n = top_n) %>%
  select(ServerName, SQS_prob_combined) %>% 
  drop_na()

# lowercase names for matching
top_sqs <- top_sqs %>%
  mutate(server_lower = tolower(ServerName))

serve1_profiles <- serve1_profiles %>%
  mutate(server_lower = tolower(ServerName))

serve2_profiles <- serve2_profiles %>%
  mutate(server_lower = tolower(ServerName))

# include avg speed, entropy, modal location for first and second serves by lowercase servername
names(serve1_profiles)

top_sqs <- top_sqs %>%
  left_join(serve1_profiles %>%
              select(server_lower, avg_speed_1st = avg_speed,
                     sd_speed_1st = sd_speed,
                     location_entropy_1st = location_entropy,
                     modal_location_1st = modal_location,
                     n_serves_1 = n_serves),
            by = "server_lower") %>%
  left_join(serve2_profiles %>%
              select(server_lower, avg_speed_2nd = avg_speed,
                     sd_speed_2nd = sd_speed,
                     location_entropy_2nd = location_entropy,
                     modal_location_2nd = modal_location,
                     n_serves_2 = n_serves),
            by = "server_lower") %>%
  mutate(
    avg_speed_1st = round(avg_speed_1st, 2),
    location_entropy_1st = round(location_entropy_1st, 2),
    avg_speed_2nd = round(avg_speed_2nd, 2),
    location_entropy_2nd = round(location_entropy_2nd, 2),
    sd_speed_1st = round(sd_speed_1st, 2),
    sd_speed_2nd = round(sd_speed_2nd, 2)
  )

# convert modal locations to character (body, body/center, body/wide, center, wide; and close to line/not close to line)
decode_width <- function(code) {
  if (is.na(code)) return(NA_character_)
  
  # part before "_", e.g. "WC", "WBW", "WBC"
  prefix <- strsplit(code, "_", fixed = TRUE)[[1]][1]
  # ignore first "W"
  width_code <- substring(prefix, 2)
  
  dplyr::case_when(
    width_code == "B"  ~ "Body",
    width_code == "BC" ~ "Body/center",
    width_code == "BW" ~ "Body/wide",
    width_code == "C"  ~ "Center",
    width_code == "W"  ~ "Wide",
    TRUE ~ NA_character_
  )
}

decode_depth <- function(code) {
  if (is.na(code)) return(NA_character_)
  
  # part after "_", e.g. "DNCTL", "DCTL"
  suffix <- strsplit(code, "_", fixed = TRUE)[[1]][2]
  # ignore first letter "D"
  depth_code <- substring(suffix, 2)
  
  dplyr::case_when(
    depth_code == "NCTL" ~ "Not close to line",
    depth_code == "CTL"  ~ "Close to line",
    TRUE ~ NA_character_
  )
}

decode_modal_location <- function(code) {
  if (is.na(code)) return(NA_character_)
  
  width <- decode_width(code)
  depth <- decode_depth(code)
  
  paste0(width, ", ", tolower(depth))
}

top_sqs <- top_sqs %>%
  mutate(
    modal_location_1st = sapply(modal_location_1st, decode_modal_location),
    modal_location_2nd = sapply(modal_location_2nd, decode_modal_location)
  ) %>% 
  select(-server_lower)

names(top_sqs)

# HTML table (temporary)
html_file <- file.path(output_dir, paste0(tag_prefix, "_top_", top_n, "_servers.html"))

top_sqs %>%
  kbl(format = "html", col.names = c("Server", "Serve Quality Score (Probability Scale)",
                                     "Avg Speed (1st)", "SD Speed (1st)", "Location Entropy (1st)", "Modal Location (1st)",
                                     "Number of 1st Serves", "Avg Speed (2nd)", "SD Speed (2nd)", "Location Entropy (2nd)",
                                     "Modal Location (2nd)", "Number of 2nd Serves")) %>%
  kable_classic(full_width = FALSE, html_font = "Calibri") %>%
  save_kable(html_file)

# screenshot HTML to PNG
png_file <- file.path(output_dir, paste0(tag_prefix, "_top_", top_n, "_servers.png"))
webshot(html_file, png_file, vwidth = 1000, vheight = 1200, zoom = 2)

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
  filter(n_serves_test > 20) %>%
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
sd(eval_df$obs_eff) # 0.08016931

summary(eval_df$obs_win)
sd(eval_df$obs_win) # 0.06604993

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
    y = "Serve Efficiency (Prop. of Serve Points Won w/ Rally ≤ 3)"
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
       p_eff, width = 7, height = 5, dpi = 300, bg = "white")
ggsave(file.path(output_dir, paste0(tag_prefix, "_SQS_vs_winpct.png")),
       p_win, width = 7, height = 5, dpi = 300, bg = "white")

############################
### compare to baselines' out of sample testing
############################

# welo
welo_baseline <- df_test_clean %>%
  mutate(
    welo_value = ifelse(ServeIndicator == 1, player1_avg_welo, player2_avg_welo)
  ) %>%
  group_by(ServerName) %>%
  summarise(
    welo_mean_test = mean(welo_value, na.rm = TRUE),
    .groups = "drop"
  )

# merge baseline (welo) with test outcomes
baseline_eval <- test_outcomes %>%
  left_join(welo_baseline,  by = "ServerName") %>%
  left_join(sqs_combined %>% select(ServerName, SQS_prob_combined), by = "ServerName") %>% 
  filter(n_serves_test > 20)

# --- Z-score standardization helper ---
zscore <- function(x) {
  if (is.numeric(x)) (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE) else x
}

# --- Standardize everything (within baseline_eval) ---
baseline_eval_std <- baseline_eval %>%
  mutate(
    SQS_prob_combined_z  = zscore(SQS_prob_combined),
    welo_mean_test_z     = zscore(welo_mean_test),
    serve_efficiency_z   = zscore(serve_efficiency_test),
    win_pct_z            = zscore(win_pct_test)
  )

# --- Metric helpers ---
rmse_fun <- function(pred, obs) sqrt(mean((pred - obs)^2, na.rm = TRUE))
corr_stats <- function(pred, obs, name_pred, name_outcome) {
  keep <- is.finite(pred) & is.finite(obs)
  if (sum(keep) < 3) return(tibble(
    predictor = name_pred, outcome = name_outcome,
    n = sum(keep), rmse = NA_real_, cor = NA_real_, p_value = NA_real_
  ))
  ct <- suppressWarnings(cor.test(pred[keep], obs[keep], method = "pearson"))
  tibble(
    predictor = name_pred, outcome = name_outcome, n = sum(keep),
    rmse = rmse_fun(pred[keep], obs[keep]),
    cor  = unname(ct$estimate), p_value = ct$p.value
  )
}

# --- Evaluate each predictor vs standardized outcomes ---
metrics_list <- list(
  corr_stats(baseline_eval_std$SQS_prob_combined_z, baseline_eval_std$serve_efficiency_z, "SQS_prob", "serve_efficiency"),
  corr_stats(baseline_eval_std$SQS_prob_combined_z, baseline_eval_std$win_pct_z,          "SQS_prob", "win_pct"),
  corr_stats(baseline_eval_std$welo_mean_test_z,    baseline_eval_std$serve_efficiency_z, "welo", "serve_efficiency"),
  corr_stats(baseline_eval_std$welo_mean_test_z,    baseline_eval_std$win_pct_z,          "welo", "win_pct")
)

metrics_baselines_std <- bind_rows(metrics_list)

baseline_path <- file.path(output_dir, paste0(tag_prefix, "_baseline_comparison.csv"))
write_csv(metrics_baselines_std, baseline_path)

#############################
### test server quality scores separately (first and second serves), compare to speed and welo
#############################

# --- Helper to evaluate one serve type ---
eval_by_serve_type <- function(serve_num,
                               sqs_tbl,                # sqs_first or sqs_second (from training)
                               tag_label,              # "first" or "second"
                               out_dir = output_dir) {
  
  # Predictions from training metric (probability scale for interpretability)
  preds <- sqs_tbl %>%
    transmute(ServerName,
              SQS_prob = plogis(SQS_logodds))   # convert that type’s SQS to prob
  
  # Test outcomes and baselines for this serve type
  test_type <- df_test_clean %>%
    filter(ServeNumber == serve_num) %>%
    mutate(
      # is_ace     = ifelse(ServeIndicator == 1, P1Ace, P2Ace),  # not used as baseline here, but kept if needed
      welo_value = ifelse(ServeIndicator == 1, player1_avg_welo, player2_avg_welo)
    ) %>%
    group_by(ServerName) %>%
    summarise(
      n_serves_test_type   = n(),
      wins_total_type      = sum(server_won, na.rm = TRUE),
      wins_rally_le3_type  = sum(server_won * (RallyCount <= 3), na.rm = TRUE),
      
      # Outcomes (per serve type)
      win_pct_type         = wins_total_type / n_serves_test_type,
      serve_eff_type       = wins_rally_le3_type / n_serves_test_type,
      
      # Baselines (per serve type)
      avg_speed_type       = mean(Speed_MPH, na.rm = TRUE),
      welo_mean_type       = mean(welo_value, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    filter(n_serves_test_type > 20)
  
  # Merge preds with test outcomes & baselines
  eval_df <- preds %>%
    inner_join(test_type, by = "ServerName")
  
  # Standardize everything before metrics
  zscore <- function(x) if (is.numeric(x)) (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE) else x
  eval_std <- eval_df %>%
    mutate(
      SQS_prob_z      = zscore(SQS_prob),
      speed_z         = zscore(avg_speed_type),
      welo_z          = zscore(welo_mean_type),
      eff_z           = zscore(serve_eff_type),
      win_z           = zscore(win_pct_type)
    )
  
  # Metrics
  rmse_fun <- function(pred, obs) sqrt(mean((pred - obs)^2, na.rm = TRUE))
  corr_stats <- function(pred, obs, name_pred, name_outcome) {
    keep <- is.finite(pred) & is.finite(obs)
    if (sum(keep) < 3) {
      return(tibble(predictor = name_pred, outcome = name_outcome,
                    n = sum(keep), rmse = NA_real_, cor = NA_real_, p_value = NA_real_))
    }
    ct <- suppressWarnings(cor.test(pred[keep], obs[keep], method = "pearson"))
    tibble(
      predictor = name_pred, outcome = name_outcome, n = sum(keep),
      rmse = rmse_fun(pred[keep], obs[keep]),
      cor  = unname(ct$estimate), p_value = ct$p.value
    )
  }
  
  metrics <- bind_rows(
    # vs serve efficiency
    corr_stats(eval_std$SQS_prob_z, eval_std$eff_z,  paste0("SQS_", tag_label), "serve_efficiency"),
    corr_stats(eval_std$speed_z,    eval_std$eff_z,  paste0("avg_speed_", tag_label), "serve_efficiency"),
    corr_stats(eval_std$welo_z,     eval_std$eff_z,  "welo", "serve_efficiency"),
    
    # vs win %
    corr_stats(eval_std$SQS_prob_z, eval_std$win_z,  paste0("SQS_", tag_label), "win_pct"),
    corr_stats(eval_std$speed_z,    eval_std$win_z,  paste0("avg_speed_", tag_label), "win_pct"),
    corr_stats(eval_std$welo_z,     eval_std$win_z,  "welo", "win_pct")
  ) %>%
    mutate(serve_type = tag_label)
  
  # Save
  out_path <- file.path(out_dir, paste0(tag_prefix, "_oos_metrics_", tag_label, "_serve.csv"))
  write_csv(metrics, out_path)
}

# --- Run for first and second serves ---
metrics_first  <- eval_by_serve_type(serve_num = 1, sqs_tbl = sqs_first,  tag_label = "first")
metrics_first
metrics_second <- eval_by_serve_type(serve_num = 2, sqs_tbl = sqs_second, tag_label = "second")
metrics_second

