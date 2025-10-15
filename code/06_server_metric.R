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

