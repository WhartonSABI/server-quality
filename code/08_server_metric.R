#################################
library(tidyverse)
library(cluster)
library(factoextra)
library(ggdendro)
library(DescTools)
#################################
#switch which dataset i'm looking at for when i run the full code
df = read_csv("wimbledon_subset_2024_f.csv")

################################
#first try! ts is cooked pls ignore
entropy <- function(x) {
  p <- prop.table(table(x))
  -sum(p * log2(p))
}

ModeChar <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}
################################
#first clustering/analysis
df <- df %>%
  mutate(server = if_else(ServeIndicator == 1, player1, player2),
         ace = if_else((ServeIndicator == 1 & P1Ace == 1) | 
                         (ServeIndicator == 2 & P2Ace == 1), 1, 0))

df_clean <- df %>%
  filter(!is.na(Speed_MPH), !is.na(ServeWidth)) %>% 
  mutate(ServeWidth = as.character(ServeWidth))

server_summary <- df_clean %>%
  group_by(server, ServeNumber) %>%
  summarise(
    mean_speed = mean(Speed_MPH),
    sd_speed = sd(Speed_MPH),
    mode_location = ModeChar(ServeWidth),
    entropy_location = entropy(ServeWidth),
    ace_pct = mean(ace),
    win_pct = mean(serving_player_won),
    .groups = "drop"
  )

server_summary <- server_summary %>%
  mutate(mode_loc_num = as.numeric(as.factor(mode_location))) %>%
  select(-mode_location)

features <- c("mean_speed", "sd_speed", "mode_loc_num", "entropy_location", "ace_pct", "win_pct")

df_first  <- server_summary %>% filter(ServeNumber == 1) %>% column_to_rownames("server")
df_second <- server_summary %>% filter(ServeNumber == 2) %>% column_to_rownames("server")

scaled_first  <- scale(df_first[, features]) %>% as.data.frame()
scaled_second <- scale(df_second[, features]) %>% as.data.frame()

d_first  <- dist(scaled_first)
hc_first <- hclust(d_first, method = "ward.D2")
fviz_dend(hc_first, k = 3, rect = TRUE, main = "1st Serve - Hierarchical Clustering")
scaled_first$cluster_hc <- cutree(hc_first, k = 6)

set.seed(42)
kmeans_first <- kmeans(scaled_first[, features], centers = 6, nstart = 25)
scaled_first$cluster_kmeans <- as.factor(kmeans_first$cluster)

fviz_cluster(kmeans_first, data = scaled_first[, features], geom = "point", 
             main = "K-Means (1st Serve)")


scaled_first %>%
  rownames_to_column("server") %>%
  group_by(cluster_kmeans) %>%
  summarise(across(all_of(features), mean, .names = "avg_{.col}"),
            n = n())




################################
#what i tried pt 2 which obv isnt good enough lol
df <- df %>%
  mutate(server = if_else(ServeIndicator == 1, player1, player2),
         ace = if_else((ServeIndicator == 1 & P1Ace == 1) | 
                         (ServeIndicator == 2 & P2Ace == 1), 1, 0),
         in_serve = if_else(ServeDepth != "Fault", 1, 0))

df_clean <- df %>%
  filter(!is.na(Speed_MPH), !is.na(ServeWidth)) %>% 
  mutate(ServeWidth = as.character(ServeWidth))

# --- Summarise per server and serve number ---
server_summary <- df_clean %>%
  group_by(server, ServeNumber) %>%
  summarise(
    mean_speed        = mean(Speed_MPH),
    sd_speed          = sd(Speed_MPH),
    mode_location     = ModeChar(ServeWidth),
    entropy_location  = entropy(ServeWidth),
    ace_pct           = mean(ace),
    win_pct           = mean(serving_player_won),
    in_pct            = mean(in_serve),
    .groups = "drop"
  )

# --- Convert mode location to numeric, select features ---
server_summary <- server_summary %>%
  mutate(mode_loc_num = as.numeric(as.factor(mode_location))) %>%
  select(-mode_location)

features <- c("mean_speed", "sd_speed", "mode_loc_num", 
              "entropy_location", "ace_pct", "win_pct", "in_pct")

# --- Split and scale ---
df_first  <- server_summary %>% filter(ServeNumber == 1) %>% column_to_rownames("server")
df_second <- server_summary %>% filter(ServeNumber == 2) %>% column_to_rownames("server")

scaled_first  <- scale(df_first[, features]) %>% as.data.frame()
scaled_second <- scale(df_second[, features]) %>% as.data.frame()

# --- Hierarchical clustering ---
d_first  <- dist(scaled_first)
hc_first <- hclust(d_first, method = "ward.D2")
fviz_dend(hc_first, k = 6, rect = TRUE, main = "1st Serve - Hierarchical Clustering")
scaled_first$cluster_hc <- cutree(hc_first, k = 8)

# --- K-means clustering ---
scaled_first_clean <- scaled_first %>%
  filter(if_all(all_of(features), is.finite))
set.seed(42)
kmeans_first <- kmeans(scaled_first[, features], centers = 6, nstart = 25)
scaled_first$cluster_kmeans <- as.factor(kmeans_first$cluster)

fviz_cluster(kmeans_first, data = scaled_first[, features], geom = "point", 
             main = "K-Means (1st Serve)")

# --- Cluster summary ---
scaled_first %>%
  rownames_to_column("server") %>%
  group_by(cluster_kmeans) %>%
  summarise(across(all_of(features), mean, .names = "avg_{.col}"),
            n = n())





################################

library(tidyverse)
library(data.table)
library(cluster)
library(factoextra)
library(recipes)
library(pheatmap)

################################
#US Open - same code, new data
df = read_csv("usopen_subset_2024_m.csv")

df_clean <- df %>%
  filter(!is.na(ServeWidth), !is.na(ServeDepth),
         ServeWidth != "", ServeDepth != "") %>%
  filter(ServeNumber %in% c(1, 2)) %>%
  mutate(
    location_bin = paste0("W", ServeWidth, "_D", ServeDepth),
    ServerName   = if_else(ServeIndicator == 1, player1, player2),
    is_ace       = if_else(ServeIndicator == 1, P1Ace, P2Ace),
    is_df        = if_else(ServeIndicator == 1, P1DoubleFault, P2DoubleFault)
  )

# --- 2. Helper functions ---
compute_entropy <- function(x) {
  p <- prop.table(table(x))
  -sum(p * log2(p))
}
get_mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

# --- 3. (Optional) Modal-location sanity checks ---
df_clean %>% 
  filter(ServeNumber == 1) %>% 
  group_by(ServerName) %>% 
  summarise(modal_location = get_mode(location_bin), .groups = "drop") %>% 
  pull(modal_location) %>% unique()

df_clean %>% 
  filter(ServeNumber == 2) %>% 
  group_by(ServerName) %>% 
  summarise(modal_location = get_mode(location_bin), .groups = "drop") %>% 
  pull(modal_location) %>% unique()

# --- 4. Build per-player serve profiles (now with in_pct) ---
get_serve_profiles <- function(df, serve_number_label) {
  df %>%
    filter(ServeNumber == serve_number_label) %>%
    group_by(ServerName) %>%
    summarise(
      avg_speed        = mean(Speed_MPH, na.rm = TRUE),
      sd_speed         = sd(Speed_MPH, na.rm = TRUE),
      cv_speed         = sd_speed / avg_speed,           # coefficient of variation
      win_rate         = mean(PointWinner == ServeIndicator, na.rm = TRUE),
      ace_pct          = mean(is_ace, na.rm = TRUE),
      df_pct           = mean(is_df,  na.rm = TRUE),      # double-fault percentage
      location_entropy = compute_entropy(location_bin),
      modal_location   = get_mode(location_bin),
      n_serves         = n(),
      .groups          = "drop"
    ) %>%
    filter(n_serves > 20)
}

serve_profiles1 <- get_serve_profiles(df_clean, 1)
serve_profiles2 <- get_serve_profiles(df_clean, 2)

# --- 5. Clustering + Elbow + Summaries (unchanged) ---
run_clustering_models <- function(profiles_df, tag) {
  library(recipes)
  library(tidyverse)
  
  # 1) One‑hot encode modal_location
  recipe_obj <- recipe(~ ., data = profiles_df) %>%
    update_role(ServerName, new_role = "id") %>%
    update_role(n_serves,   new_role = "id") %>%
    step_dummy(all_nominal_predictors()) %>%
    prep()
  
  df_encoded <- bake(recipe_obj, new_data = NULL)
  
  # 2) Drop the ID column and any rows with missing predictors
  df_clustering <- df_encoded %>%
    column_to_rownames("ServerName") %>%
    select(-n_serves) %>%
    filter(complete.cases(.))
  
  # 3) Scale everything and down‑weight the modal_location dummies
  df_scaled <- scale(df_clustering)
  modal_cols <- grep("^modal_location_", colnames(df_scaled), value = TRUE)
  df_scaled[, modal_cols] <- df_scaled[, modal_cols] * 0.6
  df_scaled[!is.finite(df_scaled)] <- 0
  # 4) How many distinct rows do we have?
  n_rows   <- nrow(df_scaled)
  n_unique <- nrow(unique(as.data.frame(df_scaled)))
  message(sprintf("[%s] rows = %d; distinct = %d", tag, n_rows, n_unique))
  if (n_unique < 2) {
    message("Not enough distinct points to cluster; returning NULL.")
    return(NULL)
  }
  
  # 5) Elbow plot: only up to n_unique - 1 clusters
  max_k <- min(10, n_unique - 1)
  wss   <- map_dbl(1:max_k, ~ kmeans(df_scaled, centers = .x, nstart = 20)$tot.withinss)
  elbow_df <- tibble(k = 1:max_k, wss = wss)
  p_elbow <- ggplot(elbow_df, aes(k, wss)) +
    geom_line() + geom_point() + theme_minimal() +
    labs(title = paste("Elbow Plot –", tag),
         x = "Number of Clusters",
         y = "Total Within-Cluster SS")
  print(p_elbow)
  
  # 6) Final K-means: never more than n_unique
  k_final <- min(3, n_unique)
  set.seed(123)
  km <- kmeans(df_scaled, centers = k_final, nstart = 25)
  
  # 7) Stitch cluster labels back onto the original profiles_df
  profiles_km <- profiles_df %>%
    slice(match(rownames(df_scaled), ServerName)) %>%  # keep same order
    mutate(cluster = factor(km$cluster))
  
  # 8) Build modal-location proportions and summary table
  modal_props <- profiles_km %>%
    group_by(cluster, modal_location) %>%
    summarise(n = n(), .groups = "drop") %>%
    group_by(cluster) %>%
    mutate(prop = n / sum(n)) %>%
    pivot_wider(names_from = modal_location, values_from = prop, values_fill = 0)
  
  summary_km <- profiles_km %>%
    group_by(cluster) %>%
    summarise(across(where(is.numeric) & !matches("n_serves"), mean, na.rm = TRUE))
  
  # 9) Return everything
  list(
    scaled      = df_scaled,
    profile_data= profiles_km,
    modal_props = modal_props,
    summary     = summary_km,
    elbow_plot  = p_elbow
  )
}

res1 <- run_clustering_models(serve_profiles1, "first_serve")
res2 <- run_clustering_models(serve_profiles2, "second_serve")

# --- 6. EDA including in_pct ---
res1$profile_data %>%
  group_by(cluster) %>%
  summarise(
    avg_speed        = mean(avg_speed),
    sd_speed         = mean(sd_speed),
    ace_pct          = mean(ace_pct),
    df_pct           = mean(df_pct),
    win_rate         = mean(win_rate),
    location_entropy = mean(location_entropy),
    n                = n()
  )

res1$profile_data %>%
  pivot_longer(
    cols = c(avg_speed, sd_speed, cv_speed, ace_pct, df_pct, win_rate, location_entropy),
    names_to  = "variable",
    values_to = "value"
  ) %>%
  ggplot(aes(factor(cluster), value, fill = factor(cluster))) +
  geom_boxplot() +
  facet_wrap(~ variable, scales = "free_y") +
  theme_minimal() +
  labs(
    x = "Cluster", y = "Value", fill = "Cluster",
    title = "Feature Distributions by Cluster"
  )

#######################################################
#random forest
library(randomForest)
# fit to predict point‐win%
rf <- randomForest(
  win_rate ~ avg_speed + sd_speed + df_pct + ace_pct +
    location_entropy,
  data = res1$profile_data, importance=TRUE
)

# extract importance (IncMSE)
imp <- importance(rf, type=1)
imp_sorted <- sort(imp[,1], decreasing=TRUE)
print(imp_sorted)

# use these importances (rescaled to sum to 1) as your weights:
rf_weights <- imp_sorted / sum(imp_sorted)

# then
res1$profile_data <- res1$profile_data %>%
  mutate(serve_score = predict(rf, .)) 


####################################################
library(scales)

# 1) Fit a regression to see feature effects
model <- lm(
  win_rate ~ avg_speed + sd_speed + df_pct + ace_pct + location_entropy,
  data = res1$profile_data
)
summary(model)


# 2) Define which features to normalize & weight
features <- c(
  "avg_speed", 
  "sd_speed", 
  "ace_pct", 
  "df_pct",
  "win_rate", 
  "location_entropy"
)

# 3) Rescale each feature to [0,1]
normalized_profiles <- res1$profile_data %>%
  mutate(across(all_of(features), ~ scales::rescale(.x, to = c(0,1))))


# 4) Choose weights (must sum to 1)
weights <- c(
  avg_speed        = 0.10,   # how fast on average
  sd_speed         = 0.05,   # speed variability
  df_pct           = 0.05,   # speed consistency
  ace_pct          = 0.30,   # aggression payoff
  win_rate         = 0.20,   # overall effectiveness
  location_entropy = 0.30    # deception value
)

# 5) Compute your composite “serve_score”
server_quality <- normalized_profiles %>%
  rowwise() %>%
  mutate(
    serve_score = sum(c_across(all_of(features)) * weights[features])
  ) %>%
  ungroup() %>%
  arrange(desc(serve_score)) %>%
  select(ServerName, cluster, serve_score, all_of(features))

# 6) Inspect top servers
server_quality
