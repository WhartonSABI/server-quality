# --- Setup ---
rm(list = ls())
library(tidyverse)
library(data.table)

# --- Load and clean data ---
df <- fread("../data/processed/scaled/wimbledon_subset_m_training.csv")
names(df)

df_clean <- df %>%
    filter(!is.na(ServeWidth), !is.na(ServeDepth), ServeWidth != "", ServeDepth != "") %>%
    filter(ServeNumber %in% c(1, 2)) %>%
    mutate(
        location_bin = paste0("W", ServeWidth, "_D", ServeDepth),
        ServerName = ifelse(ServeIndicator == 1, player1, player2),
        is_ace = ifelse(ServeIndicator == 1, P1Ace, P2Ace),
        is_df = ifelse(ServeIndicator == 1, P1DoubleFault, P2DoubleFault)
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

# --- First: compute total serves (for first serve % in calculation) ---
all_serves <- df_clean %>%
    distinct(match_id, PointNumber, ServerName)

first_serve_counts <- df_clean %>%
    filter(ServeNumber == 1) %>%
    group_by(ServerName) %>%
    summarise(n_first_serves = n(), .groups = "drop")

total_serves_per_player <- all_serves %>%
    group_by(ServerName) %>%
    summarise(n_total_serves = n(), .groups = "drop")

serve_in_df <- left_join(first_serve_counts, total_serves_per_player, by = "ServerName") %>%
    mutate(pct_in_first = n_first_serves / n_total_serves)

## --- Player-level aggregation by serve type ---
player_profiles_split <- df_clean %>%
    group_by(ServerName, ServeNumber) %>%
    summarise(
        avg_speed = mean(Speed_MPH, na.rm = TRUE),
        max_speed = max(Speed_MPH, na.rm = TRUE),
        win_rate = mean(PointWinner == ServeIndicator, na.rm = TRUE),
        ace_pct = mean(is_ace, na.rm = TRUE),
        location_entropy = compute_entropy(location_bin),
        modal_location = get_mode(location_bin),
        n_serves = n(),
        .groups = 'drop'
    ) %>%
    filter(n_serves > 50) %>%
    mutate(serve_type = ifelse(ServeNumber == 1, "First Serve", "Second Serve")) %>%
    select(-ServeNumber)

# --- Split into first and second serve datasets ---
first_serve_profiles <- player_profiles_split %>%
    filter(serve_type == "First Serve") %>%
    select(-serve_type) %>%
    rename_with(~ paste0("first_", .), -ServerName)

second_serve_profiles <- player_profiles_split %>%
    filter(serve_type == "Second Serve") %>%
    select(-serve_type) %>%
    rename_with(~ paste0("second_", .), -ServerName)

# --- Merge in pct_in_first ---
first_serve_profiles <- first_serve_profiles %>%
    left_join(serve_in_df %>% select(ServerName, pct_in_first), by = "ServerName")

# --- Preview ---
head(first_serve_profiles)
head(second_serve_profiles)

#----------------------------------------------
# clustering
#----------------------------------------------

# Join first and second serve profiles
player_profiles <- left_join(first_serve_profiles, second_serve_profiles, by = "ServerName")

# Drop non-numeric columns (e.g., modal_location) and standardize numeric features
cols_to_cluster <- player_profiles %>%
    select(-ServerName, -first_modal_location, -second_modal_location, -first_n_serves, -second_n_serves) %>% 
    drop_na()

cols_scaled <- scale(cols_to_cluster)  # Standardize

wss <- map_dbl(1:10, function(k) {
    kmeans(cols_scaled, centers = k, nstart = 20)$tot.withinss
})

# Plot Elbow
plot(1:10, wss, type = "b", pch = 19, frame = FALSE,
     xlab = "Number of clusters", ylab = "Total Within-Cluster Sum of Squares",
     main = "Elbow Method for Choosing k")
# optimal is prolly 3 or 4 clusters

set.seed(123)
kmeans_result <- kmeans(cols_scaled, centers = 4, nstart = 25)

# Add cluster labels
player_profiles_complete <- player_profiles %>% 
    drop_na()
player_profiles_complete$cluster <- as.factor(kmeans_result$cluster)

# Run PCA
pca_res <- prcomp(cols_scaled)
pca_df <- as.data.frame(pca_res$x[, 1:2]) %>%
    mutate(cluster = player_profiles_complete$cluster,
           ServerName = player_profiles_complete$ServerName)

# Plot
ggplot(pca_df, aes(x = PC1, y = PC2, color = cluster, label = ServerName)) +
    geom_point(size = 3, alpha = 0.7) +
    geom_text(size = 2, vjust = 1.5) +
    theme_minimal() +
    labs(title = "Serve Profiles Clustering (PCA Projection)--Wimbledon Males, 2021-24")
ggsave("../data/results/clustering/wimbledon_m_training_kmeans_pca.png", width = 8, height = 6, dpi = 300, bg = "white")

cluster_stats <- player_profiles_complete %>%
    select(-first_n_serves, -second_n_serves) %>%
    group_by(cluster) %>%
    summarise(across(where(is.numeric), mean, na.rm = TRUE))

write.csv(cluster_stats, "../data/results/clustering/wimbledon_m_training_kmeans.csv", row.names = FALSE)

