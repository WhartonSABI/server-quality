# --- Setup ---
rm(list = ls())
library(tidyverse)
library(data.table)

# --- Load Data ---
df <- fread("../data/processed/scaled/wimbledon_subset_m_training.csv")

# --- Identify player roles on each point ---
df <- df %>%
    filter(ServeIndicator %in% c(1, 2)) %>%
    mutate(
        ServerName = ifelse(ServeIndicator == 1, player1, player2),
        ReturnerName = ifelse(ServeIndicator == 1, player2, player1),
        is_ace = ifelse(ServeIndicator == 1, P1Ace, P2Ace),
        distance_run_server = ifelse(ServeIndicator == 1, P1DistanceRun, P2DistanceRun),
        distance_run_returner = ifelse(ServeIndicator == 1, P2DistanceRun, P1DistanceRun),
        location_bin = paste0("W", ServeWidth, "_D", ServeDepth),
        won_return_point = ifelse(PointWinner != ServeIndicator, 1, 0)
    )

# --- Helper function: entropy ---
compute_entropy <- function(x) {
    p <- prop.table(table(x))
    -sum(p * log2(p))
}

# --- Compute location entropy per server ---
location_entropy_df <- df %>%
    group_by(ServerName) %>%
    summarise(
        serve_location_entropy = compute_entropy(location_bin),
        .groups = 'drop'
    )

# --- Compute avg first and second serve speeds ---
speed_by_serve_type <- df %>%
    filter(ServeNumber %in% c(1, 2)) %>%
    group_by(ServerName, ServeNumber) %>%
    summarise(
        avg_speed = mean(Speed_MPH, na.rm = TRUE),
        .groups = 'drop'
    ) %>%
    pivot_wider(
        names_from = ServeNumber,
        values_from = avg_speed,
        names_prefix = "serve_avg_"
    ) %>%
    rename(
        serve_avg_first_speed = serve_avg_1,
        serve_avg_second_speed = serve_avg_2
    )

# --- Compute average speed_ratio using only second serves ---
speed_ratio_df <- df %>%
    filter(ServeNumber == 2) %>%
    group_by(ServerName) %>%
    summarise(
        serve_avg_speed_ratio_second = mean(speed_ratio, na.rm = TRUE),
        .groups = 'drop'
    )

# --- Aggregate general serve stats (as server) ---
server_stats <- df %>%
    group_by(ServerName) %>%
    summarise(
        n_service_points = n(),
        win_rate_serve = mean(PointWinner == ServeIndicator, na.rm = TRUE),
        ace_rate = mean(is_ace, na.rm = TRUE),
        avg_distance_run_server = mean(distance_run_server, na.rm = TRUE),
        .groups = 'drop'
    ) %>%
    left_join(location_entropy_df, by = "ServerName") %>%
    left_join(speed_by_serve_type, by = "ServerName") %>%
    left_join(speed_ratio_df, by = "ServerName")

# --- Aggregate return stats (as returner) ---
returner_stats <- df %>%
    group_by(ReturnerName) %>%
    summarise(
        n_return_points = n(),
        win_rate_return = mean(won_return_point, na.rm = TRUE),
        return_ace_rate_against = mean(is_ace, na.rm = TRUE),
        avg_distance_run_returner = mean(distance_run_returner, na.rm = TRUE),
        .groups = 'drop'
    )

# --- Rename for clarity before joining ---
server_stats <- server_stats %>%
    rename_with(~ paste0("serve_", .), -ServerName)

returner_stats <- returner_stats %>%
    rename_with(~ paste0("return_", .), -ReturnerName)

# --- Join both profiles ---
player_profiles_general <- full_join(
    server_stats, returner_stats,
    by = c("ServerName" = "ReturnerName")
) %>%
    rename(PlayerName = ServerName)

# --- Filter out players with low sample sizes (optional) ---
player_profiles_general <- player_profiles_general %>%
    filter(serve_n_service_points > 20, return_n_return_points > 20)

names(player_profiles_general)

# --- Clustering General Player Profiles ---

# 1. Exclude ID and count columns
features_for_clustering <- player_profiles_general %>%
    select(-PlayerName, -serve_n_service_points, -return_n_return_points) %>%
    drop_na()

# 2. Standardize (mean 0, sd 1)
scaled_features <- scale(features_for_clustering)

# 3. Elbow Method to choose k
set.seed(123)
wss <- map_dbl(1:10, ~ kmeans(scaled_features, centers = .x, nstart = 20)$tot.withinss)

plot(1:10, wss, type = "b", pch = 19, frame = FALSE,
     xlab = "Number of Clusters", ylab = "Total Within-Cluster Sum of Squares",
     main = "Elbow Method for General Player Profiles")

# 4. i guess 4 clusters?
k_result <- kmeans(scaled_features, centers = 4, nstart = 25)

# 5. Add cluster labels back to player profile
player_profiles_clustered <- player_profiles_general %>%
    drop_na() %>%
    mutate(cluster = as.factor(k_result$cluster))

# 6. PCA for visualization
pca_res <- prcomp(scaled_features)
pca_df <- as.data.frame(pca_res$x[, 1:2]) %>%
    mutate(
        cluster = player_profiles_clustered$cluster,
        PlayerName = player_profiles_clustered$PlayerName
    )

ggplot(pca_df, aes(x = PC1, y = PC2, color = cluster, label = PlayerName)) +
    geom_point(size = 3, alpha = 0.7) +
    geom_text(size = 2, vjust = 1.5) +
    theme_minimal() +
    labs(title = "General Player Profiles Clustering (PCA Projection)")

# 7. Save output
ggsave("../data/results/clustering/wimbledon_m_training_general_profiles_pca.png", width = 10, height = 8, dpi = 300, bg = "white")

cluster_summary <- player_profiles_clustered %>%
    group_by(cluster) %>%
    summarise(across(where(is.numeric), mean, na.rm = TRUE))

write.csv(cluster_summary, "../data/results/clustering/wimbledon_m_training_general_profiles_summary.csv", row.names = FALSE)
