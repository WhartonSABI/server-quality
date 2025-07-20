# --- Setup ---
rm(list = ls())
library(tidyverse)
library(data.table)
library(ggplot2)
library(cluster)
library(ggpubr)

# --- Load Data ---
df <- fread("../data/processed/scaled/wimbledon_subset_m_training.csv")  # Replace with correct path

# --- Step 1: Filter & Clean ---
df_clean <- df %>%
  filter(!is.na(ServeWidth), !is.na(ServeDepth), ServeWidth != "", ServeDepth != "") %>%
  filter(ServeNumber %in% c(1, 2))  # Only in-play serves

# --- Step 2: Feature Engineering ---
# For entropy, combine location into a string bin
df_clean <- df_clean %>%
  mutate(location_bin = paste0("W", ServeWidth, "_D", ServeDepth),
         ServerName = ifelse(ServeIndicator == 1, player1, player2)) 

# Function to compute entropy
compute_entropy <- function(x) {
  p <- prop.table(table(x))
  -sum(p * log2(p))
}

# Aggregate player-level features
player_profiles <- df_clean %>%
  group_by(ServerName) %>%
  summarise(
    avg_speed = mean(Speed_MPH, na.rm = TRUE),
    win_rate = mean(PointWinner == ServeIndicator, na.rm = TRUE),
    location_entropy = compute_entropy(location_bin),
    n_serves = n(),
    .groups = 'drop'
  ) %>%
  filter(n_serves > 100)  # Only include players with enough data

# --- Step 3: Clustering ---
features_for_clustering <- player_profiles %>%
  select(avg_speed, location_entropy, win_rate) %>%
  scale()

set.seed(123)
k_clusters <- 3
clustering_result <- kmeans(features_for_clustering, centers = k_clusters)

player_profiles$cluster <- factor(clustering_result$cluster)

# --- Step 4: Visualization ---
# PCA to visualize clusters
pca_res <- prcomp(features_for_clustering)
player_profiles <- player_profiles %>%
  mutate(PC1 = pca_res$x[, 1], PC2 = pca_res$x[, 2])

ggplot(player_profiles, aes(x = PC1, y = PC2, color = cluster)) +
  geom_point(size = 3, alpha = 0.8) +
  labs(title = "Server Clusters (PCA)", x = "PC1", y = "PC2") +
  theme_minimal()

# --- Optional: Summary Plot ---
ggplot(player_profiles, aes(x = cluster, y = location_entropy, fill = cluster)) +
  geom_boxplot() +
  labs(title = "Entropy by Cluster", x = "Cluster", y = "Location Entropy") +
  theme_minimal()

