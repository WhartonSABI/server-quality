# --- Setup ---
rm(list = ls())
library(tidyverse)
library(data.table)
library(cluster)
library(factoextra)
library(recipes)
library(pheatmap)

# --- Load Data ---
df <- fread("../data/processed/scaled/usopen_subset_m_training.csv")

# --- Clean and prepare ---
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

# --- Create player-level serve profiles ---
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
        filter(n_serves > 50)
}

# --- Run clustering models and visualizations ---
run_clustering_models <- function(profiles_df, tag) {
    # One-hot encode modal_location
    recipe_obj <- recipe(~ ., data = profiles_df) %>%
        update_role(ServerName, new_role = "id") %>%
        update_role(n_serves, new_role = "id") %>%
        step_dummy(all_nominal_predictors()) %>%
        prep()
    
    df_encoded <- bake(recipe_obj, new_data = NULL)
    row_names <- df_encoded$ServerName
    df_clustering <- df_encoded %>%
        column_to_rownames("ServerName") %>%
        select(-n_serves)
    
    # Standardize numeric values
    df_scaled <- scale(df_clustering)
    
    # Identify and down-weight modal_location columns
    modal_cols <- grep("^modal_location_", colnames(df_scaled), value = TRUE)
    df_scaled[, modal_cols] <- df_scaled[, modal_cols] * 0.6
    
    # Elbow Plot
    wss <- map_dbl(1:10, function(k) {
        kmeans(df_scaled, centers = k, nstart = 20)$tot.withinss
    })
    elbow_df <- tibble(k = 1:10, wss = wss)
    p_elbow <- ggplot(elbow_df, aes(x = k, y = wss)) +
        geom_line() +
        geom_point() +
        theme_minimal() +
        labs(title = paste("Elbow Plot –", tag),
             x = "Number of Clusters", y = "Total Within-Cluster Sum of Squares")
    ggsave(paste0("../data/results/clustering/", tag, "_elbow_plot.png"), p_elbow, 
           width = 6, height = 4, bg = "white")
    
    # --- K-means Clustering ---
    set.seed(123)
    kmeans_res <- kmeans(df_scaled, centers = 4, nstart = 25)
    kmeans_labels <- as.factor(kmeans_res$cluster)
    
    profiles_kmeans <- profiles_df %>%
        mutate(cluster = kmeans_labels)
    
    modal_props_kmeans <- profiles_kmeans %>%
        group_by(cluster, modal_location) %>%
        summarise(n = n(), .groups = "drop") %>%
        group_by(cluster) %>%
        mutate(prop = n / sum(n)) %>%
        pivot_wider(names_from = modal_location, values_from = prop, values_fill = 0)
    write.csv(modal_props_kmeans, paste0("../data/results/clustering/", tag, "_kmeans_modal_location_props.csv"), row.names = FALSE)
    
    cluster_summary_kmeans <- profiles_kmeans %>%
        group_by(cluster) %>%
        summarise(across(where(is.numeric) & !matches("n_serves"), \(x) mean(x, na.rm = TRUE)))
    write.csv(cluster_summary_kmeans, paste0("../data/results/clustering/", tag, "_kmeans_summary.csv"), row.names = FALSE)
    
    # PCA plot
    pca_res <- prcomp(df_scaled)
    pca_df <- as.data.frame(pca_res$x[, 1:2]) %>%
        mutate(cluster = kmeans_labels, ServerName = row_names)
    
    p_pca <- ggplot(pca_df, aes(x = PC1, y = PC2, color = cluster, label = ServerName)) +
        geom_point(size = 3, alpha = 0.7) +
        geom_text(size = 2, vjust = 1.5) +
        theme_minimal() +
        labs(title = paste("K-means Clustering (PCA Projection) –", tag))
    ggsave(paste0("../data/results/clustering/", tag, "_kmeans_pca_plot.png"), p_pca, 
           width = 8, height = 6, dpi = 300, bg = "white")
    
    # PCA loadings
    pca_loadings <- as.data.frame(pca_res$rotation)
    write.csv(pca_loadings, paste0("../data/results/clustering/", tag, "_pca_loadings.csv"))
    
    # Heatmap of cluster centers
    cluster_centers_raw <- as.data.frame(kmeans_res$centers)
    pheatmap(cluster_centers_raw,
             cluster_rows = TRUE,
             cluster_cols = TRUE,
             main = paste("K-means Cluster Centers (Raw Feature Space) –", tag),
             filename = paste0("../data/results/clustering/", tag, "_kmeans_heatmap.png"),
             width = 8,
             height = 6)
    
    # --- Hierarchical Clustering (save only outputs) ---
    hc_dist <- dist(df_scaled)
    hc <- hclust(hc_dist, method = "ward.D2")
    hc_labels <- cutree(hc, k = 4)
    
    profiles_hc <- profiles_df %>%
        mutate(cluster = as.factor(hc_labels))
    
    modal_props_hc <- profiles_hc %>%
        group_by(cluster, modal_location) %>%
        summarise(n = n(), .groups = "drop") %>%
        group_by(cluster) %>%
        mutate(prop = n / sum(n)) %>%
        pivot_wider(names_from = modal_location, values_from = prop, values_fill = 0)
    write.csv(modal_props_hc, paste0("../data/results/clustering/", tag, "_hierarchical_modal_location_props.csv"), row.names = FALSE)
    
    cluster_summary_hc <- profiles_hc %>%
        group_by(cluster) %>%
        summarise(across(where(is.numeric) & !matches("n_serves"), \(x) mean(x, na.rm = TRUE)))
    write.csv(cluster_summary_hc, paste0("../data/results/clustering/", tag, "_hierarchical_summary.csv"), row.names = FALSE)
    
    png(paste0("../data/results/clustering/", tag, "_hierarchical_dendrogram.png"), width = 1000, height = 700)
    plot(hc, labels = row_names, main = paste("Hierarchical Clustering –", tag))
    rect.hclust(hc, k = 4, border = 2:5)
    dev.off()
    
    return(profiles_kmeans)
}

# --- Run clustering and save K-means assignments ---
first_profiles   <- get_serve_profiles(df_clean, 1)
second_profiles  <- get_serve_profiles(df_clean, 2)
combined_profiles <- get_serve_profiles(df_clean, c(1, 2))

first_profiles_kmeans <- run_clustering_models(first_profiles, "first_serves")
second_profiles_kmeans <- run_clustering_models(second_profiles, "second_serves")
combined_profiles_kmeans <- run_clustering_models(combined_profiles, "combined_serves")

write.csv(first_profiles_kmeans, "../data/results/clustering/first_serves_kmeans_cluster_assignments.csv", row.names = FALSE)
write.csv(second_profiles_kmeans, "../data/results/clustering/second_serves_kmeans_cluster_assignments.csv", row.names = FALSE)
write.csv(combined_profiles_kmeans, "../data/results/clustering/combined_serves_kmeans_cluster_assignments.csv", row.names = FALSE)
