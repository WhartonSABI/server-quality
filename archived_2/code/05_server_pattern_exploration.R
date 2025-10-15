# --- Setup ---
rm(list = ls())
library(tidyverse)
library(data.table)
library(cluster)
library(factoextra)
library(recipes)
library(pheatmap)

# --- Config ---
tournament <- "usopen"  # "wimbledon" or "usopen"
gender <- "m"              # "m" or "f"
tag_prefix <- paste0(tournament, "_", ifelse(gender == "m", "males", "females"))

# --- Paths ---
input_path <- file.path("../data/processed/scaled", paste0(tournament, "_subset_", gender, "_training.csv"))
output_dir <- file.path("../data/results/clustering", tag_prefix)
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

# --- Load Data ---
df <- fread(input_path)

# --- Clean and prepare ---
df_clean <- df %>%
    filter(!is.na(ServeWidth), !is.na(ServeDepth), ServeWidth != "", ServeDepth != "") %>%
    filter(ServeNumber %in% c(1, 2)) %>%
    mutate(
        location_bin = paste0("W", ServeWidth, "_D", ServeDepth),
        ServerName = tolower(ifelse(ServeIndicator == 1, player1, player2)),
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

run_clustering_models <- function(profiles_df, serve_tag) {
    tag <- paste0(tag_prefix, "_", serve_tag)
    
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
    
    df_scaled <- scale(df_clustering)
    modal_cols <- grep("^modal_location_", colnames(df_scaled), value = TRUE)
    df_scaled[, modal_cols] <- df_scaled[, modal_cols] * 0.6
    
    # Elbow Plot
    wss <- map_dbl(1:10, function(k) kmeans(df_scaled, centers = k, nstart = 20)$tot.withinss)
    p_elbow <- ggplot(tibble(k = 1:10, wss = wss), aes(x = k, y = wss)) +
        geom_line() + geom_point() + theme_minimal() +
        labs(title = paste("Elbow Plot –", tag), x = "Number of Clusters", y = "Total Within-Cluster SS")
    ggsave(file.path(output_dir, paste0(tag, "_elbow_plot.png")), p_elbow, width = 6, height = 4, bg = "white")
    
    # K-means Clustering
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
    write.csv(modal_props_kmeans, file.path(output_dir, paste0(tag, "_kmeans_modal_location_props.csv")), row.names = FALSE)
    
    write.csv(profiles_kmeans, file.path(output_dir, paste0(tag, "_kmeans_cluster_assignments.csv")), row.names = FALSE)
    
    cluster_summary_kmeans <- profiles_kmeans %>%
        group_by(cluster) %>%
        summarise(across(where(is.numeric) & !matches("n_serves"), mean, na.rm = TRUE))
    write.csv(cluster_summary_kmeans, file.path(output_dir, paste0(tag, "_kmeans_summary.csv")), row.names = FALSE)
    
    # PCA
    pca_res <- prcomp(df_scaled)
    pca_df <- as.data.frame(pca_res$x[, 1:2]) %>%
        mutate(cluster = kmeans_labels, ServerName = row_names)
    
    p_pca <- ggplot(pca_df, aes(x = PC1, y = PC2, color = cluster, label = ServerName)) +
        geom_point(size = 3, alpha = 0.7) + geom_text(size = 2, vjust = 1.5) +
        theme_minimal() + labs(title = paste("K-means PCA Projection –", tag))
    ggsave(file.path(output_dir, paste0(tag, "_kmeans_pca_plot.png")), p_pca, width = 8, height = 6, dpi = 300, bg = "white")
    
    write.csv(as.data.frame(pca_res$rotation), file.path(output_dir, paste0(tag, "_pca_loadings.csv")))
    
    pheatmap(as.data.frame(kmeans_res$centers),
             cluster_rows = TRUE, cluster_cols = TRUE,
             main = paste("K-means Cluster Centers –", tag),
             filename = file.path(output_dir, paste0(tag, "_kmeans_heatmap.png")),
             width = 8, height = 6)
    
    # Hierarchical
    hc <- hclust(dist(df_scaled), method = "ward.D2")
    hc_labels <- cutree(hc, k = 4)
    
    profiles_hc <- profiles_df %>%
        mutate(cluster = as.factor(hc_labels))
    
    write.csv(profiles_hc, file.path(output_dir, paste0(tag, "_hierarchical_cluster_assignments.csv")), row.names = FALSE)
    
    modal_props_hc <- profiles_hc %>%
        group_by(cluster, modal_location) %>%
        summarise(n = n(), .groups = "drop") %>%
        group_by(cluster) %>%
        mutate(prop = n / sum(n)) %>%
        pivot_wider(names_from = modal_location, values_from = prop, values_fill = 0)
    write.csv(modal_props_hc, file.path(output_dir, paste0(tag, "_hierarchical_modal_location_props.csv")), row.names = FALSE)
    
    cluster_summary_hc <- profiles_hc %>%
        group_by(cluster) %>%
        summarise(across(where(is.numeric) & !matches("n_serves"), mean, na.rm = TRUE))
    write.csv(cluster_summary_hc, file.path(output_dir, paste0(tag, "_hierarchical_summary.csv")), row.names = FALSE)
    
    png(file.path(output_dir, paste0(tag, "_hierarchical_dendrogram.png")), width = 1000, height = 700)
    plot(hc, labels = row_names, main = paste("Hierarchical Clustering –", tag))
    rect.hclust(hc, k = 4, border = 2:5)
    dev.off()
    
    return(profiles_kmeans)
}

plot_cluster_boxplots <- function(profiles, method_name, tag) {
    features <- c("avg_speed", "sd_speed", "ace_pct", "location_entropy")
    df_long <- profiles %>%
        select(cluster, all_of(features)) %>%
        pivot_longer(cols = all_of(features), names_to = "feature", values_to = "value") %>%
        mutate(cluster = factor(cluster), feature = factor(feature, levels = features))
    
    p <- ggplot(df_long, aes(x = cluster, y = value, fill = cluster)) +
        geom_boxplot(alpha = 0.7) +
        facet_wrap(~ feature, scales = "free_y") +
        theme_minimal() +
        labs(title = paste(method_name, "– Clustered Feature Distributions –", tag),
             x = "Cluster", y = "Value") +
        theme(legend.position = "none")
    
    ggsave(file.path(output_dir, paste0(tag, "_", method_name, "_boxplots_facet.png")),
           p, width = 10, height = 6, bg = "white")
}

plot_modal_location_barplot <- function(modal_props_df, method_name, tag) {
    modal_clean <- modal_props_df %>%
        select(where(~ is.numeric(.) || is.factor(.))) %>%
        select(-any_of("n"))
    
    df_long <- modal_clean %>%
        pivot_longer(-cluster, names_to = "location", values_to = "prop") %>%
        mutate(cluster = factor(cluster))
    
    p <- ggplot(df_long, aes(x = cluster, y = prop, fill = location)) +
        geom_col(position = "stack", color = "white") +
        theme_minimal() +
        labs(title = paste(method_name, "Modal Serve Location Proportions –", tag),
             x = "Cluster", y = "Proportion", fill = "Modal Location")
    
    ggsave(file.path(output_dir, paste0(tag, "_", method_name, "_modal_location_barplot.png")),
           p, width = 8, height = 5, bg = "white")
}

# --- Run all ---
serve_sets <- list(
    first = get_serve_profiles(df_clean, 1),
    second = get_serve_profiles(df_clean, 2),
    combined = get_serve_profiles(df_clean, c(1, 2))
)

for (serve_tag in names(serve_sets)) {
    profiles_kmeans <- run_clustering_models(serve_sets[[serve_tag]], serve_tag)
    
    tag <- paste0(tag_prefix, "_", serve_tag)
    hc_profiles <- fread(file.path(output_dir, paste0(tag, "_hierarchical_cluster_assignments.csv"))) %>%
        mutate(cluster = as.factor(cluster))
    kmeans_profiles <- fread(file.path(output_dir, paste0(tag, "_kmeans_cluster_assignments.csv"))) %>%
        mutate(cluster = as.factor(cluster))
    
    modal_kmeans <- fread(file.path(output_dir, paste0(tag, "_kmeans_modal_location_props.csv")))
    modal_hc     <- fread(file.path(output_dir, paste0(tag, "_hierarchical_modal_location_props.csv")))
    
    plot_cluster_boxplots(kmeans_profiles, "kmeans", tag)
    plot_cluster_boxplots(hc_profiles, "hierarchical", tag)
    
    plot_modal_location_barplot(modal_kmeans, "kmeans", tag)
    plot_modal_location_barplot(modal_hc, "hierarchical", tag)
}
