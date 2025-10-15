# --- Setup ---
rm(list = ls())
library(tidyverse)
library(data.table)
library(cluster)
library(factoextra)
library(recipes)
library(pheatmap)

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
        filter(n_serves > 20)
}

# --- get serve profiles ---
serve1_profiles <- get_serve_profiles(df_clean, serve_number_label = 1)
serve2_profiles <- get_serve_profiles(df_clean, serve_number_label = 2)

