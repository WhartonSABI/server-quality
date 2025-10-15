rm(list = ls())
library(tidyverse)
library(data.table)
library(ggrepel)
library(ggplot2)
library(scales)

df <- fread("../data/processed/scaled/usopen_subset_m_training.csv")

df_clean <- df %>%
    filter(!is.na(ServeWidth), !is.na(ServeDepth), ServeWidth != "", ServeDepth != "") %>%
    filter(ServeNumber %in% c(1, 2)) %>%
    mutate(
        location_bin = paste0("W", ServeWidth, "_D", ServeDepth),
        ServerName = tolower(if_else(ServeIndicator == 1, player1, player2)),
        is_ace = if_else(ServeIndicator == 1, P1Ace, P2Ace),
        state = as.character(state)  # ensure matchable format
    )

df_clean <- df_clean %>%
    filter(!is.na(importance))

#Functions
compute_entropy <- function(x) {
    p <- prop.table(table(x))
    -sum(p * log2(p))
}
get_mode <- function(x) {
    ux <- unique(x)
    ux[which.max(tabulate(match(x, ux)))]
}

#Importance Mapping
importance_map <- fread("../data/results/score_importance_dtmc_hard.csv") %>%
    select(state, importance) %>%
    distinct()

importance_to_states <- importance_map %>%
    group_by(importance) %>%
    summarise(states = paste(sort(unique(state)), collapse = ", "), .groups = "drop")

#Importance Summary
importance_summary <- df_clean %>%
    group_by(importance) %>%
    summarise(
        avg_speed = mean(Speed_MPH, na.rm = TRUE),
        sd_speed = sd(Speed_MPH, na.rm = TRUE),
        ace_pct = mean(is_ace, na.rm = TRUE),
        location_entropy = compute_entropy(location_bin),
        modal_location = get_mode(location_bin),
        n = n(),
        .groups = "drop"
    ) %>%
    left_join(importance_to_states, by = "importance") %>%
    arrange(desc(importance))

#write.csv(importance_summary, "../data/results/importance_analysis/behavior_by_importance.csv", row.names = FALSE)

#EDA PLOTS
dir.create("../data/results/importance_analysis", showWarnings = FALSE)

p1 <- ggplot(importance_summary, aes(x = importance, y = avg_speed)) +
    geom_point(size = 2) + geom_smooth(method = "lm", se = FALSE) +
    theme_minimal() + labs(title = "Average Speed vs Importance", y = "Avg Speed (mph)", x = "Importance")

p2 <- ggplot(importance_summary, aes(x = importance, y = sd_speed)) +
    geom_point(size = 2) + geom_smooth(method = "lm", se = FALSE) +
    theme_minimal() + labs(title = "Speed SD vs Importance", y = "Speed SD", x = "Importance")

p3 <- ggplot(importance_summary, aes(x = importance, y = ace_pct)) +
    geom_point(size = 2) + geom_smooth(method = "lm", se = FALSE) +
    theme_minimal() + labs(title = "Ace % vs Importance", y = "Ace %", x = "Importance")

p4 <- ggplot(importance_summary, aes(x = importance, y = location_entropy)) +
    geom_point(size = 2) + geom_smooth(method = "lm", se = FALSE) +
    theme_minimal() + labs(title = "Serve Location Entropy vs Importance", y = "Entropy", x = "Importance")

ggsave("../data/results/importance_analysis/avg_speed_vs_importance.png", p1, width = 6, height = 4, bg = "white")
ggsave("../data/results/importance_analysis/sd_speed_vs_importance.png", p2, width = 6, height = 4, bg = "white")
ggsave("../data/results/importance_analysis/ace_pct_vs_importance.png", p3, width = 6, height = 4, bg = "white")
ggsave("../data/results/importance_analysis/entropy_vs_importance.png", p4, width = 6, height = 4, bg = "white")

#REGRESSION
lm1 <- lm(avg_speed ~ importance, data = importance_summary)
lm2 <- lm(sd_speed ~ importance, data = importance_summary)
lm3 <- lm(ace_pct ~ importance, data = importance_summary)
lm4 <- lm(location_entropy ~ importance, data = importance_summary)

sink("../data/results/importance_analysis/linear_model_summaries.txt")
cat("Regression Summaries:\n\n")
summary(lm1)
summary(lm2)
summary(lm3)
summary(lm4)
sink()

#Importance Frequency
modal_counts <- df_clean %>%
    group_by(importance, location_bin) %>%
    summarise(n = n(), .groups = "drop") %>%
    group_by(importance) %>%
    mutate(prop = n / sum(n)) %>%
    pivot_wider(names_from = location_bin, values_from = prop, values_fill = 0)

write.csv(modal_counts, "../data/results/importance_analysis/modal_location_props.csv", row.names = FALSE)
