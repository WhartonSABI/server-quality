# --- Setup ---
rm(list = ls())
library(tidyverse)
library(data.table)
library(recipes)
library(caret)
library(randomForest)
library(ggrepel)
library(scales)
library(ggplot2)
library(xgboost)
library(pheatmap)
library(viridis)
library(broom)

#-----------------------------
# --- Config ---
#-----------------------------
outcome_var <- "win_rate"  # "serve_efficiency" or "win_rate"
gender <- "m"                      # "m" or "f"
tournament <- "usopen"          # "wimbledon" or "usopen"

#-----------------------------
# --- Derived Tags and Paths ---
#-----------------------------
outcome_type <- ifelse(outcome_var == "serve_efficiency", "serve_efficiency_outcome", "win_pct_outcome")
tournament_gender_tag <- paste0(tournament, "_", ifelse(gender == "m", "males", "females"))

base_dir <- file.path("../data/results/server_quality_models", outcome_type, tournament_gender_tag)
dir.create(file.path(base_dir, "comparison"), recursive = TRUE, showWarnings = FALSE)
importance_dir <- file.path(base_dir, "importance_behavior")
dir.create(importance_dir, recursive = TRUE, showWarnings = FALSE)

train_path <- file.path("../data/processed/scaled", paste0(tournament, "_subset_", gender, "_training.csv"))
test_path  <- file.path("../data/processed/scaled", paste0(tournament, "_subset_", gender, "_testing.csv"))

get_cluster_path <- function(tournament, gender) {
    tg <- paste0(tournament, "_", ifelse(gender == "m", "males", "females"))
    file.path("../data/results/clustering", tg, paste0(tg, "_combined_kmeans_cluster_assignments.csv"))
}

#-----------------------------
# --- Helper Functions ---
#-----------------------------
compute_entropy <- function(x) {
    p <- prop.table(table(x))
    -sum(p * log2(p))
}

get_serve_profiles <- function(df, serve_numbers) {
    df_sub <- df %>%
        filter(ServeNumber %in% serve_numbers) %>%
        mutate(modal_location = paste0("W", ServeWidth, "_D", ServeDepth)) %>%
        group_by(ServerName)
    
    serve_stats <- df_sub %>%
        summarise(
            avg_speed = mean(Speed_MPH, na.rm = TRUE),
            sd_speed = sd(Speed_MPH, na.rm = TRUE),
            location_entropy = compute_entropy(modal_location),
            win_rate = mean(PointWinner == ServeIndicator, na.rm = TRUE),
            serve_efficiency = mean((PointWinner == ServeIndicator) & (RallyCount <= 3), na.rm = TRUE),
            modal_location = names(sort(table(modal_location), decreasing = TRUE))[1],
            n_serves = n(),
            .groups = "drop"
        ) %>%
        filter(n_serves > 20)
    
    modal_dummies <- model.matrix(~ modal_location - 1, data = serve_stats) %>% as.data.frame()
    modal_scaled <- as.data.frame(scale(modal_dummies))
    
    X_cont <- serve_stats %>% select(avg_speed, sd_speed, location_entropy)
    X_scaled <- as.data.frame(scale(X_cont))
    X_final <- bind_cols(X_scaled, modal_scaled)
    
    df_final <- bind_cols(serve_stats %>% select(ServerName, !!sym(outcome_var)), X_final)
    colnames(df_final)[2] <- outcome_var
    return(df_final)
}

evaluate_model_metric <- function(test_df, model_df, metric_col) {
    df <- test_df %>%
        inner_join(model_df %>% select(ServerName, !!sym(metric_col)), by = "ServerName") %>%
        group_by(ServerName) %>%
        summarise(
            actual = mean(!!sym(outcome_var)),
            metric_score = first(!!sym(metric_col)),
            .groups = "drop"
        )
    
    df <- df %>% mutate(across(c(actual, metric_score), scale))
    
    r <- cor(df$actual, df$metric_score, use = "complete.obs")
    lm_fit <- lm(actual ~ metric_score, data = df)
    pval <- summary(lm_fit)$coefficients["metric_score", "Pr(>|t|)"]
    rmse_val <- sqrt(mean((df$actual - df$metric_score)^2, na.rm = TRUE))
    
    tibble(
        cor = r,
        p_value = pval,
        rmse = rmse_val,
        avg_metric = mean(df$metric_score, na.rm = TRUE),
        n = nrow(df)
    )
}

run_pipeline_models <- function(df_clean, serve_label, output_dir, clusters_df) {
    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
    set.seed(42)
    
    # --- Build per-server profiles (main effects only) ---
    profiles <- get_serve_profiles(df_clean, serve_label)  # returns ServerName, outcome, and predictors
    
    # --- Merge clusters (lowercased ServerName was already enforced upstream) ---
    profiles <- profiles %>%
        inner_join(clusters_df %>% select(ServerName, cluster), by = "ServerName")
    
    # --- Targets and main-effect predictors ---
    y <- profiles[[outcome_var]]
    X_main <- profiles %>% select(-ServerName, -all_of(outcome_var))  # includes modal dummies, speeds, entropy, cluster
    
    # Ensure cluster is a factor
    if (!is.factor(X_main$cluster)) X_main$cluster <- factor(X_main$cluster)
    
    # --- Create interaction-expanded design matrix (cluster * all predictors) ---
    # This yields cluster main effects + interactions with every predictor
    X_int <- model.matrix(~ cluster * . - 1, data = X_main)  # '-1' to control intercept explicitly via dummies
    
    # --- Scale numeric columns of the design matrix (safe for LM/RF/XGB) ---
    X_int_scaled <- scale(X_int)
    
    # --- Modeling frame for caret ---
    df_model_scaled <- data.frame(X_int_scaled)
    df_model_scaled[[outcome_var]] <- y
    df_model_scaled <- df_model_scaled %>% relocate(all_of(outcome_var))
    
    # --- CV control ---
    cv_ctrl <- trainControl(method = "cv", number = 5, savePredictions = "final")
    
    # --- Linear Model with interactions already in columns ---
    lm_model <- train(
        reformulate(colnames(df_model_scaled)[-1], response = outcome_var),
        data = df_model_scaled,
        method = "lm",
        trControl = cv_ctrl
    )
    pred_lm  <- lm_model$pred %>% arrange(rowIndex) %>% pull(pred)
    resid_lm <- y - pred_lm
    std_pred_lm  <- scale(pred_lm)[,1]
    std_resid_lm <- scale(resid_lm)[,1]
    performance_lm <- std_pred_lm + std_resid_lm
    
    # Save LM coefficients
    tidy_lm <- broom::tidy(lm_model$finalModel)
    write.csv(tidy_lm, file.path(output_dir, paste0("lm_coefficients_", outcome_var, ".csv")), row.names = FALSE)
    
    # --- Random Forest on the same interaction-expanded matrix ---
    rf_model <- train(
        reformulate(colnames(df_model_scaled)[-1], response = outcome_var),
        data = df_model_scaled,
        method = "rf",
        importance = TRUE,
        trControl = cv_ctrl
    )
    pred_rf  <- rf_model$pred %>% arrange(rowIndex) %>% pull(pred)
    resid_rf <- y - pred_rf
    std_pred_rf  <- scale(pred_rf)[,1]
    std_resid_rf <- scale(resid_rf)[,1]
    performance_rf <- std_pred_rf + std_resid_rf
    
    # RF importance (now includes interaction features like "cluster2:avg_speed")
    rf_importance <- varImp(rf_model)$importance %>%
        rownames_to_column(var = "Variable") %>%
        arrange(desc(Overall))
    write.csv(rf_importance, file.path(output_dir, paste0("rf_importance_", outcome_var, ".csv")), row.names = FALSE)
    
    rf_plot <- ggplot(rf_importance, aes(x = reorder(Variable, Overall), y = Overall)) +
        geom_col(fill = "steelblue") +
        coord_flip() +
        labs(title = paste0("Random Forest Variable Importance (", outcome_var, ")"),
             x = "Variable", y = "Importance") +
        theme_minimal(base_size = 12)
    ggsave(file.path(output_dir, paste0("rf_importance_", outcome_var, ".png")), rf_plot, width = 8, height = 6, bg = "white")
    
    # --- Compute RF-weighted average on MAIN effects only (preserves interpretability) ---
    # Use ONLY non-interaction, non-cluster columns from the original (unexpanded) predictors
    # i.e., columns in X_main except 'cluster'
    X_main_only <- X_main %>% select(-cluster)
    X_main_scaled <- as.data.frame(scale(X_main_only))
    
    # Build a "main-effect-only" RF to derive weights OR reuse importance from the big RF but restrict to main effects.
    rf_main <- train(
        reformulate(colnames(X_main_scaled), response = outcome_var),
        data = cbind(setNames(list(y), outcome_var), X_main_scaled),
        method = "rf",
        importance = TRUE,
        trControl = cv_ctrl
    )
    
    rf_main_imp <- varImp(rf_main)$importance %>%
        rownames_to_column(var = "Variable") %>%
        arrange(desc(Overall))
    rf_weights <- rf_main_imp$Overall
    names(rf_weights) <- rf_main_imp$Variable
    rf_weights <- rf_weights / sum(rf_weights, na.rm = TRUE)
    common_vars <- intersect(names(rf_weights), colnames(X_main_scaled))
    weighted_avg <- rowSums(t(t(as.matrix(X_main_scaled[, common_vars, drop = FALSE])) * rf_weights[common_vars]))
    
    # --- XGBoost on the same interaction-expanded matrix ---
    xgb_grid <- expand.grid(nrounds = 100, max_depth = 3, eta = 0.1, gamma = 0,
                            colsample_bytree = 1, min_child_weight = 1, subsample = 1)
    xgb_model <- train(
        reformulate(colnames(df_model_scaled)[-1], response = outcome_var),
        data = df_model_scaled,
        method = "xgbTree",
        tuneGrid = xgb_grid,
        trControl = cv_ctrl,
        verbose = 0
    )
    pred_xgb  <- xgb_model$pred %>% arrange(rowIndex) %>% pull(pred)
    resid_xgb <- y - pred_xgb
    std_pred_xgb  <- scale(pred_xgb)[,1]
    std_resid_xgb <- scale(resid_xgb)[,1]
    performance_xgb <- std_pred_xgb + std_resid_xgb
    
    # Save XGBoost importance (feature names = design matrix columns)
    xgb_imp <- xgb.importance(feature_names = colnames(df_model_scaled)[-1], model = xgb_model$finalModel)
    write.csv(xgb_imp, file.path(output_dir, paste0("xgb_importance_", outcome_var, ".csv")), row.names = FALSE)
    
    xgb_plot <- ggplot(xgb_imp, aes(x = reorder(Feature, Gain), y = Gain)) +
        geom_col(fill = "darkorange") +
        coord_flip() +
        labs(title = paste0("XGBoost Variable Importance (", outcome_var, ")"), x = "Variable", y = "Gain") +
        theme_minimal(base_size = 12)
    ggsave(file.path(output_dir, paste0("xgb_importance_", outcome_var, ".png")), xgb_plot, width = 8, height = 6, bg = "white")
    
    # helper function to rescale to [-1, 1]
    rescale_signed <- function(x) {
        max_abs <- max(abs(x), na.rm = TRUE)
        if (max_abs == 0) return(rep(0, length(x)))
        x / max_abs
    }
    
    # --- Output Combined ---
    profiles_extended <- profiles %>%
        mutate(
            performance_lm  = rescale_signed(performance_lm),
            performance_rf  = rescale_signed(performance_rf),
            performance_xgb = rescale_signed(performance_xgb),
            pred_lm = pred_lm,
            pred_rf = pred_rf,
            pred_xgb = pred_xgb,
            weighted_avg = rescale_signed(weighted_avg)
        )
    
    write.csv(profiles_extended, file.path(output_dir, "serve_quality_all_models.csv"), row.names = FALSE)
    return(list(profiles = profiles_extended))
}

evaluate_welo_baseline <- function(df_train, df_test, serve_numbers, model_name) {
    train <- df_train %>% filter(ServeNumber %in% serve_numbers)
    test <- df_test %>% filter(ServeNumber %in% serve_numbers)
    
    train_welo_df <- train %>%
        mutate(welo = if_else(ServeIndicator == 1, player1_avg_welo, player2_avg_welo)) %>%
        group_by(ServerName) %>%
        summarise(avg_welo = mean(welo, na.rm = TRUE), .groups = "drop")
    
    test_performance_df <- test %>%
        group_by(ServerName) %>%
        summarise(
            win_rate = mean(PointWinner == ServeIndicator, na.rm = TRUE),
            serve_efficiency = mean((PointWinner == ServeIndicator) & (RallyCount <= 3), na.rm = TRUE),
            .groups = "drop"
        )
    
    df <- inner_join(train_welo_df, test_performance_df, by = "ServerName") %>%
        drop_na()
    
    df <- df %>%
        mutate(
            avg_welo = scale(avg_welo)[, 1],
            win_rate = scale(win_rate)[, 1],
            serve_efficiency = scale(serve_efficiency)[, 1]
        )
    
    r <- cor(df[[outcome_var]], df$avg_welo)
    lm_fit <- lm(df[[outcome_var]] ~ avg_welo, data = df)
    pval <- summary(lm_fit)$coefficients["avg_welo", "Pr(>|t|)"]
    rmse_val <- sqrt(mean((df[[outcome_var]] - predict(lm_fit))^2))
    
    tibble(
        cor = r,
        p_value = pval,
        rmse = rmse_val,
        avg_metric = mean(df$avg_welo, na.rm = TRUE),
        n = nrow(df),
        Model = "avg_welo",
        Data = model_name
    )
}

save_evals <- function(name, model_df, df_test_clean, df_train_clean, serve_numbers) {
    metrics <- c("performance_lm", "performance_rf", "performance_xgb", "weighted_avg")
    out <- lapply(metrics, function(m) evaluate_model_metric(df_test_clean, model_df, m) %>% mutate(Model = m))
    result <- bind_rows(out) %>% mutate(Data = name)
    
    # Add Welo baseline
    welo_result <- evaluate_welo_baseline(df_train_clean, df_test_clean, serve_numbers, name)
    result <- bind_rows(result, welo_result)
    
    write.csv(result, file.path(base_dir, "comparison", paste0("test_eval_", name, ".csv")), row.names = FALSE)
    return(result)
}

plot_scatter_models <- function(df_test_clean, model_df, df_train_clean, serve_numbers, label) {
    outcome_label <- ifelse(outcome_var == "serve_efficiency", "Serve Efficiency", "Win Percentage")
    
    train_welo_df <- df_train_clean %>%
        filter(ServeNumber %in% serve_numbers) %>%
        mutate(welo = if_else(ServeIndicator == 1, player1_avg_welo, player2_avg_welo)) %>%
        group_by(ServerName) %>%
        summarise(avg_welo = mean(welo, na.rm = TRUE), .groups = "drop")
    
    actual_df <- df_test_clean %>%
        filter(ServeNumber %in% serve_numbers) %>%
        group_by(ServerName) %>%
        summarise(actual = mean(!!sym(outcome_var), na.rm = TRUE), .groups = "drop")
    
    all_df <- actual_df %>%
        inner_join(model_df %>% select(ServerName, performance_lm, performance_rf, performance_xgb, weighted_avg), by = "ServerName") %>%
        inner_join(train_welo_df, by = "ServerName")
    
    all_df <- all_df %>%
        mutate(
            actual = scale(actual)[, 1],
            avg_welo = scale(avg_welo)[, 1]
        ) %>%
        rename(
            LM = performance_lm,
            RF = performance_rf,
            XGB = performance_xgb,
            Weighted = weighted_avg,
            Welo = avg_welo
        )
    
    metric_names <- c("LM", "RF", "XGB", "Weighted", "Welo")
    
    for (m in metric_names) {
        df_m <- all_df %>%
            select(ServerName, actual, metric_score = !!sym(m)) %>%
            mutate(Model = m)
        
        lm_fit <- lm(actual ~ metric_score, data = df_m)
        df_m <- df_m %>%
            mutate(predicted = predict(lm_fit),
                   residual = actual - predicted,
                   above_line = residual > 0)
        
        p <- ggplot(df_m, aes(x = metric_score, y = actual)) +
            geom_point(aes(color = above_line, alpha = abs(residual)), size = 2.5) +
            geom_line(aes(y = predicted), linetype = "dotted", color = "black") +
            scale_color_manual(values = c("red", "darkgreen")) +
            geom_text_repel(aes(label = ifelse(abs(residual) > 0.5, ServerName, "")), size = 3) +
            theme_minimal(base_size = 12) +
            labs(
                title = paste(label, "-", m, "Model"),
                x = paste("Server Quality (", m, ")"),
                y = paste("Actual", outcome_label)
            )
        
        ggsave(file.path(base_dir, "comparison", paste0("scatter_", tolower(label), "_", tolower(m), ".png")),
               p, width = 9, height = 6, bg = "white")
    }
}

#-----------------------------
# --- Load Data ---
#-----------------------------
df_train <- fread(train_path)
df_test <- fread(test_path)

# load clusters
cluster_path <- get_cluster_path(tournament, gender)
clusters_df <- fread(cluster_path) %>%
    mutate(
        ServerName = tolower(ServerName),
        cluster = factor(cluster)   # ensure categorical (levels "1","2","3","4")
    )

# --- Preprocess ---
df_clean <- df_train %>%
    filter(!is.na(ServeWidth), !is.na(ServeDepth), ServeWidth != "", ServeDepth != "") %>%
    filter(ServeNumber %in% c(1, 2)) %>%
    mutate(
        location_bin = paste0("W", ServeWidth, "_D", ServeDepth),
        modal_location = paste0("W", ServeWidth, "_D", ServeDepth),
        ServerName = tolower(if_else(ServeIndicator == 1, player1, player2)),
        is_ace = if_else(ServeIndicator == 1, P1Ace, P2Ace),
        win_rate = PointWinner == ServeIndicator,
        serve_efficiency = (PointWinner == ServeIndicator) & (RallyCount <= 3)
    )

df_test_clean <- df_test %>%
    filter(!is.na(ServeWidth), !is.na(ServeDepth), ServeWidth != "", ServeDepth != "") %>%
    filter(ServeNumber %in% c(1, 2)) %>%
    mutate(
        modal_location = paste0("W", ServeWidth, "_D", ServeDepth),
        ServerName = tolower(if_else(ServeIndicator == 1, player1, player2)),
        win_rate = PointWinner == ServeIndicator,
        serve_efficiency = (PointWinner == ServeIndicator) & (RallyCount <= 3)
    )

#-----------------------------
# --- Run Models and Save Results ---
#-----------------------------
first_results    <- run_pipeline_models(df_clean, 1, file.path(base_dir, "first_serve"),  clusters_df)
second_results   <- run_pipeline_models(df_clean, 2, file.path(base_dir, "second_serve"), clusters_df)
combined_results <- run_pipeline_models(df_clean, c(1, 2), file.path(base_dir, "combined"), clusters_df)

eval_first    <- save_evals("first", first_results$profiles, df_test_clean, df_clean, 1)
eval_second   <- save_evals("second", second_results$profiles, df_test_clean, df_clean, 2)
eval_combined <- save_evals("combined", combined_results$profiles, df_test_clean, df_clean, c(1, 2))

plot_scatter_models(df_test_clean, first_results$profiles, df_clean, 1, "First Serve")
plot_scatter_models(df_test_clean, second_results$profiles, df_clean, 2, "Second Serve")
plot_scatter_models(df_test_clean, combined_results$profiles, df_clean, c(1, 2), "Combined")

#-----------------------------
# === Score State Behavior Analysis ===
#-----------------------------
df_clean <- df_clean %>%
    mutate(modal_location = paste0("W", ServeWidth, "_D", ServeDepth))

df_clean_enriched <- df_clean %>%
    left_join(combined_results$profiles %>% select(ServerName, serve_score_rf_weighted = weighted_avg), by = "ServerName") %>%
    filter(!is.na(serve_score_rf_weighted))

median_quality <- median(df_clean_enriched$serve_score_rf_weighted, na.rm = TRUE)
df_clean_enriched <- df_clean_enriched %>%
    mutate(quality_group = if_else(serve_score_rf_weighted >= median_quality, "High Quality", "Low Quality"))

state_order <- df_clean_enriched %>%
    group_by(state) %>%
    summarise(mean_importance = mean(importance, na.rm = TRUE), .groups = "drop") %>%
    arrange(mean_importance) %>%
    pull(state)

custom_order <- c("AD-40", "40-AD",
                  "40-40", "30-40", "15-40", "0-40",
                  "40-30", "30-30", "15-30", "0-30",
                  "40-15", "30-15", "15-15", "0-15",
                  "40-0",  "30-0",  "15-0",  "0-0")

df_list <- list(
    importance = df_clean_enriched %>% mutate(state = factor(state, levels = state_order)),
    custom = df_clean_enriched %>% mutate(state = factor(state, levels = custom_order))
)

make_boxplot <- function(data, xvar, yvar, fillvar, title, ylabel, filename) {
    p <- ggplot(data, aes_string(x = xvar, y = yvar, fill = fillvar)) +
        geom_boxplot(outlier.size = 0.5, alpha = 0.75) +
        labs(title = title, x = "Score State", y = ylabel, fill = "Server Type") +
        theme_minimal(base_size = 12) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    ggsave(file.path(importance_dir, filename), p, width = 9, height = 5.5, bg = "white")
}

walk2(names(df_list), df_list, function(tag, df) {
    make_boxplot(df, "state", "Speed_MPH", "quality_group",
                 paste("Serve Speed by Score State (", tag, " order)", sep = ""),
                 "Serve Speed (MPH)",
                 paste0("boxplot_speed_by_state_", tag, ".png"))
    
    ace_df <- df %>%
        group_by(ServerName, state, quality_group) %>%
        summarise(ace_pct = mean(is_ace, na.rm = TRUE), .groups = "drop")
    
    make_boxplot(ace_df, "state", "ace_pct", "quality_group",
                 paste("Ace Percentage by Score State (", tag, " order)", sep = ""),
                 "Ace Percentage",
                 paste0("boxplot_acepct_by_state_", tag, ".png"))
    
    entropy_df <- df %>%
        group_by(ServerName, state, quality_group) %>%
        summarise(location_entropy = compute_entropy(modal_location), .groups = "drop")
    
    make_boxplot(entropy_df, "state", "location_entropy", "quality_group",
                 paste("Serve Location Entropy by Score State (", tag, " order)", sep = ""),
                 "Serve Location Entropy",
                 paste0("boxplot_entropy_by_state_", tag, ".png"))
})

heatmap_df <- df_clean %>%
    group_by(modal_location) %>%
    summarise(mean_importance = mean(importance, na.rm = TRUE), .groups = "drop") %>%
    separate(modal_location, into = c("Width", "Depth"), sep = "_D") %>%
    mutate(Width = str_remove(Width, "W"))

heatmap_matrix <- heatmap_df %>%
    pivot_wider(names_from = Depth, values_from = mean_importance) %>%
    column_to_rownames("Width") %>%
    as.matrix()

heatmap_df_long <- as.data.frame(as.table(heatmap_matrix))
colnames(heatmap_df_long) <- c("ServeWidth", "ServeDepth", "Importance")

p_heatmap <- ggplot(heatmap_df_long, aes(x = ServeDepth, y = ServeWidth, fill = Importance)) +
    geom_tile(color = "white") +
    scale_fill_viridis(name = "Mean Importance", option = "D") +
    theme_minimal() +
    labs(
        title = "Mean Point Importance by Serve Location",
        x = "Serve Depth", y = "Serve Width"
    ) +
    theme(axis.text = element_text(size = 10),
          plot.title = element_text(hjust = 0.5, face = "bold"))

ggsave(file.path(importance_dir, "modal_location_heatmap.png"),
       p_heatmap, width = 7, height = 5, bg = "white")

