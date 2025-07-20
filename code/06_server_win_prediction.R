# --- Setup ---
rm(list = ls())
# install.packages(c("caret", "Metrics"))
library(tidyverse)
library(data.table)
library(caret)
library(Metrics)  # for logLoss()

# --- Load + clean data ---
df <- fread("../data/processed/scaled/wimbledon_subset_m_training.csv")

df <- df %>%
    filter(!is.na(ServeWidth), !is.na(ServeDepth)) %>%
    filter(ServeNumber %in% c(1, 2)) %>%
    mutate(
        location_bin = paste0("W", ServeWidth, "_D", ServeDepth),
        ServerName = ifelse(ServeIndicator == 1, player1, player2),
    )


# --- Entropy helper ---
compute_entropy <- function(x) {
    p <- prop.table(table(x))
    -sum(p * log2(p))
}

# --- Split by serve number ---
df_first <- df %>% filter(ServeNumber == 1)
df_second <- df %>% filter(ServeNumber == 2)

# --- Compute serve location entropy for each player ---
entropy_first <- df_first %>%
    group_by(ServerName) %>%
    summarise(entropy = compute_entropy(location_bin), .groups = "drop")

entropy_second <- df_second %>%
    group_by(ServerName) %>%
    summarise(entropy = compute_entropy(location_bin), .groups = "drop")

# --- Merge entropy + select variables ---
df_first <- df_first %>%
    left_join(entropy_first, by = "ServerName") %>%
    select(serving_player_won, Speed_MPH, ServeWidth, ServeDepth, entropy) %>%
    drop_na()
# could add p_server_beats_returner and importance to control for skill & point importance
# either way, entropy isn't significant

df_second <- df_second %>%
    left_join(entropy_second, by = "ServerName") %>%
    select(serving_player_won, Speed_MPH, ServeWidth, ServeDepth, entropy) %>%
    drop_na()

# --- Train-test split function ---
split_data <- function(df, train_frac = 0.8) {
    set.seed(42)
    train_index <- createDataPartition(df$serving_player_won, p = train_frac, list = FALSE)
    train <- df[train_index, ]
    test <- df[-train_index, ]
    return(list(train = train, test = test))
}

# --- Standardization using training stats ---
standardize_with_train <- function(train, test) {
    # Define formula for dummy variables
    dummies <- dummyVars(~ ., data = train %>% select(-serving_player_won))
    
    # Create numeric matrices
    train_x <- predict(dummies, newdata = train)
    test_x <- predict(dummies, newdata = test)
    
    # Scale using training set stats
    pre_proc <- preProcess(train_x, method = c("center", "scale"))
    train_scaled <- predict(pre_proc, train_x)
    test_scaled <- predict(pre_proc, test_x)
    
    # Reattach outcome
    train_final <- bind_cols(serving_player_won = train$serving_player_won, as.data.frame(train_scaled))
    test_final <- bind_cols(serving_player_won = test$serving_player_won, as.data.frame(test_scaled))
    
    return(list(train = train_final, test = test_final))
}

# --- First serve: split and standardize ---
split_first <- split_data(df_first)
first_std <- standardize_with_train(split_first$train, split_first$test)

# --- Second serve: split and standardize ---
split_second <- split_data(df_second)
second_std <- standardize_with_train(split_second$train, split_second$test)

# --- Fit models ---
model_first <- glm(serving_player_won ~ ., data = first_std$train, family = binomial)
model_second <- glm(serving_player_won ~ ., data = second_std$train, family = binomial)

# --- Predict and evaluate ---
evaluate_model <- function(model, test_data, label = "Model") {
    pred_probs <- predict(model, newdata = test_data, type = "response")
    pred_class <- ifelse(pred_probs >= 0.5, 1, 0)
    accuracy <- mean(pred_class == test_data$serving_player_won)
    logloss <- logLoss(actual = test_data$serving_player_won, predicted = pred_probs)
    
    cat(paste0("\n=== ", label, " ===\n"))
    cat(sprintf("Accuracy: %.4f\n", accuracy))
    cat(sprintf("Log Loss: %.4f\n", logloss))
    print(confusionMatrix(as.factor(pred_class), as.factor(test_data$serving_player_won)))
}

# --- Results ---
cat("\n=== First Serve Model Summary ===\n")
print(summary(model_first))

cat("\n=== Second Serve Model Summary ===\n")
print(summary(model_second))

evaluate_model(model_first, first_std$test, label = "First Serve Model")
evaluate_model(model_second, second_std$test, label = "Second Serve Model")
