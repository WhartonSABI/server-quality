rm(list=ls())
library(tidyverse)

# --- Config ---
tournament <- "usopen"  # "wimbledon" or "usopen"
gender <- "m"              # "m" or "f"

# --- Paths ---
training_path <- file.path("../data/processed/scaled", paste0(tournament, "_subset_", gender, "_training.csv"))
testing_path <- file.path("../data/processed/scaled", paste0(tournament, "_subset_", gender, "_testing.csv"))

# combine data
training <- fread(training_path)
testing <- fread(testing_path)

all_data <- bind_rows(training, testing)

# 80-20 train test split
set.seed(123)
train_indices <- sample(seq_len(nrow(all_data)), size = 0.8 * nrow(all_data))
train_data <- all_data[train_indices, ]
test_data <- all_data[-train_indices, ]

fwrite(train_data, file.path("../data/processed/scaled", paste0(tournament, "_", gender, "_training.csv")))
fwrite(test_data, file.path("../data/processed/scaled", paste0(tournament, "_", gender, "_testing.csv")))

