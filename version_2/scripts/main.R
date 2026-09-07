library(tidyverse)
library(janitor)
library(randomForest)

# Load CSV data sets
raw_data_location <- "version_2/raw_data"

load_csv <- function(file_location) {
  read_csv(file_location, name_repair = "universal_quiet")
}

disease_tibble <- load_csv(paste0(raw_data_location, "/DATA - Disease.csv"))
waste_tibble <- load_csv(paste0(raw_data_location, "/DATA - Waste.csv"))

# Generate training and testing indexes
train_indexes <- sample(1:nrow(disease_tibble), 0.8 * nrow(disease_tibble))

# Train a random forest model for each disease
train_rf <- function(column_index) {
  randomForest(
    x = slice(waste_tibble, train_indexes),
    y = disease_tibble[[column_index]][train_indexes]
  )
}

rf_models <- map(2:ncol(disease_tibble), train_rf)
names(rf_models) <- colnames(disease_tibble)[2:ncol(disease_tibble)]

