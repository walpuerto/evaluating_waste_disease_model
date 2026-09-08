library(tidyverse)
library(janitor)
library(randomForest)

# For the sake of reproducibility
set.seed(1000)

# Prepare the data sets in a format usable in R
raw_data_location <- "version_2/raw_data"

load_csv <- function(file_location) {
  read_csv(file_location, name_repair = "universal_quiet")
}

disease_tibble <- load_csv(paste0(raw_data_location, "/DATA - Disease.csv"))
waste_tibble <- load_csv(paste0(raw_data_location, "/DATA - Waste.csv"))

# Allocate other indexes for testing later
train_indexes <- sample(1:nrow(disease_tibble), 0.7 * nrow(disease_tibble))

# Each disease should have its own model
train_rf <- function(column_index) {
  randomForest(
    x = slice(select(waste_tibble, -Name.of.Place), train_indexes),
    y = disease_tibble[[column_index]][train_indexes]
  )
}

rf_models <- map(2:ncol(disease_tibble), train_rf)
names(rf_models) <- colnames(disease_tibble)[2:ncol(disease_tibble)]
print(rf_models)

# This makes it easier to compute the Rank Graduation Box metrics later
generate_matrix <- function(column_index) {
  y_hat <- predict(
      rf_models[[column_index]],
      slice(select(waste_tibble, -Name.of.Place), -train_indexes)
    )
  names(y_hat) <- NULL
  y <- disease_tibble[[column_index + 1]][-train_indexes]
  tibble(y, y_hat)
}

prediction_matrices <- map(1:length(rf_models), generate_matrix)
names(prediction_matrices) <- names(rf_models)

# Calculate the accuracy of each model
source("rgb_metrics/rg_metric.R")

RGA <- function(index) {
  RG(
    prediction_matrices[[index]]$y,
    prediction_matrices[[index]]$y_hat,
    TRUE
  )
}

model_accuracies <- map(1:length(prediction_matrices), RGA)
names(model_accuracies) <- names(prediction_matrices)
print(model_accuracies)

# Calculate the variable importance of each model
train_reduced <- function(column_index) {
  reduced <- map(
    2:ncol(waste_tibble),
    \(excluded_index) {
      randomForest(
        x = slice(
          select(waste_tibble, -excluded_index, -Name.of.Place),
          train_indexes
          ),
        y = disease_tibble[[column_index]][train_indexes]
      )
    }
  )
  names(reduced) <- names(waste_tibble)[2:ncol(waste_tibble)]
  reduced
}

reduced_models <- map(2:ncol(disease_tibble), train_reduced)
names(reduced_models) <- colnames(disease_tibble)[2:ncol(disease_tibble)]

# Table it so that it is easier to look up the values later :))
generate_reduced_matrix <- function(disease_index) {
  models <- reduced_models[[disease_index]]
  variable_predictions <- map(
    1:length(models),
    \(variable_index) {
      y_hat <- predict(
        models[[variable_index]],
        slice(waste_tibble, -train_indexes)
      )
      names(y_hat) <- NULL
      y_hat
    }
  )
  names(variable_predictions) <- names(waste_tibble)[2:ncol(waste_tibble)]
  as_tibble(variable_predictions)
}

reduced_matrix <- map(1:length(reduced_models), generate_reduced_matrix)
names(reduced_matrix) <- colnames(disease_tibble)[2:ncol(disease_tibble)]

# Solve the RG of each reduced prediction to the full prediction
RGE <- function(disease_index) {
  y_full <- prediction_matrices[[disease_index]]$y_hat
  map(
    2:ncol(waste_tibble),
    \(y_reduced) {
      RG(
        y_full,
        y_reduced
      )
    }
  )
}

map(2:ncol(disease_tibble), RGE)


