#' Bagging Classification using class probabilities
#' This script implements the Bagging classification algorithm using class probabilities

# Check and load required libraries
if (!require(randomForest)) install.packages("randomForest")
if (!require(ggplot2)) install.packages("ggplot2")

library(randomForest)
library(ggplot2)

# Function generates a bootstrap sample from the provided data. 
# It randomly selects samples with replacement, creating a new dataset of the same size as the original:

bootstrap_sample <- function(X, Y) {
  #' @param X Data frame or matrix of predictors
  #' @param Y Vector of the response variable
  #' @return A list containing the bootstrap sample of predictors (`X`) and the corresponding response variable (`Y`)
  
  n <- nrow(X)
  sample_indices <- sample(1:n, n, replace = TRUE)
  return(list("X" = X[sample_indices, ], "Y" = Y[sample_indices]))
}

# Function trains multiple classification models (decision trees) on different 
# bootstrap samples. The number of models (`B`) can be specified:

bagging_classification <- function(X, Y, B = 100) {
  #' @param X Data frame or matrix of predictors
  #' @param Y Vector of the response variable
  #' @param B Number of bootstrap samples/models to train (default is 100)
  #' @return List of trained classification models

  models <- list()
  
  for (b in 1:B) {
    sample_data <- bootstrap_sample(X, Y)
    X_boot = sample_data$X
    Y_boot = sample_data$Y
    
    models[[b]] <- randomForest(X_boot, Y_boot, ntree = 1) # Training a single decision tree on each bootstrap sample
  }
  
  return(models)
}

# Function aggregates the predictions from all trained models using class probabilities.

bagging_prediction <- function(models, X, class_count) {
  #' @param models List of trained models from bagging_training
  #' @param X Data frame or matrix of predictors to make predictions on
  #' @param class_count The number of distinct classes in the response variable
  #' @return Vector of aggregated predictions using majority voting
  
  B <- length(models)
  n <- nrow(X)
  K <- class_count
  
  prob_matrix <- matrix(0, n, K) # Matrix to store the summed probabilities
  
  for (i in 1:B) {
    prob_matrix <- prob_matrix + predict(models[[i]], X, type = "prob") # Summing up predicted probabilities
  }
  
  prob_matrix <- prob_matrix / B # Averaging the probabilities
  bagged_predictions <- apply(prob_matrix, 1, which.max) # Selecting the class with the highest average probability
  
  return(bagged_predictions)
}

# Function visualizes the decision boundary created by the bagging model 
# along with the actual data points:

plot_decision_boundary <- function(data, models, class_count, title) {
  #' @param data Data frame containing the original data points
  #' @param models List of trained models from bagging_training
  #' @param title Title for the plot
  #' @return ggplot object showing the decision boundary and data points.
  
  x_seq <- seq(0, 1, length.out = 100)
  grid <- expand.grid(x1 = x_seq, x2 = x_seq)
  grid$y <- bagging_prediction(models, grid, class_count)
  
  ggplot(data, aes(x = x1, y = x2, color = y)) +
    geom_point() +
    geom_contour(data = grid, aes(z = as.numeric(y)), breaks = c(1.5), col = "black") +
    ggtitle(title) +
    theme_minimal()
}

#' @examples 
#' models <- bagging_classification(X_train, Y_train, B = 1000)
#' predictions <- bagging_prediction(models, X_test, unique(Y_train))


# The script generates synthetic data for classification to demonstrate the bagging process:

set.seed(123)
n <- 400
x1 <- runif(n, 0, 1)
x2 <- runif(n, 0, 1)
y <- ifelse(sin(2 * pi * x1) + rnorm(n, 0, 0.1) > x2, 1, 2)
data <- data.frame(x1 = x1, x2 = x2, y = as.factor(y))

# Two models are trained and visualized:
# 
# 1. Single Tree Model (`B = 1`):
#    - Trains a single decision tree and visualizes the decision boundary.
# 
# 2. Bagging Model (`B = 1000`):
#    - Trains 1000 decision trees and visualizes the decision boundary.

# Extract predictors and response variable from the data
X = data[, !names(data) %in% "y"]
Y = data$y

# Train and plot for B = 1000 (Bagging)
models <- bagging_classification(X, Y, B = 1000)
plot1 <- plot_decision_boundary(data, models, length(unique(Y)), "Bagging classification with 1000 trees")

# Train and plot for B = 1 (Single Tree)
one_tree <- bagging_classification(X, Y, B = 1)
plot2 <- plot_decision_boundary(data, one_tree, length(unique(Y)), "Bagging classification with 1 tree")

# Combine plots using `cowplot`
plot_grid(plot1, plot2, ncol = 1)
