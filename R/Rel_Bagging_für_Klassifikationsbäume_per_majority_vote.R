#' Bagging Classification using majority vote method
#' This script implements the Bagging classification algorithm using majority voting

# Check and load required libraries
if (!require(ggplot2)) install.packages("ggplot2")
if (!require(cowplot)) install.packages("cowplot")

library(ggplot2)
library(cowplot)

source("random_forsest.R")
source("main.R")

# Function generates a bootstrap sample from the provided data. It randomly selects 
# samples with replacement, creating a new dataset of the same size as the original:
#' @param X Data frame or matrix of predictors.
#' @param Y Vector of the response variable.
#' @return List containing the bootstrap sample of predictors (`X`) and the corresponding response variable (`Y`).
#' @export
#' @example
bootstrap_sample <- function(X, Y) {
  
  
  if (!is.list(X) && !is.data.frame(X) && !is.matrix(X))
  {
    stop("X must be list or matrix or data frame!")
  }
  
  if (!is.vector(Y))
  {
    stop("Y must be vector!")
  }
  n <- nrow(X)
  
  if (n != length(y))
  {
    stop("Dimension of x and y in data are not equal")
  }
  sample_indices <- sample(1:n, n, replace = TRUE)
  return(list("X" = X[sample_indices, ], "Y" = Y[sample_indices]))
}

# Function trains multiple classification models (decision trees) on different bootstrap 
# samples. The number of models (`B`) can be specified:

bagging_training <- function(X, Y, B = 100, x_vector = NULL) {
  #' @param X Data frame or matrix of predictors
  #' @param Y Vector of the response variable
  #' @param B Number of bootstrap samples/models to train (default is 100)
  #' @return List of trained classification models
  
  n <- nrow(X)
  models = list()
  
  for (i in 1:B) {
    sample_data <- bootstrap_sample(X, Y)
    X_boot = sample_data$X
    Y_boot = sample_data$Y
    data <- list(X_boot, y = Y_boot)
    models[[i]] <- random_forest(x = x_vector, y = y, type = 'cla', data = data, B = 1) # Training a single decision tree on each bootstrap sample
  }
  
  return(models)
}

# Function aggregates the predictions from all trained models using majority voting:

bagging_prediction <- function(models, X) {
  #' @param models List of trained models from bagging_training
  #' @param X Data frame or matrix of predictors to make predictions on
  #' @return Vector of aggregated predictions using majority voting
  
  B = length(models)
  n = nrow(X)
  predictions <- matrix(0, n, B)
  
  for (i in 1:B) {
    predictions[, i] <- prediction(list_tree = models[[i]], list_x = t(X), type = 'class')
  }
  print(predictions)
  # Use majority voting to determine the final prediction
  bagged_predictions <- apply(predictions, 1, function(row) names(which.max(table(row))))
  
  return(bagged_predictions)
}

# Function visualizes the decision boundary created by the bagging model 
# along with the actual data points:

plot_decision_boundary <- function(data, models, title) {
  #' @param data Data frame containing the original data points
  #' @param models List of trained models from bagging_training
  #' @param title Title for the plot
  #' @return ggplot object showing the decision boundary and data points.
  
  x_seq <- seq(0, 1, length.out = 100)
  grid <- expand.grid(x1 = x_seq, x2 = x_seq)
  #print(bagging_prediction(models,grid))
  grid$y <- bagging_prediction(models, grid)
  
  ggplot(data, aes(x = x1, y = x2, color = y)) +
    geom_point() +
    geom_contour(data = grid, aes(z = as.numeric(y)), breaks = c(1.5), col = "black") +
    ggtitle(title) +
    theme_minimal()
}

#' @examples 
#' models <- bagging_classification(X_train, Y_train, B = 1000)
#' predictions <- bagging_prediction(models, X_test)

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
models <- bagging_training(X, as.vector(as.numeric(Y)), B = 10, x_vector = c(x1, x2))
print(models)
plot1 <- plot_decision_boundary(data, models, "Bagging classification with B = 1000 trees")

# Train and plot for B = 1 (Single Tree)
one_tree <- bagging_training(X, Y, B = 1, x_vector = c(x1, x2))
plot2 <- plot_decision_boundary(data, one_tree, "Bagging classification with B = 1 tree")

# Combine plots using `cowplot`
plot_grid(plot1, plot2, ncol = 1)
