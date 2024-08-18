#' Bagging Classification using majority vote method
#' This script implements the Bagging classification algorithm using majority voting

# Check and load required libraries
if (!require(ggplot2)) install.packages("ggplot2")
if (!require(cowplot)) install.packages("cowplot")

library(ggplot2)
library(cowplot)

source("greedy_algorithm.R")
source("main.R")

# Function generates a bootstrap sample from the provided data. It randomly selects 
# samples with replacement, creating a new dataset of the same size as the original:
#' @param X Data frame or matrix of predictors.
#' @param Y Vector of the response variable.
#' @return List containing the bootstrap sample of predictors (`X`) and the corresponding response variable (`Y`).
#' @export
#' @example
bootstrap_sample_classification <- function(X, Y) {
  
  
  if (!is.list(X) && !is.data.frame(X) && !is.matrix(X))
  {
    stop("X must be list or matrix or data frame!")
  }
  
  if (!is.vector(Y))
  {
    stop("Y must be vector!")
  }
  n <- nrow(X)
  
  if (n != length(Y))
  {
    stop("Dimension of x and y in data are not equal")
  }
  sample_indices <- sample(1:n, n, replace = TRUE)
  return(list("X" = X[sample_indices,], "Y" = Y[sample_indices]))
}

# Function trains multiple classification models (decision trees) on different bootstrap 
# samples. The number of models (`B`) can be specified:

bagging_classification<- function(X, Y, B = 100, x_vector = NULL) {
  #' @param X Data frame or matrix of predictors
  #' @param Y Vector of the response variable
  #' @param B Number of bootstrap samples/models to train (default is 100)
  #' @return List of trained classification models
  
  n <- nrow(X)
  models = list()
  
  for (i in 1:B) {
    sample_data <- bootstrap_sample_classification(X, Y)
    X_boot = sample_data$X
    Y_boot = sample_data$Y
    data <- list(x = X_boot, y = Y_boot)
    models[[i]] <- greedy_cart_classification(data = data) # Training a single decision tree on each bootstrap sample
  }
  
  return(models)
}

# Function aggregates the predictions from all trained models using majority voting:

bagging_classification_prediction <- function(models, X) {
  #' @param models List of trained models from bagging_training
  #' @param X Data frame or matrix of predictors to make predictions on
  #' @return Vector of aggregated predictions using majority voting
  
  B = length(models)
  n = nrow(X)
  predictions <- matrix(0, n, B)
  
  for (i in 1:B) {
    print(length(prediction(list_tree = models[[i]], list_x = t(X), type = 'class')))
    predictions[, i] <- prediction(list_tree = models[[i]], list_x = t(X), type = 'class')
  }
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
  grid$y <- bagging_classification_prediction(models, grid)
  
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

