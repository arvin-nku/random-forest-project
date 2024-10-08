#' #' Bagging regression
#' #' This script implements a Bagging approach for regression 
#' 
#' source("greedy_algorithm.R")
#' source("main.R")
#' 
#' # Function generates a bootstrap sample from the provided data. It randomly selects 
#' # samples with replacement, creating a new dataset of the same size as the original:
#' #' @param X Data frame or matrix of predictors.
#' #' @param Y Vector of the response variable.
#' #' @return List containing the bootstrap sample of predictors (`X`) and the corresponding response variable (`Y`).
#' #' @export
#' #' @example
#' bootstrap_sample_regression <- function(X, Y) {
#'   if (!is.list(X) && !is.data.frame(X) && !is.matrix(X))
#'   {
#'     stop("X must be list or matrix or data frame!")
#'   }
#'   
#'   if (!is.vector(Y))
#'   {
#'     stop("Y must be vector!")
#'   }
#'   n <- ncol(X)
#'   if (n != length(Y))
#'   {
#'     stop("Dimension of x and y in data are not equal")
#'   }
#'   sample_indices <- sample(1:n, n, replace = TRUE)
#'   return(list("X" = X[, sample_indices, drop = F], "Y" = Y[sample_indices]))
#' }
#' 
#' # Function trains multiple regression models (decision trees) on different bootstrap 
#' # samples. The number of models (`B`) can be specified:
#' 
#' #' @param X Data frame or matrix of predictors
#' #' @param Y Vector of the response variable
#' #' @param B Number of bootstrap samples/models to train (default is 100)
#' #' @return List of trained regression models.
#' #' @export
#' #' @example
#' 
#' bagging_regression <- function(X, Y, B = 100) {
#'   
#'   if (!is.list(X) && !is.data.frame(X) && !is.matrix(X))
#'   {
#'     stop("X must be list or matrix or data frame!")
#'   }
#'   
#'   if (!is.vector(Y))
#'   {
#'     stop("Y must be vector!")
#'   }
#'   
#'   if(as.integer(B)!= B){
#'     warning("B is not an integer. The value of B is set to ", trunc(B))
#'     B <- trunc(B)
#'   }
#'   if(B < 1){
#'     stop("Number of bootstrap samples must be greater than or equal to 1")
#'   }
#'   models <- list()
#'   
#'   
#'   for (i in 1:B) {
#'     #debug(bootstrap_sample)
#'     sample_data <- bootstrap_sample_regression(X, Y)
#'     X_boot = sample_data$X
#'     Y_boot = sample_data$Y
#'     #print(X_boot)
#'     data <- list(x = X_boot, y = Y_boot)
#'     model <- greedy_cart_regression(data, num_split = 5)# Training a single decision tree on each bootstrap sample
#'     models[[i]] <- model
#'   }
#'   
#'   return(models)
#' }
#' 
#' # Function aggregates the predictions from all trained models by averaging them:
#' #' @param models List of trained models from bagging_regression
#' #' @param X Data frame or matrix of predictors to make predictions on
#' #' @return Vector of aggregated predictions.
#' #' @export
#' #' @example
#' 
#' bagging_regression_prediction <- function(models, X) {
#' 
#'   if (!is.list(models))
#'   {
#'     stop("Models must be list!")
#'   }
#'   
#'   if (!is.list(X) && !is.data.frame(X) && !is.matrix(X))
#'   {
#'     stop("X must be list or matrix or data frame!")
#'   }
#'   #print(models)
#'   
#'   preds <- sapply(models, function(model) {
#'     #debug(prediction)
#'     prediction(list_tree = list(model), list_x = X, type = 'reg')
#'   })
#'   
#'   #print(preds)
#'   return(rowMeans(preds))
#' }
#' 
#' #' @examples
#' #' models <- bagging_regression(X_train, Y_train, B = 1000)
#' #' predictions <- bagging_prediction(models, X_test)
#' 
#' # The script generates synthetic data to demonstrate the bagging regression process:
#' 
#' #set.seed(123)
#' #n <- 200
#' #x <- runif(n, 0, 1)
#' #y <- sin(2 * pi * x) + rnorm(n, 0, 0.1)
#' #data <- list(x = matrix(x, nrow = 1, byrow = TRUE), y = y)
#' 
#' 
#' # Two models are trained and visualized:
#' # 
#' # 1. Single Tree Model (`B = 1`):
#' #    - Trains a single decision tree and visualizes the prediction against the true function.
#' # 
#' # 2. Bagging Model (`B = 1000`):
#' #    - Trains 1000 decision trees and visualizes the aggregated prediction.
#' 
#' # Extract predictors and response variable from the data
#' #X <- data$x
#' #Y <- data$y
#' #print(X)
#' #print(Y)
#' # Plotting layout
#' #par(mfrow = c(2, 2))
#' 
#' # Training and plotting for B = 1 (Single Tree)
#' #debug(random_forest)
#' #debug(bootstrap_sample)
#' #debug(find_best_split_regression)
#' #single_model <- random_forest(x = x, y = y, type = 'reg', data = data, B = 1, A = 200, m = 1, num_leaf = 10, depth = NULL, num_split = 2, min_num = 1)#bagging_regression(X, Y, B = 1)
#' #print("A")
#' #print(single_model)
#' #pred_single <- bagging_prediction(list(single_model), X)
#' #plot(data$x, data$y, pch = 21, bg = 'lightgrey', main = "Bagging regression with 1 tree", xlab = "x1", ylab = "y")
#' #lines(sort(data$x), pred_single[order(data$x)], col = 'blue')
#' #lines(sort(data$x), sin(2 * pi * sort(data$x)), col = 'red')
#' 
#' # Training and plotting for B = 1000 (Bagging)
#' #models <- bagging_regression(X, Y, B = 1000)
#' #pred_bagging <- bagging_prediction(models, X)
#' #plot(data$x, data$y, pch = 21, bg = 'lightgrey', main = "Bagging regression with 1000 trees", xlab = "x1", ylab = "y")
#' #lines(sort(data$x), pred_bagging[order(data$x)], col = 'blue')
#' #lines(sort(data$x), sin(2 * pi * sort(data$x)), col = 'red')
#' #print_forest_splits(models)
#' # Resetting the plotting area
#' #par(mfrow = c(1, 1))
