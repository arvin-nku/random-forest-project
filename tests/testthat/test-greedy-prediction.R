library(testthat)

# Test that prediction function works with a greedy_cart tree (regression)
test_that("prediction works with a greedy_cart tree for regression", {
  # Test data
  set.seed(123)
  X <- runif(100, 0, 1)
  e <- rnorm(100, 0, 0.1)
  Y <- X^2 + e
  data_reg_li <- list(a = X, b = Y)
  
  # Generate a tree using greedy_cart
  greedy_tree <- greedy_cart(x = a, y = b, data = data_reg_li, type = "reg",
                             num_leaf = 10, depth = 5, num_split = 2, min_num = 1, m = 1)
  
  # Example prediction input
  list_x <- matrix(c(0.1, 0.4, 0.6, 0.8, 0.9), nrow = 1)
  
  # Perform predictions using the generated tree
  predictions <- prediction(list(greedy_tree$tree), list_x, type = "reg")
  
  # Check if predictions are numeric
  expect_type(predictions, "double")
  
  # Check if predictions have the correct length
  expect_equal(length(predictions), ncol(list_x))
})

# Test that prediction function works with a greedy_cart tree (classification)
test_that("prediction works with a greedy_cart tree for classification", {
  # Test data
  set.seed(123)
  X1 <- runif(200, 0, 1)
  X2 <- runif(200, 0, 1)
  e <- rnorm(200, 0, 0.05)
  k <- function(x, y) (x - 0.5) * (y - 0.5)
  g <- function(x, y, e) {
    Y <- c()
    for(i in seq_along(x)) {
      if(k(X1[i], X2[i]) - e[i] <= 0) {
        Y[i] <- 1
      } else {
        Y[i] <- 2
      }
    }
    Y
  }
  Y <- g(X1, X2, e)
  data_class_li <- list(x1 = X1, x2 = X2, y = Y)
  
  # Generate a tree using greedy_cart
  greedy_tree <- greedy_cart(x = c(x1, x2), y = y, data = data_class_li, type = "class",
                             num_leaf = 10, depth = 5, num_split = 2, min_num = 1, m = 1)
  
  # Example prediction input
  list_x <- matrix(c(0.1, 0.4, 0.6, 0.8, 0.9), nrow = 2)
  
  # Perform predictions using the generated tree
  predictions <- prediction(list(greedy_tree$tree), list_x, type = "cla")
  
  # Check if predictions are integer (class labels)
  expect_type(predictions, "integer")
  
  # Check if predictions have the correct length
  expect_equal(length(predictions), ncol(list_x))
})

# Test prediction function with invalid inputs (regression)
test_that("prediction function handles invalid inputs for regression", {
  # Test data
  set.seed(123)
  X <- runif(100, 0, 1)
  e <- rnorm(100, 0, 0.1)
  Y <- X^2 + e
  data_reg_li <- list(a = X, b = Y)
  
  # Generate a tree using greedy_cart
  greedy_tree <- greedy_cart(x = a, y = b, data = data_reg_li, type = "reg",
                             num_leaf = 10, depth = 5, num_split = 2, min_num = 1, m = 1)
  
  # Example prediction input
  list_x <- matrix(c(0.1, 0.4, 0.6, 0.8, 0.9), nrow = 1)
  
  # Test invalid inputs
  expect_error(prediction(list_tree = NULL, list_x = list_x, type = "reg"))
  expect_error(prediction(list_tree = list(greedy_tree$tree), list_x = NULL, type = "reg"))
  expect_error(prediction(list_tree = list(greedy_tree$tree), list_x = list_x, type = NULL))
  expect_error(prediction(list_tree = list(greedy_tree$tree), list_x = list_x, type = "invalid_type"))
})

# Test prediction function with invalid inputs (classification)
test_that("prediction function handles invalid inputs for classification", {
  # Test data
  set.seed(123)
  X1 <- runif(200, 0, 1)
  X2 <- runif(200, 0, 1)
  e <- rnorm(200, 0, 0.05)
  k <- function(x, y) (x - 0.5) * (y - 0.5)
  g <- function(x, y, e) {
    Y <- c()
    for(i in seq_along(x)) {
      if(k(X1[i], X2[i]) - e[i] <= 0) {
        Y[i] <- 1
      } else {
        Y[i] <- 2
      }
    }
    Y
  }
  Y <- g(X1, X2, e)
  data_class_li <- list(x1 = X1, x2 = X2, y = Y)
  
  # Generate a tree using greedy_cart
  greedy_tree <- greedy_cart(x = c(x1, x2), y = y, data = data_class_li, type = "class",
                             num_leaf = 10, depth = 5, num_split = 2, min_num = 1, m = 1)
  
  # Example prediction input
  list_x <- matrix(c(0.1, 0.4, 0.6, 0.8, 0.9), nrow = 2)
  
  # Test invalid inputs
  expect_error(prediction(list_tree = NULL, list_x = list_x, type = "cla"))
  expect_error(prediction(list_tree = list(greedy_tree$tree), list_x = NULL, type = "cla"))
  expect_error(prediction(list_tree = list(greedy_tree$tree), list_x = list_x, type = NULL))
  expect_error(prediction(list_tree = list(greedy_tree$tree), list_x = list_x, type = "invalid_type"))
})
