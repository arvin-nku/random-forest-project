test_that("random_forest_regression works with valid input", {
  
  #Testdata
  set.seed(123)
  X1 <- runif(100, 0, 1)
  X2 <- runif(100, 0, 1)
  e <- rnorm(100, 0, 0.1)
  Y <- X1^2 + X2 + e
  
  data <- list(x = matrix(c(X1, X2), nrow = 2, byrow = TRUE), y = Y)
  
  #function with valid input
  result <- random_forest_regression(data, B = 10, A = 20, m = 1, num_leaf = 10, depth = NULL, num_split = 2, min_num = 1)
  
  #check result list
  expect_type(result, "list")
  
  #check list length correct
  expect_equal(length(result), 10)
  
  #every tree not null
 lapply(result, function(tree){ expect_false(is.null(tree))})
 
})

test_that("random_forest_regression throws errors for invalid input", {
  
  #Testdata
  set.seed(123)
  X1 <- runif(100, 0, 1)
  X2 <- runif(100, 0, 1)
  e <- rnorm(100, 0, 0.1)
  Y <- X1^2 + X2 + e
  
  data <- list(x = matrix(c(X1, X2), nrow = 2, byrow = TRUE), y = Y)
  
  #Invalid input:
  
  #B
  expect_error(random_forest_regression(data, B = -1, A = 2), "Number of bootstrap samples must be greater than or equal to 1")
  
  #A
  expect_warning(random_forest_regression(data, B = 10, A = 200, m = 1), "A is too large")
  expect_warning(random_forest_regression(data, B = 5, A = -5, m = 1), "A must be greater 0")
  
  #m
  expect_warning(random_forest_regression(data, B = 5, A = 2, m = -5), "m must be greater 0")
  
  #num_leaf
  expect_warning(random_forest_regression(data, B = 5, A = 2, num_leaf = -1), "num_leaf must be greater than or equal to 1")
  
  #num_split
  expect_warning(random_forest_regression(data, B = 5, A = 2, num_split = 1), "num_split must be greater or equal to 2")
  
  #min_num
  expect_warning(random_forest_regression(data, B = 5, A = 2, min_num = 0), "min_num must be greater than or equal to 1")
})

test_that("random_forest_regression uses bagging if m and A equal dimension and length", {
  
  #Testdata
  set.seed(123)
  X1 <- runif(100, 0, 1)
  X2 <- runif(100, 0, 1)
  e <- rnorm(100, 0, 0.1)
  Y <- X1^2 + X2 + e
  data <- list(x = matrix(c(X1, X2), nrow = 2, byrow = TRUE), y = Y)
  
  #bagging mode
  expect_warning(random_forest_regression(data, B = 2, A = length(Y), m = nrow(data$x)), 
                 "Bagging is used. To use Random Forest, enter a value less than 2 for m or a value less than 100 for A")
})

