test_that("random_forest_classification works with valid input", {
  #Testdata
  set.seed(123)
  X1 <- runif(100, 0, 1)
  X2 <- runif(100, 0, 1)
  e <- rnorm(100, 0, 0.1)
  linear_comb <- 1.8 * X1 + 0.3 * X2 + e
  Y <- ifelse(linear_comb > 1, 1, 2)
  data <- list(x = matrix(c(X1, X2), nrow = 2, byrow = TRUE), y = Y)
  
  #function with valid input
  result <- random_forest_classification(data, B = 10, A = 20, m = 1, num_leaf = 10, depth = NULL, num_split = 2, min_num = 1, unique = FALSE)
  
  #result is list
  expect_type(result, "list")
  
  #length correect
  expect_equal(length(result), 10)
  
  #trees not null
  lapply(result, function(tree) { expect_false(is.null(tree)) })
})

test_that("random_forest_classification throws errors for invalid input", {
  
  #Testdata
  set.seed(123)
  X1 <- runif(100, 0, 1)
  X2 <- runif(100, 0, 1)
  e <- rnorm(100, 0, 0.1)
  linear_comb <- 1.8 * X1 + 0.3 * X2 + e
  Y <- ifelse(linear_comb > 1, 1, 2)
  data <- list(x = matrix(c(X1, X2), nrow = 2, byrow = TRUE), y = Y)
  
  #Invalid input:
  
  #data
  expect_error(random_forest_classification(data = list(x = 1, y = 1), B = 10), "data must be a list")
  
  #B
  expect_error(random_forest_classification(data, B = -1), "Number of bootstrap samples must be greater than or equal to 1")
  
  #A
  expect_warning(random_forest_classification(data, B = 10, A = 200), "A is too large")
  expect_warning(random_forest_classification(data, B = 10, A = -5), "A must be greater 0")
  
  #m
  expect_warning(random_forest_classification(data, B = 10, m = -5), "m must be greater 0")
  
  #num_leaf
  expect_warning(random_forest_classification(data, B = 10, num_leaf = -1), "num_leaf must be greater than or equal to 1")
  
  #num_split
  expect_warning(random_forest_classification(data, B = 10, num_split = 1), "num_split must be greater or equal to 2")
  
  #min_num
  expect_warning(random_forest_classification(data, B = 10, min_num = 0), "min_num must be greater than or equal to 1")
})

test_that("random_forest_classification uses bagging if m and A equal dimension and length", {
  #testdata
  set.seed(123)
  X1 <- runif(100, 0, 1)
  X2 <- runif(100, 0, 1)
  e <- rnorm(100, 0, 0.1)
  linear_comb <- 1.8 * X1 + 0.3 * X2 + e
  Y <- ifelse(linear_comb > 1, 1, 2)
  data <- list(x = matrix(c(X1, X2), nrow = 2, byrow = TRUE), y = Y)
  
  #bagging mode
  expect_warning(random_forest_classification(data, B = 2, A = length(Y), m = nrow(data$x)), 
                 "Bagging is used")
})
