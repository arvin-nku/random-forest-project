library(testthat)
library(dplyr)

# Test that the greedy_cart function works with valid input
test_that("greedy_cart works with valid input", {
  # Test data
  set.seed(123)
  X1 <- runif(100, 0, 1)
  X2 <- runif(100, 0, 1)
  e <- rnorm(100, 0, 0.1)
  Y <- X1^2 + X2 + e
  data_reg_li <- list(x = matrix(c(X1, X2), nrow = 2, byrow = TRUE), y = Y)

  # Run the greedy_cart function with valid input
  result <- greedy_cart(x = c(X1, X2), y = Y, data = data_reg_li, type = "reg",
                        num_leaf = 10, depth = 5, num_split = 2, min_num = 1, m = 1)

  # Check if result is an environment
  expect_type(result, "environment")

  # Check if the tree structure is returned as expected
  expect_true(is.list(result$tree))

  # Check if the tree is not empty
  expect_gt(nrow(result$tree), 0)

  # Check if the tree has a root node
  expect_equal(result$tree$name[1], "root")

  # Check if the number of leaves is correct
  expect_lte(nrow(filter(result$tree, name == "leaf")), 10)

  # Check if the depth does not exceed the specified limit
  max_depth <- max(floor(log2(result$tree$node)))
  expect_lte(max_depth, 5)
})

#Test that greedy_cart generates different trees with varying parameters
test_that("greedy_cart generates different trees with varying parameters", {
  # Test data
  set.seed(123)
  X1 <- runif(100, 0, 1)
  X2 <- runif(100, 0, 1)
  e <- rnorm(100, 0, 0.1)
  Y <- X1^2 + X2 + e
  data_reg_li <- list(x = matrix(c(X1, X2), nrow = 2, byrow = TRUE), y = Y)
  
  # Generate trees with different num_leaf parameters
  tree1 <- greedy_cart(x = c(X1, X2), y = Y, data = data_reg_li, type = "reg", num_leaf = 10)
  tree2 <- greedy_cart(x = c(X1, X2), y = Y, data = data_reg_li, type = "reg", num_leaf = 20)
  
  # Check if the structures are different
  expect_false(identical(tree1$tree, tree2$tree))
  
  # Generate trees with different depth parameters
  tree3 <- greedy_cart(x = c(X1, X2), y = Y, data = data_reg_li, type = "reg", depth = 3)
  tree4 <- greedy_cart(x = c(X1, X2), y = Y, data = data_reg_li, type = "reg", depth = 5)
  
  # Check if the structures are different
  expect_false(identical(tree3$tree, tree4$tree))
})

#Test that the greedy_cart function produces expected tree structure with simple data
test_that("greedy_cart creates valid tree structure", {
  # Test data
  X <- c(0.1, 0.2, 0.8, 0.9)
  Y <- c(1, 2, 2, 3)
  data_reg_li <- list(x = matrix(X, nrow = 1), y = Y)
  # Generate tree with specific parameters
  result <- greedy_cart(x = X, y = Y, data = data_reg_li, type = "reg",
                        num_leaf = 2, depth = 2, num_split = 2, min_num = 1, m = 1)
  # Check if tree has the correct number of nodes
  expect_gt(nrow(result$tree), 0)
  # Check if tree has exactly 2 leaf nodes
  expect_equal(nrow(filter(result$tree, name == "leaf")), 2)
  # Check if the tree has a root node
  expect_equal(result$tree$name[1], "root")
})


# Test that greedy_cart throws warnings for invalid input
test_that("greedy_cart throws warnings for invalid input", {
  # Test data
  set.seed(123)
  X1 <- runif(100, 0, 1)
  X2 <- runif(100, 0, 1)
  e <- rnorm(100, 0, 0.1)
  Y <- X1^2 + X2 + e
  data_reg <- tibble(x1 = X1, x2 = X2, y = Y)
  
  # Test invalid num_leaf
  expect_warning(greedy_cart(x = c(x1, x2), y = y, data = data_reg, type = "reg",
                             num_leaf = -1), regexp = "num_leaf must be greater than or equal to 1")
  
  # Test invalid depth
  expect_warning(greedy_cart(x = c(x1, x2), y = y, data = data_reg, type = "reg",
                             depth = -1), regexp = "depth must be greater than or equal to 0")
  
  # Test invalid num_split
  expect_warning(greedy_cart(x = c(x1, x2), y = y, data = data_reg, type = "reg",
                             num_split = 1), regexp = "num_split must be greater than or equal to 2")
  
  # Test invalid min_num
  expect_warning(greedy_cart(x = c(x1, x2), y = y, data = data_reg, type = "reg",
                             min_num = 0), regexp = "min_num must be greater than or equal to 1")
  
  # Test invalid m
  expect_warning(greedy_cart(x = c(x1, x2), y = y, data = data_reg, type = "reg",
                             m = -1), regexp = "m must be greater than 0")
  
  # Check that the function returns a valid tree even when warnings are issued
  result <- greedy_cart(x = c(x1, x2), y = y, data = data_reg, type = "reg",
                        num_leaf = -1, depth = -1, num_split = 1, min_num = 0, m = -1)
  
  expect_type(result, "environment")
  expect_true(is.list(result$tree))
  expect_gt(nrow(result$tree), 0)
  
  print("Test passed")
})
