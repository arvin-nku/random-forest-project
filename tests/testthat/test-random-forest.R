test_that("random_forest works with valid input", {

  #Testdata
  set.seed(123)
  X1 <- runif(100, 0, 1)
  X2 <- runif(100, 0, 1)
  e <- rnorm(100, 0, 0.1)
  Y_reg <- X1^2 + X2 + e
  data_reg <- list(x = cbind(X1, X2), y = Y_reg)
  linear_comb <- 1.8 * X1 + 0.3 * X2 + e
  Y_cla <- ifelse(linear_comb > 1, 1, 2)
  data_cla <- list(a = X1, b = X2, y = Y_cla)

  #reg
  result_reg <- random_forest(x = c(X1,X2), y = Y_reg, data = data_reg, type = "reg", B = 10, A = 20, m = 1, num_leaf = 5, depth = NULL, num_split = 3, min_num = 2)

  #list
  expect_type(result_reg, "list")

  #length
  expect_length(result_reg, 10)

  #trees not NULL
  lapply(result_reg, function(tree) { expect_false(is.null(tree)) })

  #cla
  result_cla <- random_forest(x = c(X1,X2), y = Y_cla, data = data_cla, type = "cla", B = 10, A = 20, m = 1, num_leaf = 5, depth = NULL, num_split = 3, min_num = 2, unique = TRUE)

  #list
  expect_type(result_cla, "list")

  #length
  expect_length(result_cla, 10)

  #trees not NULL
  lapply(result_cla, function(tree) { expect_false(is.null(tree)) })
})

test_that("random_forest automatically determines type", {

  #Testdata
  set.seed(123)
  X1 <- runif(100, 0, 1)
  X2 <- runif(100, 0, 1)
  e <- rnorm(100, 0, 0.1)
  Y_reg <- X1^2 + X2 + e
  linear_comb <- 1.8 * X1 + 0.3 * X2 + e
  data_reg <- list(x = cbind(X1, X2), y = Y_reg)
  Y_cla <- ifelse(linear_comb > 1, 1, 2)
  data_cla <- list(a = X1, b = X2, y = Y_cla)


  #reg auto type
  expect_warning(result_auto_reg <- result_reg_auto <- random_forest(x = c(X1,X2), y = Y_reg, data = data_reg, B = 5, m = 1),"Type was forgotten to specify and was automatically set to regression")

  #list
  expect_type(result_auto_reg, "list")

  #B
  expect_length(result_auto_reg, 5)

  #treees not NULL
  lapply(result_auto_reg, function(tree) { expect_false(is.null(tree)) })

  # Automatische Typbestimmung für Klassifikation
  expect_warning(result_auto_cla <- random_forest(x = c(X1,X2), y = Y_cla, data = data_cla, B = 5, m = 1),"Type was forgotten to specify and was automatically set to classification")

  #list
  expect_type(result_auto_cla, "list")

  #B
  expect_length(result_auto_cla, 5)

  #trees not NULL
  lapply(result_auto_cla, function(tree) { expect_false(is.null(tree)) })
})