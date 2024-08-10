# random sample for regression
create_ran_sample_reg <- function(n, m){
  set.seed(n)
  X1 <- runif(m, 0, 1)
  X2 <- runif(m, 0, 1)  
  e <- rnorm(m, 0, 0.1)
  Y_reg <- X1^2 + X2 + e
  data_reg_li <- list(x = matrix(c(X1, X2), nrow = 2, byrow = TRUE), y = Y_reg)
  return(data_reg_li)
}

#random sample for classification
create_ran_sample_cla <- function(n, m){
  set.seed(n)
  X1 <- runif(m, 0, 1)
  X2 <- runif(m, 0, 1)
  e <- rnorm(m, 0, 0.1)
  linear_comb <- 1.8 * X1 + 0.3 * X2 + e
  Y_cla <- ifelse(linear_comb > 1, 1, 2)
  data_cla_li <- list(x = matrix(c(X1, X2), nrow = 2, byrow = TRUE), y = Y_cla)
  return(data_cla_li)
}

