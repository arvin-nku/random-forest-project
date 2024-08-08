#' Random Forest Algorithm
#' 
#' Random Forest Algorithm for regression
#' 
#' @param data a named list with regression data\cr 
#' the x-values have the name x and are in the form of a matrix\cr
#' the row-number indicating the dimension of the data\cr
#' the y-values have name y and are in the form of a vector\cr
#' the row-number must be the same as that of x
#' @param B number of bootstrap samples to be created
#' @param A sample size to be used\cr
#' must be greater than 0 and less than or equal to the number of observations\cr
#' the default value corresponds to the total size of data
#' @param m positive number of coordinates used in each iteration\cr
#' the default value is the dimension of the data
#' @param num_leaf Termination condition: The tree has `num_leaf` leaves.\cr
#' Must be greater than or equal to 1\cr
#' The default value is the number of data-points
#' @param depth Termination condition: The tree has depth `depth`\cr
#' The default value is the maximum achievable depth
#' @param num_split only split nodes that contain at least `num_split` elements\cr
#' must be greater than or equal to 2
#' @param min_num only divides a nod if both child nodes at least `min_num` elements\cr
#' must be greater than or equal to 1
#' 
#' @return a list of trees `B` 
#' @export
#' 
#' @examples
#' X1 <- runif(100, 0, 1)
#' X2 <- runif(100, 0, 1)
#' X3 <- runif(100, 0, 1)
#' X4 <- runif(100, 0, 1)
#' Y <- 2 * X1 + 3 * X2 + 3 * X3 + 4 * X4
#' data <- list(x = matrix(c(X1, X2, X3, X4), nrow = 4, byrow = TRUE), y = Y)
#' random_forest_regression(data, B = 10, A = 20, m = 3, num_leaf = 10, depth = NULL, num_split = 2, min_num = 1)


random_forest_regression <- function(data, B, A = NULL, m = 0, num_leaf = NULL,
                                     depth = NULL, num_split = 2, min_num = 1){
  ##Verification of the input 
  
  #dimension of the data$x
  d <- nrow(data$x)
  n <- ncol(data$x)
  #dimension of data$y
  len <- length(data$y)
  
  ##data
  if(!is.list(data)){
    stop("data must be a list")
  }
  if(n != len){
    stop("Dimension of x and y in data are not equal")
  }
  
  ##B
  if(as.integer(B)!= B){
    warning("B is not an integer. The value of B is set to ", trunc(B))
    B <- trunc(B)
  }
  if(B < 1){
    stop("Number of bootstrap samples must be greater than or equal to 1")
  }
  
  ##A
  if(is.null(A)){
    A <- len
  }
  if(as.integer(A) != A){
    warning("A is not an integer. The value of A is set to ", trunc(A))
    A <- trunc(A)
  }
  if(A <= 0){
    warning("A must be greater 0. The value of A is set to ", len)
    A <- len
  }
  if(A > len){
    warning("A is too large, as it must not be larger than the length of y (", len,"). The value of A is set to ", len)
    A <- len
  }
  
  ##m
  if(missing(m)){
    m <- d
  }
  if(as.integer(m) != m){
    warning("m is not a integer. The value of m is set to ", trunc(m))
    m <- trunc(m)
  }
  if(m <= 0){
    warning("m must be greater 0. The value of m is set to ", d)
    m <- d
  }
  if(m > d){
    warning("m is too large, as it must not to be larger than the dimension of x (", d,"). Set value of m to ", d)
    m <- d
  }
  
  ##num_leaf
  if(is.null(num_leaf)){
    num_leaf <- len
  }
  if(as.integer(num_leaf) != num_leaf){
    warning("num_leaf is not a integer. The value of num_leaf is set to ", trunc(num_leaf))
    num_leaf <- trunc(num_leaf)
  }
  if(num_leaf < 1){
    warning("num_leaf must be greater than or equal to 1. The value of num_leaf is set to ", len)
    num_leaf <- len
  }
  if(num_leaf > len){
    warning("num_leaf is too large, as it must not to be larger than the dimension of y (", len, "). The value of num_leaf is set to ", len)
    num_leaf <- len
  }
  
  ##depth (check only valid input check more precisely greedy_cart)
  if(!is.null(depth)){
    if(as.integer(depth) != depth){
      warning("depth is not an integer. The value of depth is set to ", trunc(depth))
      depth <- trunc(depth)
    }
    if(depth < 0){
      warning("depth must be greater tan or equal to 0. The value of depth is set to NULL")
      depth <- NULL
    }
  }
  
  ##num_split
  if(as.integer(num_split) != num_split){
    warning("num_split is not an integer. The value of num_split is set to ", trunc(num_split))
    num_split <- trunc(num_split)
  }
  if(num_split < 2){
    warning("num_split must be greater or equal to 2. The value of num_split is set to ", 2)
    num_split <- 2
  }
  
  ##min_num
  if(as.integer(min_num) != min_num){
    warning("min_num is not an integer. The value of min_num is set to ", trunc(min_num))
    min_num <- trunc(min_num)
  }
  if(min_num < 1){
    warning("min_num must be greater than or equal to 1. The value of min_num is set to ", 1)
    min_num <- 1
  }
  
  ##verification not equivalent to bagging
  if(m == d & A == len){
    warning("Bagging is used. To use Random Forest, enter a value less than ", d, " for m or a value less than ", len, " for A")
  }
  ## rest of verification is in greedy_cart 
  
  #initialization of a list for saving the B decision trees
  tree <- vector("list", B)
  
  for (i in 1:B) {
    #take random samples s (two different cases)
    #A<len replace = F
    if(A < len){
      s <- sample(1:len, A)
    }
    #replace = T
    else{
      s <- sample(1:len, len, replace = T)
    }
    
    #data of the samples taken
    s_data_x <- data$x[ , s, drop = F] #drop = F ensures that we have a matrix
    s_data_y <- data$y[s]
    s_data_x_y <- list(x = s_data_x, y = s_data_y)
    
    tree[[i]] <- greedy_cart_regression(s_data_x_y, num_leaf = num_leaf,
                                        depth = depth, num_split = num_split, 
                                        min_num = min_num, m = m)$tree
    
  }
  
  return(tree)
  
}


#' Random Forest Algorithm for classification
#' 
#' @param data a named list with classification data\cr 
#' the x-values have the name x and are in the form of a matrix\cr
#' the row-number indicating the dimension of the data\cr
#' the y-values have name y and are in the form of a vector\cr
#' the row-number must be the same as that of x
#' @param B number of bootstrap samples to be created
#' @param A sample size to be used\cr
#' must be greater than 0 and less than or equal to the number of observations\cr
#' the default value corresponds to the total size of data
#' @param m positive number of coordinates used in each iteration\cr
#' the default value is the dimension of the data
#' @param num_leaf Termination condition: The tree has `num_leaf` leaves.\cr
#' Must be greater than or equal to 1\cr
#' The default value is the number of data-points
#' @param depth Termination condition: The tree has depth `depth`\cr
#' The default value is the maximum achievable depth
#' @param num_split only split nodes that contain at least `num_split` elements\cr
#' must be greater than or equal to 2
#' @param min_num only divides a nod if both child nodes at least `min_num` elements\cr
#' must be greater than or equal to 1  
#' @param unique if `unique`is set to TRUE, nodes where all data points in this node have the the class are not split\cr
#' The default value is FALSE  
#' 
#' 
#' @return a list of trees `B` 
#' @export
#' 
#' @examples
#' X1 <- runif(100, 0, 1)
#' X2 <- runif(100, 0, 1)
#' X3 <- runif(100, 0, 1)
#' X4 <- runif(100, 0, 1)
#' Y <- ifelse(2 * X1 + 3 * X2 + 3 * X3 + 4 * X4 > 2.5, 1, 2)
#' data <- list(x = matrix(c(X1, X2, X3, X4), nrow = 4, byrow = TRUE), y = Y)
#' random_forest_classification(data, B = 10, A = 20, m = 3, num_leaf = 10, depth = NULL, num_split = 2, min_num = 1, unique = F)


random_forest_classification <- function(data, B, A = NULL, m = 0, num_leaf = NULL, 
                                         depth = NULL, num_split = 2, min_num = 1, 
                                         unique = F ){
  
  ##Verification of the input 
  
  #dimension of the data$x
  d <- nrow(data$x)
  n <- ncol(data$x)
  #dimension of data$y
  len <- length(data$y)
  
  ##data
  if(!is.list(data)){
    stop("data must be a list")
  }
  if(n != len){
    stop("Dimension of x and y in data are not equal")
  }
  
  ##B
  if(as.integer(B)!= B){
    warning("B is not an integer. The value of B is set to ", trunc(B))
    B <- trunc(B)
  }
  if(B < 1){
    stop("Number of bootstrap samples must be greater than or equal to 1")
  }
  
  ##A
  if(is.null(A)){
    A <- len
  }
  if(as.integer(A) != A){
    warning("A is not an integer. The value of A is set to ", trunc(A))
    A <- trunc(A)
  }
  if(A <= 0){
    warning("A must be greater 0. The value of A is set to ", len)
    A <- len
  }
  if(A > len){
    warning("A is too large, as it must not be larger than the length of y (", len,"). The value of A is set to ", len)
    A <- len
  }
  
  ##m
  if(missing(m)){
    m <- d
  }
  if(as.integer(m) != m){
    warning("m is not a integer. The value of m is set to ", trunc(m))
    m <- trunc(m)
  }
  if(m <= 0){
    warning("m must be greater 0. The value of m is set to ", d)
    m <- d
  }
  if(m > d){
    warning("m is too large, as it must not to be larger than the dimension of x (", d,"). Set value of m to ", d)
    m <- d
  }
  
  ##num_leaf
  if(is.null(num_leaf)){
    num_leaf <- len
  }
  if(as.integer(num_leaf) != num_leaf){
    warning("num_leaf is not a integer. The value of num_leaf is set to ", trunc(num_leaf))
    num_leaf <- trunc(num_leaf)
  }
  if(num_leaf < 1){
    warning("num_leaf must be greater than or equal to 1. The value of num_leaf is set to ", len)
    num_leaf <- len
  }
  if(num_leaf > len){
    warning("num_leaf is too large, as it must not to be larger than the dimension of y (", len, "). The value of num_leaf is set to ", len)
    num_leaf <- len
  }
  
  ##depth (check only valid input check more precisely greedy_cart)
  if(!is.null(depth)){
    if(as.integer(depth) != depth){
      warning("depth is not an integer. The value of depth is set to ", trunc(depth))
      depth <- trunc(depth)
    }
    if(depth < 0){
      warning("depth must be greater than or equal to 0. The value of depth is set to NULL")
      depth <- NULL
    }
  }
  
  ##num_split
  if(as.integer(num_split) != num_split){
    warning("num_split is not an integer. The value of num_split is set to ", trunc(num_split))
    num_split <- trunc(num_split)
  }
  if(num_split < 2){
    warning("num_split must be greater or equal to 2. The value of num_split is set to ", 2)
    num_split <- 2
  }
  
  ##min_num
  if(as.integer(min_num) != min_num){
    warning("min_num is not an integer. The value of min_num is set to ", trunc(min_num))
    min_num <- trunc(min_num)
  }
  if(min_num < 1){
    warning("min_num must be greater than or equal to 1. The value of min_num is set to ", 1)
    min_num <- 1
  }
  
  ##unique
  if(!is.logical(unique)){
    warning("unique is not logical. The value of unique is set to FALSE")
    unique <- F
  }
  
  ##verification not equivalent to bagging
  if(m == d & A == len){
    warning("Bagging is used. To use Random Forest, enter a value less than ", d, " for m or a value less than ", len, " for A")
  }
  
  ## rest of verification is in greedy_cart
  #initialization of a list for saving the B decision trees
  tree <- vector("list", B)
  
  for (i in 1:B) {
    #take random samples s (two different cases)
    #A<len replace = F
    if(A < len){
      s <- sample(1:len, A)
    }
    #replace = T
    else{
      s <- sample(1:len, len, replace = T)
    }
    
    #data of the samples taken
    s_data_x <- data$x[ , s, drop = F] #drop = F ensures that we have a matrix
    s_data_y <- data$y[s]
    s_data_x_y <- list(x = s_data_x, y = s_data_y)
    
    tree[[i]] <- greedy_cart_classification(s_data_x_y, num_leaf = num_leaf,
                                            depth = depth, num_split = num_split, 
                                            min_num = min_num, m = m, unique = unique)$tree
    
  }
  
  return(tree)
  
}


#' Random Forest Algorithm
#'
#' Random Forest Algorithm for either regression or classification
#'
#' @param x column name or list of column names of the input variable(s) 
#' @param y column/list name of the target variable \cr
#' one dimensional vector
#' @param type “reg” for regression tree\cr
#' “cla” for classification tree\cr 
#' if `type` is missing, the function tries to guess the correct type#
#' @param data named list or tibble containing the combined data of x and y
#' @param B number of bootstrap samples to be created
#' @param A sample size to be used\cr
#' must be greater than 0 and less than or equal to the number of observations\cr
#' the default value corresponds to the total size of data
#' @param m positive number of coordinates used in each iteration\cr
#' the default value is the dimension of the data
#' @param num_leaf Termination condition: The tree has `num_leaf` leaves.\cr
#' Must be greater than or equal to 1\cr
#' The default value is the number of data-points
#' @param depth Termination condition: The tree has depth `depth`\cr
#' The default value is the maximum achievable depth
#' @param num_split only split nodes that contain at least `num_split` elements\cr
#' must be greater than or equal to 2
#' @param min_num only divides a nod if both child nodes at least `min_num` elements\cr
#' must be greater than or equal to 1  
#' @param unique if `unique`is set to TRUE, nodes where all data points in this node have the the class are not split\cr
#' The default value is FALSE
#' 
#' @return a list of `B` trees
#' 
#' @export
#' 
#' @examples 
#' X1 <- runif(100, 0, 1)
#' X2 <- runif(100, 0, 1)
#' X3 <- runif(100, 0, 1)
#' X4 <- runif(100, 0, 1)
#' Y <-  X1 + X2 + X3 + X4
#' data_reg <- tibble(a = X1, b = X2, c = X3, d = X4, y = Y)
#' random_forest(x = c(a, b, c, d), y = Y, data = data_reg, type = "reg", B = 10, A = 20, m = 3, num_leaf = 10, depth = NULL, num_split = 2, min_num = 1, unique = F)
#'
#'
#' X1 <- runif(100, 0, 1)
#' X2 <- runif(100, 0, 1)
#' X3 <- runif(100, 0, 1)
#' X4 <- runif(100, 0, 1)
#' Y <- ifelse(X1 + X2 + X3 + X4 > 2.5, 1, 2)
#' data_cla <- list(a = X1, b = X2, c = X3, d = X4, y = Y)
#' random_forest(x = c(a, b, c, d), y = Y, data = data_cla, type = "cla", B = 10, A = 20, m = 3, num_leaf = 10, depth = NULL, num_split = 2, min_num = 1, unique = F)


random_forest <- function(x, y, data, type = NULL, B, A = NULL, m = 0,num_leaf = NULL, 
                          depth = NULL, num_split = 2, min_num = 1, unique = F ){
  
  #check that x and y is extracted correctly from data
  x1 <- enquo(x)
  y1 <- enquo(y)
  
  data_x <- eval_tidy(x1, data)
  data_y <- eval_tidy(y1, data)
  
  #verification of the input
  
  ##x
  if(!is.numeric(data_x)){
    stop("The data of x must be numeric")
  }
  
  ##y
  if(!is.numeric(data_y)){
    stop("The data of y must be numeric")
  }
  if(!NCOL(data_y) == 1){
    stop("The dimension of y must be 1")
  }
  
  ##combination of x and y is permitted
  if(!(as.integer((length(data_x)/length(data_y)))*length(data_y) == length(data_x))){
    stop("The length of x and y is not compatible")
  }
  
  ##class of data correct
  if(!is.list(data)){
    stop("data must be a list")
  }
  
  #rest of verification is in random_forest_regression/classification
  
  #bringing data into the right form
  nrow_x <- length(data_x)/length(data_y)
  mat_x <- matrix(data_x, nrow = nrow_x, byrow = T)
  data_x_y <- list(x = mat_x, y = data_y)
  
  #try to guess the type if type == NULL
  if(is.null(type)){
    y_int <- as.integer(data_y)
    if(all(y_int == data_y) & all(y_int >= 1)){
      warning("Type was forgotten to specify and was automatically set to classification")
      type <- "cla"
    }
    else{
      warning("Type was forgotten to specify and was automatically set to regression")
      type <- "reg"
    }
  }
  
  if(type == "reg"){
    return(random_forest_regression(data = data_x_y, B = B, A = A, m = m,
                                    num_leaf = num_leaf, depth = depth,
                                    num_split = num_split, min_num = min_num))
  }
  if(type == "cla"){
    return(random_forest_classification(data = data_x_y, B = B, A = A, m = m,
                                        num_leaf = num_leaf, depth = depth,
                                        num_split = num_split, min_num = min_num,
                                        unique = unique))
  }
  else{
    stop("Invalid type: The type must be reg for regression or cla for classification")
  }
}

### Beispiel und Anschauung

library(rlang)
library(dplyr)

X1 <- runif(100, 0, 1)
X2 <- runif(100, 0, 1)
X3 <- runif(100, 0, 1)
X4 <- runif(100, 0, 1)
Y <-  X1 + X2 + X3 + X4
data_reg <- tibble(a = X1, b = X2, c = X3, d = X4, y = Y)
random_forest(x = c(a, b, c, d), y = Y, data = data_reg, type = "reg", B = 10, A = 20, m = 3, num_leaf = 10, depth = NULL, num_split = 2, min_num = 1, unique = F)
rand_reg <- random_forest(x = c(a, b, c, d), y = Y, data = data_reg, type = "reg", B = 10, A = 20, m = 3, num_leaf = 10, depth = NULL, num_split = 2, min_num = 1, unique = F)

# Funktion, um die Splits eines einzelnen Baums zu drucken
print_tree_splits <- function(node, depth = 0) {
  indent <- paste(rep("  ", depth), collapse = "")
  
  if (is.null(node$left) && is.null(node$right)) {
    # Wenn der Knoten ein Blatt ist
    cat(indent, "Leaf: Predict", node$prediction, "\n")
  } else {
    # Wenn der Knoten ein innerer Knoten ist
    #cat(indent, "Node: ","\n")
    
    # Ausgabe für den linken Teilbaum
    if (!is.null(node$left)) {
      cat(indent, "Left subtree: (Split val =", node$value, ")\n")
      print_tree_splits(node$left, depth + 1)
    }
    
    # Ausgabe für den rechten Teilbaum
    if (!is.null(node$right)) {
      cat(indent, "Right subtree: (Split val =", node$value, ")\n")
      print_tree_splits(node$right, depth + 1)
    }
    

  }
}


# Funktion, um die Splits aller Bäume im Random Forest zu drucken
print_forest_splits <- function(forest) {
  for (i in seq_along(forest)) {
    cat("Tree", i, ":\n")
    print_tree_splits(forest[[i]])
    cat("\n")
  }
}

# Beispielnutzung mit dem vorher erstellten Wald
print_forest_splits(rand_reg)

X1 <- runif(100, 0, 1)
X2 <- runif(100, 0, 1)
X3 <- runif(100, 0, 1)
X4 <- runif(100, 0, 1)
Y <- ifelse(X1 + X2 + X3 + X4 > 2.5, 1, 2)
data_cla <- list(a = X1, b = X2, c = X3, d = X4, y = Y)
random_forest(x = c(a, b, c, d), y = Y, data = data_cla, type = "cla", B = 10, A = 20, m = 3, num_leaf = 10, depth = NULL, num_split = 2, min_num = 1, unique = F)
rand_cla <- random_forest(x = c(a, b, c, d), y = Y, data = data_cla, type = "cla", B = 10, A = 20, m = 3, num_leaf = 10, depth = NULL, num_split = 2, min_num = 1, unique = F)

print_forest_splits(rand_cla)



