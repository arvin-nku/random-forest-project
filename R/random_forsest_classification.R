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
#' 
#'
#' 
#' 
#' 
#'
random_forest_regression <- function(data, B, A = NULL, m = 0, num_leaf = NULL,
                                     depth = NULL, num_split = 2, min_num = 1){
  ##Verification of the input 
  
  #dimension of the data$x
  d <- nrow(data$x)
  #dimension of data$y
  len <- length(data$y)
  
  ##data
  if(d != len){
    stop("Dimension of x and y in data are not equal")
    #stop oder warning?
  }
  
  ##B
  if(as.integer(B)!= B){
    warning("B is not an integer. The value of B is set to", trunc(B))
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
    warning("A is too large, as it must not be larger than the length of y. The value of A is set to ", len)
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
    warning("m is too large, as it must not to be larger than the dimension of x. Set value of m to ", d)
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
    warning("num_leaf is too large, as it must not to be larger than the dimension of y. The value of num_leaf is set to ", len)
    num_leaf <- len
  }
  
  ##depth (hier nur gültge Eingabe genauer Wert dann in greedy_algo bzw default wert
  # das keine Fehlermeldung)
  if(!is.null(depth)){
    if(as.integer(depth) != depth){
      warning("depth is not an integer. The value of depth is set to ", trunc(depth))
      depth <- trunc(depth)
    }
    if(depth < 0){
      warning("depth must be greater tan or equal to 0. The value of depth is set to ", NULL)
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
  ## rest of verification is in greedy_cart 
  
  
  
  
}


data <- list(x=matrix(runif(20,0,1)), y= 1:20)

random_forest_regression(data, B = 1, A = 5, m = 15, num_leaf = 2, depth = 2, 
                         num_split = 3, min_num = 3.3)
data



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
random_forest_classification <- function(){
  
}   



#' Random Forest Algorithm
#'
#' Random Forest Algorithm for either regression or classification
#'
#' d
#'  