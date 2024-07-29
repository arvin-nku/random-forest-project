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
random_forest_regression <- function(){
  
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
random_forest_classification <- function(){
  
}   



#' Random Forest Algorithm
#'
#' Random Forest Algorithm for either regression or classification
#'
#'
#'  