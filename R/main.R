library(dplyr)
library(rlang)

#' Prediction of datapoint or a set of data points in a tree or a set of trees
#'
#' @param list_tree a list containing an arbitrary number greater or equal to 1 of trees in tibble form\cr 
#' as used by `greedy_cart()`, `bagging()` or `random_forest()`
#' @param list_x a matrix of values to be predicted, where each column corresponds to a datapoint \cr
#' the number of rows corresponds to the dimension of the x-values in the list/matrix
#' @param type “reg” for regression tree\cr
#' “cla” for classification tree\cr
#' 
#' @return 
#' @export
#' @example
#' X1 <- runif(100, 0, 1)
#' X2 <- runif(100, 0, 1)
#' e <- rnorm(50, 0, 0.1)
#' Y <- X1^2 + X2 + e
#' data_reg <- tibble(a = X1, b = X2, y = Y) 
#' list_tree <- random_forest(x = X, y = Y, data = data_reg, type = "reg", B = 5)
#' list_x <- matrix(c(0.1, 0.3, 0.5, 0.7, 0.8, 0.9), ncol = 6)
#' predictions <- prediction(list_tree, list_x, type = "reg")


prediction <- function(list_tree, list_x, type = NULL){
  
  #Verification of the input
  
  if(!is.list(list_tree)) {
    stop("The input of list_tree must be a list")
  }
  
  if(!is.matrix(list_x)){
    stop("The input of list_x must be a matrix")
  }
  
  if(is.null(type)){
    stop("The type is not set! Set the type reg for regression or cla for classification.")
  }
  
  if(length(list_tree[[1]][[1]][[1]]) != nrow(list_x)){
    stop("The dimension of list_tree is not equal to the dimension of list_x")
  }

  #helpfunctions
  #no leaf-node
  no_leaf <- function(tree, node_number){
    node_number_tree <- tree$A[tree$node == node_number]
    if(length(node_number_tree[[1]]) > 1){
      return(T)
    }
    return(F)
  }
  
  #node exists
  exists_node <- function(tree, node_number){
    for(i in 1:length(tree$node)){
      if(node_number == tree$node[[i]]){
        return(T)
      }
    }
    return(F)
  }
  
  #predictions with a tree
  pred <- function(tree, x){
    #start-node for prediction
    node_cur <- tree$node[tree$name == "root"]
    
    #find leaf node
    while(no_leaf(tree, node_cur)){
      #left and right child nodes
      left_node <- node_cur * 2
      right_node <- node_cur * 2 + 1
      
      #node has a left child
      if(exists_node(tree, left_node)){
        dim_cur <- tree$split_index[tree$node == left_node]
        if(x[dim_cur] < tree$split_point[tree$node == left_node]){
          node_cur <- left_node  #move to the left child
        } else {
          node_cur <- right_node  #move to the right child
        }
      } else {
        #break if there is no child
        break
      }
    }
    
    # Return the prediction value of the leaf node
    return(tree$value[tree$node == node_cur])
  }
  
  #predict a single value using all trees
  predict_single_value <- function(x_col, list_tree, type) {
    #predictions for the current column
    y_list <- sapply(list_tree, function(tree) pred(tree, x_col))
    
    #compute the final value based on the type
    if (type == "reg") {
      print(y_list)
      return(mean(y_list, na.rm = TRUE))
    }
    if (type == "class") {
      return(as.integer(tail(names(sort(table(y_list))), 1)))
    }
  }
  
  #predictions for all columns in list_x
  y_p <- sapply(1:ncol(list_x), function(j) predict_single_value(list_x[,j], list_tree, type))
  
  return(y_p)
}
X1 <- runif(100, 0, 1)
X2 <- runif(100, 0, 1)
e <- rnorm(50, 0, 0.1)
Y <- X1^2 + X2 + e
data_reg <- tibble(a = X1, b = X2, y = Y) 
list_tree <- random_forest(x = c(X1,X2), y = Y, data = data_reg, type = "reg", B = 5, A = 50, m = 1)
list_x <- matrix(c(0.1, 0.3, 0.5, 0.7, 0.8, 0.9), ncol = 6)
predictions <- prediction(list_tree, list_x, type = "reg")
