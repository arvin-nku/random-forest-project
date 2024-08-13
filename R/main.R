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
#' e <- rnorm(100, 0, 0.1)
#' Y <- X1^2 + X2 + e
#' data_reg_li <- list(a = X1, b=X2, y= Y)
#' list_tree <- random_forest(x = c(X1,X2), y = Y, data = data_reg_li, type = "reg", B = 5, A = 10, m = 1)
#' list_x <- matrix(c(0, 0.2, 0.3, 0.4, 0.6, 0.7, 0.9, 1), nrow = 2)
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
  
  if(length(list_tree[[1]]$A[[1]][[1]]) != nrow(list_x)){
    stop("The dimension of list_tree is not equal to the dimension of list_x")
  }
  
  #helpfunctions
  #node exists
  exists_node <- function(tree, node_number){
    return(any(tree$node == node_number))
  }

  #no leaf-node
  no_leaf <- function(tree, node_number) {
    return(length(tree$A[tree$node == node_number][[1]]) > 1)
  }
  
  #predictions with one tree
  pred <- function(tree, x){
    #start-node for prediction
    node_cur <- tree$node[tree$name == "root"]
    
    #left and right child nodes
    node_left <- node_cur*2 
    node_right <- (node_cur*2) + 1
    
    #find leaf node
    while(no_leaf(tree, node_cur) && ((exists_node(tree, node_left)) || exists_node(tree, node_right))){
      #node has a left child
      if(exists_node(tree, node_left)){
        dim_cur <- tree$split_index[tree$node == node_left]
        
        #take the first not NA value if the first is NA
        if(is.na(dim_cur)){
          dim_cur <- tree$split_index[!is.na(tree$split_index)][1]
        }
        #compare value with split point
        if(x[dim_cur] < tree$split_point[tree$node == node_left]){
          #move left
          node_new <- node_left
          node_cur <- tree$node[tree$node == node_new]  #move to the left child
        } else {
          #move right
          node_new <- node_right
          node_cur <- tree$node[tree$node == node_new]  #move to the right child
        }
      }
      else if(exists_node(tree, node_right)){
        #if left does not exists check for right
        dim_cur <- tree$split_index[tree$node == node_right]
        
      if(x[dim_cur] < tree$split_point[tree$node == node_right]){
        #move left
        node_new <- node_left
        node_cur <- tree$node[tree$node == node_new]
      }
      else{
        #move right
        node_new <- node_right
        node_cur <- tree$node[tree$node == node_new]
      }
    }
    #update left/right node 
    node_left <- node_cur*2 
    node_right <- (node_cur*2) + 1 
  }
    # Return the prediction value of the leaf node
    return(tree$c_value[tree$node == node_cur])
  }
  
  #calculation of predictions based on a list of decision trees
  y_p <- sapply(1:ncol(list_x), function(i) {
    #for each column in list_x
    list_y <- sapply(1:length(list_tree), function(j) {
    #for each tree in list_tree 
      pred(list_tree[[j]], list_x[, i])
    })
    #calculate the result based on the type
    if (type == "reg") {
      return(mean(list_y))
    } else if (type == "cla") {
      return(as.integer(tail(names(sort(table(list_y))), 1)))
    }
  })
  return(y_p)
}
