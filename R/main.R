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
  
  if(length(list_tree[[1]]$A[[1]][[1]]) != nrow(list_x)){
    stop("The dimension of list_tree is not equal to the dimension of list_x")
  }

  #helpfunctions
  #no leaf-node
  no_leaf <- function(tree, node_number){
    node_data <- tree[tree$node == node_number, ]
    if(!is.null(node_data$split_index)){
      return(T)
    }
    else{
      return(F)
    }
  }
  
  #node exists
  exists_node <- function(tree, node_number){
    return(any(tree$node == node_number))
  }
  
  #predictions with a tree
  pred <- function(tree, x){
    #start-node for prediction
    node_cur <- tree$node[tree$name == "root"]
    
    #find leaf node
    while(no_leaf(tree, node_cur)){
      #left and right child nodes
      node_left <- node_cur*2 
      node_right <- (node_cur*2) + 1
      #node has a left child
      if(exists_node(tree, node_cur *2)){
        dim_cur <- tree$split_index[tree$node == node_cur*2]
        #take the first not NA value if the fisrt is NA
        #if(is.na(dim_cur)){
          #dim_cur <- tree$split_index[!is.na(tree$split_index)][1]
        #}
        if(x[dim_cur] < tree$split_point[tree$node == node_cur*2]){
          node_new <- (tree$node[tree$node == node_cur])*2
          node_cur <- tree$node[tree$node == node_new]  #move to the left child
        } else {
          node_new <-((tree$node[tree$node == node_cur])*2)+1
          node_cur <- tree$node[tree$node == node_new]  #move to the right child
        }
      }
      else if(exists_node(tree, (node_cur *2)+1)){
        dim_cur <- tree$split_index[tree$node == (node_cur*2)+1]
        
      if(x[dim_cur] < tree$split_point[tree$node == (node_cur*2)+1]){
        node_new <- (tree$node[tree$node == node_cur])*2
        node_cur <- tree$node[tree$node == node_new]
      }
      else{
        node_new <- ((tree$node[tree$node == node_cur])*2)+1
        node_cur <- tree$node[tree$node == node_new]
      }
    }
  }
    # Return the prediction value of the leaf node
    return(tree$c_value[tree$node == node_cur])
  }
  
  y_p <- c()
  for (j in 1:ncol(list_x)) {
    y_list <- c()
    for(i in 1:length(list_tree)){
      y_list <- append(y_list, pred(list_tree[[i]], list_x[,j]))
    }
    
    if(type == "reg"){
      y_p <- append(y_p, mean(y_list))
    }
    else if(type == "cla"){
      y_p <- append(y_p, as.integer(tail(names(sort(table(y_list))), 1)))
    }
  }
  return(y_p)
}
X1 <- runif(100, 0, 1)
X2 <- runif(100, 0, 1)
e <- rnorm(50, 0, 0.1)
Y <- X1^2 + X2 + e
data_reg_li <- list(a = X1, b=X2, y= Y)
list_tree <- random_forest(x = c(X1,X2), y = Y, data = data_reg_li, type = "reg", B = 5, A = 10, m = 1)
list_x <- matrix(c(0, 0, 17, 17, 1.8, 2.9, 23, 12), nrow = 2)
predictions <- prediction(list_tree, list_x, type = "reg")
