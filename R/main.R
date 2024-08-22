#' Prediction of datapoint or a set of data points in a tree or a set of trees
#'
#' @importFrom utils tail
#'
#' @param list_tree a list containing an arbitrary number greater or equal to 1 of trees in tibble form\cr 
#' as used by `greedy_cart()`, `bagging()` or `random_forest()`
#' @param list_x a matrix of values to be predicted, where each column corresponds to a datapoint \cr
#' the number of rows corresponds to the dimension of the x-values in the list/matrix
#' @param type “reg” for regression tree\cr
#' “cla” for classification tree\cr
#' 
#' @return A numeric vector containing the predicted values. For regression trees (`type = "reg"`), 
#' the predictions are the mean of the predictions across all trees for each data point. 
#' For classification trees (`type = "cla"`), the predictions are the most frequent class label across all trees for each data point.
#' @export
#' @examples
#' X1 <- runif(100, 0, 1)
#' X2 <- runif(100, 0, 1)
#' e <- rnorm(100, 0, 0.1)
#' Y <- X1^2 + X2 + e
#' data_reg_li <- list(a = X1, b=X2, y= Y)
#' list_tree <- random_forest(x = c(X1,X2), y = Y, data = data_reg_li, type = "reg", B = 5, A = 10, m = 1)
#' list_x <- matrix(c(0, 0.2, 0.3, 0.4, 0.6, 0.7, 0.9, 1), nrow = 2)
#' predictions <- prediction(list_tree, list_x, type = "reg")

# Prediction function
prediction <- function(list_tree, list_x, type = NULL) {
  
  # Verify inputs
  if (!is.list(list_tree)) stop("The input of list_tree must be a list")
  if (!is.matrix(list_x)) stop("The input of list_x must be a matrix")
  if (is.null(type)) stop("The type is not set! Set the type 'reg' for regression or 'cla' for classification.")
  if (type != "reg" & type != "cla") stop("Invalid type!")
  
  # Adjust for different input types in list_tree
  if (is.environment(list_tree[[1]])) {
    list_tree <- lapply(list_tree, function(env) env$tree)
  } else if (is.data.frame(list_tree[[1]])) {
    list_tree <- list_tree  # Assume trees are directly in list
  } else {
    stop("The input list_tree does not contain valid tree objects.")
  }
  
  # Helper function to check if a node exists
  exists_node <- function(tree, node_number) {
    return(any(tree$node == node_number))
  }
  
  no_leaf <- function(tree, node_number) {
    if (is.na(node_number) || node_number == "") {
      stop("Node number is NA or empty.")
    }
    if (!exists_node(tree, node_number)) {
      stop(paste("Node", node_number, "does not exist in the tree."))
    }
    leaf_status <- length(tree$A[tree$node == node_number][[1]]) > 1
    return(leaf_status)
  }
  
  # Prediction function for a single tree
  pred <- function(tree, x) {
    node_cur <- 1
    node_left <- node_cur * 2
    node_right <- (node_cur * 2) + 1
    
    while (no_leaf(tree, node_cur) && (exists_node(tree, node_left) || exists_node(tree, node_right))) {
      if (exists_node(tree, node_left)) {
        dim_cur <- tree$split_index[tree$node == node_left]
        
        if (is.na(dim_cur)) {
          dim_cur <- tree$split_index[!is.na(tree$split_index)][1]
        }
        
        if (x[dim_cur] < tree$split_point[tree$node == node_left]) {
          node_cur <- node_left
        } else {
          node_cur <- node_right
        }
      } else if (exists_node(tree, node_right)) {
        dim_cur <- tree$split_index[tree$node == node_right]
        
        if (x[dim_cur] < tree$split_point[tree$node == node_right]) {
          node_cur <- node_left
        } else {
          node_cur <- node_right
        }
      }
      
      node_left <- node_cur * 2
      node_right <- (node_cur * 2) + 1
    }
    
    return(tree$c_value[tree$node == node_cur])
  }
  
  # Prediction based on a list of trees
  y_p <- sapply(1:ncol(list_x), function(i) {
    list_y <- sapply(1:length(list_tree), function(j) {
      pred(list_tree[[j]], list_x[, i])
    })
    
    if (type == "reg") {
      return(mean(list_y))
    } else if (type == "cla") {
      return(as.integer(tail(names(sort(table(list_y))), 1)))
    }
  })
  
  return(y_p)
}
