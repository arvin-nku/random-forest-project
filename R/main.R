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


# Prediction Function with Improved Root Node Handling
prediction <- function(list_tree, list_x, type = NULL) {
  
  # Verification of the input
  if (!is.list(list_tree)) stop("The input of list_tree must be a list")
  if (!is.matrix(list_x)) stop("The input of list_x must be a matrix")
  if (is.null(type)) stop("The type is not set! Set the type 'reg' for regression or 'cla' for classification.")
  if (type != "reg" & type != "cla") stop("Invalid type!")
  
  # Filter out NA from split_index before counting unique values
  valid_split_indices <- unique(na.omit(list_tree[[1]]$split_index))
  
  # Print out the filtered split_index values for debugging
  # print("Filtered split_index values in the first tree (excluding NA):")
  # print(valid_split_indices)
  
  # Check if the feature number in list_x matches the expected features in the tree
  expected_features <- length(valid_split_indices)
  # print(paste("Expected features in tree:", expected_features))
  # print(paste("Number of features in list_x:", nrow(list_x)))
  
  if (nrow(list_x) != expected_features) {
    stop("Mismatch between the number of features in list_x and the expected number of features in the trees.")
  }
  
  # Helper functions
  exists_node <- function(tree, node_number) {
    return(any(tree$node == node_number))
  }
  
  no_leaf <- function(tree, node_number) {
    # Debugging information
    if (is.na(node_number) || node_number == "") {
      stop("Node number is NA or empty.")
    }
    # print(paste("Checking node number:", node_number))
    
    if (!exists_node(tree, node_number)) {
      stop(paste("Node", node_number, "does not exist in the tree."))
    }
    
    leaf_status <- length(tree$A[tree$node == node_number][[1]]) > 1
    print(paste("Node", node_number, "is a leaf node:", !leaf_status))
    
    return(leaf_status)
  }
  
  # Prediction with one tree
  pred <- function(tree, x) {
    # Print the full tree for debugging
    # print("Full tree structure:")
    # print(tree)
    
    # Start at the root node, which is typically node 1
    node_cur <- 1
    # print(paste("Initial node_cur value:", node_cur))
    
    node_left <- node_cur * 2
    node_right <- (node_cur * 2) + 1
    
    while (no_leaf(tree, node_cur) && ((exists_node(tree, node_left)) || exists_node(tree, node_right))) {
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
  
  # Prediction based on a list of decision trees
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
