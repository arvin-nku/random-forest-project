library(dplyr)
library(rlang)

#' Greedy Algorithm (Regression)
#'
#' Greedy algorithm for regression data
#'
#' @param data A named list that contains regression data
#' The x values have the name 'x' and are in the form of a matrix where the row number gives the dimension of the data.
#' The y values have the name 'y' and are in the form of a vector.
#' @param depth Condition to end: the tree has a depth 'depth'. Must be greater than or equal to 0.
#' The default value is the maximal achievable depth.
#' @param num_split Split only nodes which contain at least 'num_split' elements. Must be greater than or equal to 2.
#' @param min_num Only split a node if both child nodes have at least 'min_num' elements. Must be greater than or equal to 1.
#' @param num_leaf Condition to end: the tree has 'num_leaf' leaves. Must be greater than or equal to 1.
#' The default value is the maximal achievable number of leaves (the number of data points).
#' @param m Parameter for the Random Forest algorithm: positive number of coordinates (features) to use in each iteration.
#' The default value is the dimension of the data (all features).
#'
#' @return A list containing the decision tree in tibble form and other relevant details.
#' @export
greedy_cart_regression <- function(data, num_leaf = NULL, depth = NULL, num_split = 2, min_num = 1, m = 0) {
  # Input verification and setting default parameters
  d <- nrow(data$x)  # Number of dimensions/features
  n <- ncol(data$x)  # Number of data points
  
  # Set default values for num_leaf and depth if not provided
  if (is.null(num_leaf)) num_leaf <- n
  if (is.null(depth)) depth <- Inf
  
  # Ensure all conditions are met
  stopifnot(num_split >= 2, min_num >= 1, num_leaf >= 1, depth >= 0)
  
  # Set the number of features to use in each iteration (m)
  if (m <= 0) m <- d
  if (m > d) m <- d
  
  # Initialize the decision tree with the root node
  tree <- tibble(
    node = 1,  # Root node identifier
    name = "leaf",  # Node type (initially, the root is a leaf)
    split_index = NA,  # No split at the root
    split_point = NA,  # No split point at the root
    y = list(data$y),  # All y values at the root
    A = list(as.data.frame(t(data$x))),  # All x values at the root
    c_value = mean(data$y)  # Mean of y values (regression prediction)
  )
  
  current_leaves <- 1  # Number of current leaf nodes
  
  # Tree growing loop until we reach the desired number of leaves or no more splits are possible
  while (current_leaves < num_leaf && any(tree$name == "leaf")) {
    leaves <- which(tree$name == "leaf")  # Indices of current leaves
    
    # Iterate over each leaf node to attempt a split
    for (v in leaves) {
      current_data <- tree$A[[v]]  # Data points in the current leaf
      
      # Skip splitting if the node has fewer data points than num_split
      if (nrow(current_data) < num_split) next
      
      best_split <- list(error = Inf)  # Initialize the best split with infinite error
      
      # Iterate over a random subset of features
      for (j in sample(1:d, m)) {
        sort_values <- sort(unique(current_data[[j]]))  # Unique sorted values of the feature
        
        # Attempt to find the best split point
        for (i in 1:(length(sort_values) - 1)) {
          split_point <- mean(sort_values[i:(i+1)])  # Potential split point
          left_idx <- which(current_data[[j]] < split_point)  # Indices of points going left
          right_idx <- which(current_data[[j]] >= split_point)  # Indices of points going right
          
          # Ensure both child nodes have at least min_num elements
          if (length(left_idx) < min_num || length(right_idx) < min_num) next
          
          # Calculate the mean and error for the left and right splits
          left_mean <- mean(tree$y[[v]][left_idx])
          right_mean <- mean(tree$y[[v]][right_idx])
          left_error <- sum((tree$y[[v]][left_idx] - left_mean)^2)
          right_error <- sum((tree$y[[v]][right_idx] - right_mean)^2)
          total_error <- left_error + right_error
          
          # Update the best split if the current one is better
          if (total_error < best_split$error) {
            best_split <- list(
              j = j,
              s = split_point,
              left_idx = left_idx,
              right_idx = right_idx,
              left_mean = left_mean,
              right_mean = right_mean,
              error = total_error
            )
          }
        }
      }
      
      # If a valid split was found, update the tree structure
      if (best_split$error < Inf) {
        tree$name[v] <- "inner"  # The current node becomes an inner node (not a leaf)
        
        # Add left child node
        tree <- add_row(tree,
                        node = 2 * tree$node[v],
                        name = "leaf",
                        split_index = best_split$j,
                        split_point = best_split$s,
                        y = list(tree$y[[v]][best_split$left_idx]),
                        A = list(current_data[best_split$left_idx, , drop = FALSE]),
                        c_value = best_split$left_mean
        )
        
        # Add right child node
        tree <- add_row(tree,
                        node = 2 * tree$node[v] + 1,
                        name = "leaf",
                        split_index = best_split$j,
                        split_point = best_split$s,
                        y = list(tree$y[[v]][best_split$right_idx]),
                        A = list(current_data[best_split$right_idx, , drop = FALSE]),
                        c_value = best_split$right_mean
        )
      }
      
      # Update the count of leaf nodes
      current_leaves <- sum(tree$name == "leaf")
    }
  }
  
  # Update the root node's name for clarity
  tree %>% mutate(name = ifelse(node == 1, "root", name))
  
  return(list(tree = tree))
}

#' Greedy Algorithm (Classification)
#'
#' Greedy algorithm for classification data
#'
#' @param data A named list that contains classification data
#' The x values have the name 'x' and are in the form of a matrix where the row number gives the dimension of the data.
#' The y values have the name 'y' and are in the form of a vector.
#' @param depth Condition to end: the tree has a depth 'depth'. Must be greater than or equal to 0.
#' The default value is the maximal achievable depth.
#' @param num_split Split only nodes which contain at least 'num_split' elements. Must be greater than or equal to 2.
#' @param min_num Only split a node if both child nodes have at least 'min_num' elements. Must be greater than or equal to 1.
#' @param num_leaf Condition to end: the tree has 'num_leaf' leaves. Must be greater than or equal to 1.
#' The default value is the maximal achievable number of leaves (the number of data points).
#' @param m Parameter for the Random Forest algorithm: positive number of coordinates (features) to use in each iteration.
#' The default value is the dimension of the data (all features).
#' @param unique Parameter for classification data: if 'unique' is set to TRUE, we don't split nodes where all data points in this node have the same class (y value).
#' The default value is FALSE.
#'
#' @return A list containing the decision tree in tibble form and other relevant details.
#' @export
greedy_cart_classification <- function(data, num_leaf = NULL, depth = NULL, num_split = 2, min_num = 1, m = 0, unique = FALSE) {
  # Input verification and setting default parameters
  d <- nrow(data$x)  # Number of dimensions/features
  n <- ncol(data$x)  # Number of data points
  
  # Set default values for num_leaf and depth if not provided
  if (is.null(num_leaf)) num_leaf <- n
  if (is.null(depth)) depth <- Inf
  
  # Ensure all conditions are met
  stopifnot(num_split >= 2, min_num >= 1, num_leaf >= 1, depth >= 0)
  
  # Set the number of features to use in each iteration (m)
  if (m <= 0) m <- d
  if (m > d) m <- d
  
  # Initialize the decision tree with the root node
  tree <- tibble(
    node = 1,  # Root node identifier
    name = "leaf",  # Node type (initially, the root is a leaf)
    split_index = NA,  # No split at the root
    split_point = NA,  # No split point at the root
    y = list(data$y),  # All y values at the root
    A = list(as.data.frame(t(data$x))),  # All x values at the root
    c_value = as.integer(names(sort(table(data$y), decreasing = TRUE))[1])  # Most frequent class (classification prediction)
  )
  
  current_leaves <- 1  # Number of current leaf nodes
  
  # Tree growing loop until we reach the desired number of leaves or no more splits are possible
  while (current_leaves < num_leaf && any(tree$name == "leaf")) {
    leaves <- which(tree$name == "leaf")  # Indices of current leaves
    
    # Iterate over each leaf node to attempt a split
    for (v in leaves) {
      current_data <- tree$A[[v]]  # Data points in the current leaf
      
      # Skip splitting if the node has fewer data points than num_split or if all y values are identical (if unique is TRUE)
      if (nrow(current_data) < num_split || (unique && length(unique(tree$y[[v]])) == 1)) next
      
      best_split <- list(error = Inf)  # Initialize the best split with infinite error
      
      # Iterate over a random subset of features
      for (j in sample(1:d, m)) {
        sort_values <- sort(unique(current_data[[j]]))  # Unique sorted values of the feature
        
        # Attempt to find the best split point
        for (i in 1:(length(sort_values) - 1)) {
          split_point <- mean(sort_values[i:(i+1)])  # Potential split point
          left_idx <- which(current_data[[j]] < split_point)  # Indices of points going left
          right_idx <- which(current_data[[j]] >= split_point)  # Indices of points going right
          
          # Ensure both child nodes have at least min_num elements
          if (length(left_idx) < min_num || length(right_idx) < min_num) next
          
          # Calculate the most frequent class and error for the left and right splits
          left_class <- as.integer(names(sort(table(tree$y[[v]][left_idx]), decreasing = TRUE))[1])
          right_class <- as.integer(names(sort(table(tree$y[[v]][right_idx]), decreasing = TRUE))[1])
          left_error <- length(left_idx) * (1 - sum(tree$y[[v]][left_idx] == left_class) / length(left_idx))
          right_error <- length(right_idx) * (1 - sum(tree$y[[v]][right_idx] == right_class) / length(right_idx))
          total_error <- left_error + right_error
          
          # Update the best split if the current one is better
          if (total_error < best_split$error) {
            best_split <- list(
              j = j,
              s = split_point,
              left_idx = left_idx,
              right_idx = right_idx,
              left_class = left_class,
              right_class = right_class,
              error = total_error
            )
          }
        }
      }
      
      # If a valid split was found, update the tree structure
      if (best_split$error < Inf) {
        tree$name[v] <- "inner"  # The current node becomes an inner node (not a leaf)
        
        # Add left child node
        tree <- add_row(tree,
                        node = 2 * tree$node[v],
                        name = "leaf",
                        split_index = best_split$j,
                        split_point = best_split$s,
                        y = list(tree$y[[v]][best_split$left_idx]),
                        A = list(current_data[best_split$left_idx, , drop = FALSE]),
                        c_value = best_split$left_class
        )
        
        # Add right child node
        tree <- add_row(tree,
                        node = 2 * tree$node[v] + 1,
                        name = "leaf",
                        split_index = best_split$j,
                        split_point = best_split$s,
                        y = list(tree$y[[v]][best_split$right_idx]),
                        A = list(current_data[best_split$right_idx, , drop = FALSE]),
                        c_value = best_split$right_class
        )
      }
      
      # Update the count of leaf nodes
      current_leaves <- sum(tree$name == "leaf")
    }
  }
  
  # Update the root node's name for clarity
  tree %>% mutate(name = ifelse(node == 1, "root", name))
  
  return(list(tree = tree))
}

# Wrapper function for greedy_cart to handle both regression and classification

#' Greedy Algorithm
#'
#' Greedy algorithm for either regression or classification data
#'
#' @param x Column/list name(s) of the x value(s)
#' @param y Column/list name of the y value
#' @param data Tibble or named list with data
#' @param type "reg" for regression tree or "cla" for classification tree.
#' If type is missing, the function tries to "guess" the type based on the data.
#' @param depth Condition to end: the tree has depth 'depth'. Must be greater than or equal to 0.
#' The default value is the maximal achievable depth.
#' @param num_split Split only nodes which contain at least 'num_split' elements. Must be greater than or equal to 2.
#' @param min_num Only split a node if both child nodes have at least 'min_num' elements. Must be greater than or equal to 1.
#' @param num_leaf Condition to end: the tree has 'num_leaf' leaves. Must be greater than or equal to 1.
#' The default value is the maximal achievable number of leaves (the number of data points).
#' @param m Parameter for the Random Forest algorithm: positive number of coordinates (features) to use in each iteration.
#' The default value is the dimension of the data (all features).
#' @param unique Parameter for classification data: if 'unique' is set to TRUE, we don't split nodes where all data points in this node have the same class (y value).
#' The default value is FALSE.
#'
#' @return A list containing the decision tree in tibble form and other relevant details.
#' @export
greedy_cart <- function(x, y, data, type = NULL, num_leaf = NULL, depth = NULL, num_split = 2, min_num = 1, m = 0, unique = FALSE) {
  # Convert input data from tibble or list to the required format
  x1 <- enquo(x)
  y1 <- enquo(y)
  data_x <- eval_tidy(x1, data)
  data_y <- eval_tidy(y1, data)
  
  # Input validation to ensure data is numeric and y has only one column
  stopifnot(is.numeric(data_x), is.numeric(data_y), NCOL(data_y) == 1)
  
  # Convert x values into a matrix form
  mat <- matrix(data_x, nrow = length(data_x) / length(data_y), byrow = TRUE)
  data <- list(x = mat, y = data_y)
  
  # Guess the type if not provided
  if (is.null(type)) {
    if (all(as.integer(data_y) == data_y) && all(data_y >= 1)) {
      type <- "cla"
      warning("Type was set to classification based on the data.")
    } else {
      type <- "reg"
      warning("Type was set to regression based on the data.")
    }
  }
  
  # Call the appropriate greedy_cart function based on the type
  if (type == "reg") {
    return(greedy_cart_regression(data, num_leaf, depth, num_split, min_num, m))
  } else if (type == "cla") {
    return(greedy_cart_classification(data, num_leaf, depth, num_split, min_num, m, unique))
  } else {
    stop("Invalid type! Use 'reg' for regression or 'cla' for classification.")
  }
}
