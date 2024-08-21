# Used libraries
library(tibble)
library(rlang)
library(dplyr)

#' Find Leaf Nodes
#'
#' Helper function to identify leaf nodes in a decision tree.
#' The function filters the nodes where `name` is "leaf" and returns the corresponding node identifiers.
#' 
#' @param tree A tibble representing the decision tree.
#' @return A vector of node identifiers corresponding to leaf nodes.
#' @export
find_leaf1 <- function(tree){
  tree %>%
    filter(name == "leaf") -> leafs
  return(leafs$node)
}

#' Greedy Algorithm (Regression)
#'
#' Greedy algorithm for regression data.
#' This function builds a regression tree using a greedy algorithm based on minimizing squared error.
#'
#' @param data A named list containing regression data. The x values have the name `x` and are in the form of a matrix where the row number gives the dimension of the data. The y values have the name `y` and are in the form of a vector.
#' @param num_leaf Condition to end: the tree has `num_leaf` leaves. Must be greater than or equal to 1. The default value is the maximum number of leaves (the number of data points).
#' @param depth Condition to end: the tree has depth `depth`. Must be greater than or equal to 0. The default value is the maximum achievable depth.
#' @param num_split Split only nodes which contain at least `num_split` elements. Must be greater than or equal to 2.
#' @param min_num Only split a node if both child nodes have at least `min_num` elements. Must be greater than or equal to 1.
#' @param m Parameter for Random Forest algorithm: positive number of coordinates used in each iteration. The default value is the dimension of the data.
#'
#' @return An environment containing the elements `dim`, `values`, and `tree`. 
#' \itemize{
#'   \item `dim`: The dimension of the data.
#'   \item `values`: Data in a tibble format.
#'   \item `tree`: Decision tree in the form of a tibble. It has the following columns:
#'     \itemize{
#'       \item `node`: Node identifier, where node n has child nodes 2n and 2n + 1.
#'       \item `name`: Type of the node (root, inner node, leaf).
#'       \item `split_index`: Index at which data is split.
#'       \item `split_point`: Value at which data is split.
#'       \item `y`: y values of data contained in this node.
#'       \item `A`: x values of data contained in this node.
#'       \item `c_value`: The approximate value for the data elements in a node.
#'     }
#' }
#' @export
#' 
#' @examples
#' X <- runif(100,0,1)
#' e <- rnorm(100,0,0.2)
#' Y <- sin(2*pi*X) + e
#' data <- list(x = matrix(X, nrow = 1), y = Y)
#' val <- greedy_cart_regression(data, depth = 3)
#' val$values
#' val$tree

greedy_cart_regression <- function(data, num_leaf = NULL, depth = NULL, num_split = 2, min_num = 1, m = 0){
  
  # Input verification and setting default parameters
  d <- nrow(data$x)  # Number of dimensions/features
  
  if(is.null(num_leaf)) num_leaf <- length(data$y) # Set num_leaf to the number of data points if not provided
  
  # Parameter validation
  if (!is.null(depth)){
    if(depth < 0) {warning("depth must be greater than or equal to 0. depth is set to the maximal depth"); depth <- -1}
  }
  if (num_split < 2) {warning("num_split must be greater than or equal to 2. num_split is set to 2"); num_split <- 2}
  if (min_num < 1) {warning("min_num must be greater than or equal to 1. min_num is set to 1"); min_num <- 1}
  if (num_leaf < 1) {warning("num_leaf must be greater than or equal to 1. num_leaf is set to ", length(data$y)); num_leaf <- length(data$y)}
  
  if(is.null(depth)) depth <- -1
  if(as.integer(num_leaf) != num_leaf) {warning("num_leaf is not an integer. The value is set to ", ceiling(num_leaf)); num_leaf <- ceiling(num_leaf)}
  if(as.integer(depth) != depth) {warning("depth is not an integer. The value is set to ", ceiling(depth)); depth <- ceiling(depth)}
  if(as.integer(num_split) != num_split) {warning("num_split is not an integer. The value is set to ", ceiling(num_split)); num_split <- ceiling(num_split)}
  if(as.integer(min_num) != min_num) {warning("min_num is not an integer. The value is set to ", ceiling(min_num)); min_num <- ceiling(min_num)}
  
  t <- num_leaf
  
  row <- nrow(data$x)
  if(as.integer(m) != m) {warning("m is not an integer. m is set to ", ceiling(m)); m <- ceiling(m)}
  if (m > row) {warning("m is too big. m is set to " , row); m <- row}
  if(missing(m)) m <- row
  if(m <= 0) {warning("m must be greater than 0. m is set to ", row); m <- row}
  
  # Initialize environment to store the tree and other relevant information
  greedyReg <- new.env()
  
  dat <- t(data$x)  # Transpose x for easier manipulation
  colnames(dat) <- paste0('x', 1:ncol(dat))  # Name the columns x1, x2, etc.
  tb <- as_tibble(dat)
  if(row == 1){
    greedyReg$values <- bind_cols(as_tibble_col(as.vector(data$x), column_name = "x"), as_tibble_col(as.vector(data$y), column_name = "y"))
  } else{
    greedyReg$values <- bind_cols(tb, as_tibble_col(as.vector(data$y), column_name = "y"))
  }
  
  greedyReg$dim <- row  # Store the dimension of the data
  # X and y setup
  X <- lapply(seq_len(ncol(data$x)), function(i) data$x[,i])
  n <- length(data$y)
  mean <- 1/n*sum(data$y)
  tree <- tibble(node = 1, name = "leaf", split_index = NA, split_point = NA, y = list(NULL), A = list(NULL), c_value = mean)
  
  tree[tree$node == 1,]$A[[1]] <- X  # Initialize the root node
  tree[tree$node == 1,]$y[[1]] <- as.list(data$y)
  
  # Utility functions for tree splitting
  # A1 and A2 are used to split data into left and right child nodes
  A1 <- function(j,s,v){
    set <- tree[tree$node == v, ]$A[[1]]
    A_1 <- list()
    for(i in seq_along(set)){
      if(set[[i]][j] < s){
        A_1[[length(A_1) + 1]] <- set[[i]]
      }
    }
    A_1
  }
  
  A2 <- function(j,s,v){
    set <- tree[tree$node == v, ]$A[[1]]
    A_2 <- list()
    for(i in seq_along(set)){
      if(set[[i]][j] >= s){
        A_2[[length(A_2) + 1]] <- set[[i]]
      }
    }
    A_2
  }
  
  # c1 and c2 calculate the mean y value for the left and right child nodes respectively
  c1 <- function(j,s,v){
    Y <- 0
    A_1 <- A1(j,s,v)
    for(i in seq_along(X)){
      if(Position(function(x) identical(x, X[[i]]), A_1, nomatch = 0) > 0) Y <- Y + data$y[i]
    }
    1/length(A_1) * Y
  }
  
  c2 <- function(j,s,v){
    Y <- 0
    A_2 <- A2(j,s,v)
    for(i in seq_along(X)){
      if(Position(function(x) identical(x, X[[i]]), A_2, nomatch = 0) > 0) Y <- Y + data$y[i]
    }
    1/length(A_2) * Y
  }
  
  # Find y values associated with data points in a specific node
  find_y <- function(nodes){
    tr <- tree[tree$node == nodes,]$A[[1]]
    Y <- list()
    for(i in seq_along(X)){
      if(Position(function(x) identical(x, X[[i]]), tr, nomatch = 0) > 0){
        Y[[length(Y) + 1]] <- data$y[i]
      }
    }
    Y
  }
  
  # Tree construction loop
  # This loop iteratively splits the tree until stopping criteria are met
  cond <- sapply(tree$A, length)
  if(depth == -1){
    depth_count <- -2
  } else{
    depth_count <- 0
  }
  
  while(!all(cond %in% 0:(num_split - 1)) & length(find_leaf1(tree)) <= t - 1 & depth_count < depth){
    tree1 <- tree  # Save the current state of the tree to check for changes
    leafs <- find_leaf1(tree)  # Find the current leaf nodes
    
    for(v in leafs){
      if(length(tree[tree$node == v,]$A[[1]]) %in% 0:(num_split - 1)) next  # Skip if node is too small
      
      # Minimize function to find the best split point
      minimize <- function(s){
        Y <- 0
        A <- A1(j,s,v)
        c_1 <- c1(j,s,v)
        c_2 <- c2(j,s,v)
        for(i in seq_along(X)){
          if(Position(function(x) identical(x, X[[i]]), A, nomatch = 0) > 0){
            Y <- Y + (data$y[i] - c_1)^2
          } else{
            Y <- Y + (data$y[i] - c_2)^2
          }
        }
        Y
      }
      
      # Find the best split among selected features
      op <- rep(NA, d)
      value <- rep(NA, d)
      t1 <- tree[tree$node == v,]$A[[1]]
      
      S <- sample(1:d,m)  # Randomly select m features
      S <- sort(S)
      for(k in S){
        j <- k
        idx <- sapply(t1, function(x) x[j])
        if(length(unique(idx)) == 1){
          optimum <- minimize(idx[[1]])
          op[k] <- optimum
          value[k] <- idx[[1]]
        } else{
          optimum <- optimize(minimize, c(min(idx), max(idx)))
          op[k] <- optimum$objective
          value[k] <- optimum$minimum
        }
      }
      
      opt <- c()
      opt[1] <- which.min(op)  # Select the feature with the minimum error
      opt[2] <- value[which.min(op)]  # Select the corresponding split value
      
      c_1 <- c1(opt[1],opt[2],v)
      c_2 <- c2(opt[1],opt[2],v)
      
      A_1 <- A1(opt[1],opt[2],v)
      A_2 <- A2(opt[1],opt[2],v)
      num_leafs <- length(find_leaf1(tree))
      
      # Split the node only if both resulting child nodes have at least min_num elements
      if(length(A_1) >= min_num & length(A_2) >= min_num){
        if(num_leafs == t){
          break
        } else{
          tree %>%
            add_row(node = 2*v, split_index = opt[1], split_point = opt[2], c_value = c_1) %>%
            add_row(node = 2*v + 1, split_index = opt[1], split_point = opt[2], c_value = c_2) -> tree
          tree[tree$node == 2*v,]$A[[1]] <- A1(opt[1],opt[2],v)
          tree[tree$node == 2*v + 1,]$A[[1]] <- A2(opt[1],opt[2],v)
          tree[tree$node == 2*v,]$y[[1]] <- find_y(2*v)
          tree[tree$node == 2*v + 1,]$y[[1]] <- find_y(2*v + 1)
          
          tree %>%
            mutate(name = ifelse(node == v, "inner node", ifelse(node == 2*v, "leaf", ifelse(node == 2*v + 1, "leaf", name)))) -> tree
        }
      }
    }
    
    tree %>%
      filter(name == "leaf") -> leaf_tree
    cond <- sapply(leaf_tree$A, length)
    if(depth != -1){
      depth_count <- depth_count + 1
    }
    
    if(isTRUE(all.equal(tree, tree1))) break  # Stop if no change in the tree
  }
  
  # Mark the root node
  tree %>%
    mutate(name = ifelse(node == 1, "root", name)) -> tree
  
  greedyReg$tree <- tree
  
  return(greedyReg)
}

#' Greedy Algorithm (Classification)
#'
#' Greedy algorithm for classification data.
#'
#' @param data A named list containing classification data. The x values have the name `x` and are in the form of a matrix where the row number gives the dimension of the data. The y values have the name `y` and are in the form of a vector.
#' @param num_leaf Condition to end: the tree has `num_leaf` leaves. Must be greater than or equal to 1. The default value is the maximum number of leaves (the number of data points).
#' @param depth Condition to end: the tree has depth `depth`. Must be greater than or equal to 0. The default value is the maximum achievable depth.
#' @param num_split Split only nodes which contain at least `num_split` elements. Must be greater than or equal to 2.
#' @param min_num Only split a node if both child nodes have at least `min_num` elements. Must be greater than or equal to 1.
#' @param m Parameter for Random Forest algorithm: positive number of coordinates used in each iteration. The default value is the dimension of the data.
#' @param unique If `unique` is set to TRUE, nodes where all data points in this node have the same class (y value) are not split. The default value is FALSE.
#'
#' @return An environment containing the elements `dim`, `values`, and `tree`.
#' \itemize{
#'   \item `dim`: The dimension of the data.
#'   \item `values`: Data in a tibble format.
#'   \item `tree`: Decision tree in the form of a tibble. It has the following columns:
#'     \itemize{
#'       \item `node`: Node identifier, where node n has child nodes 2n and 2n + 1.
#'       \item `name`: Type of the node (root, inner node, leaf).
#'       \item `split_index`: Index at which data is split.
#'       \item `split_point`: Value at which data is split.
#'       \item `y`: y values of data contained in this node.
#'       \item `A`: x values of data contained in this node.
#'       \item `c_value`: The approximate value for the data elements in a node.
#'     }
#' }
#' @export
#' 
#' @examples
#' X1 <- runif(200,0,1)
#' X2 <- runif(200,0,1)
#' e <- rnorm(200,0,0.2)
#' kappa <- function(x,y) y - 0.5 - 0.3*sin(2*pi*x)
#' f <- function(x,y,e){
#'   Y <- c()
#'   for(i in seq_along(x)){
#'     if(kappa(X1[i],X2[i]) - e[i] <= 0){
#'       Y[i] <- 1
#'     } else{
#'       Y[i] <- 2
#'     }
#'   }
#'   Y
#' }
#' data <- list(x = matrix(c(X1,X2), nrow = 2, byrow = TRUE), y = f(X1,X2,e))
#' val <- greedy_cart_classification(data, num_split = 10)
#' val$values
#' val$tree

greedy_cart_classification <- function(data, num_leaf = NULL, depth = NULL, num_split = 2, min_num = 1, m = 0, unique = FALSE){
  if(is.null(num_leaf)) num_leaf <- length(data$y)
  d <- nrow(data$x)
  
  if (!is.null(depth)){
    if(depth < 0) {warning("depth must be greater than or equal to 0. depth is set to the maximal depth"); depth <- -1}
  }
  if (num_split < 2) {warning("num_split must be greater than or equal to 2. num_split is set to 2"); num_split <- 2}
  if (min_num < 1) {warning("min_num must be greater than or equal to 1. min_num is set to 1"); min_num <- 1}
  if (num_leaf < 1) {warning("num_leaf must be greater than or equal to 1. num_leaf is set to ", length(data$y)); num_leaf <- length(data$y)}
  
  if(!is.logical(unique)) {warning("unique must be logical. unique is set to FALSE"); unique <- FALSE}
  
  if(is.null(depth)) depth <- -1
  if(as.integer(num_leaf) != num_leaf) {warning("num_leaf is not an integer. The value is set to ", ceiling(num_leaf)); num_leaf <- ceiling(num_leaf)}
  if(as.integer(depth) != depth) {warning("depth is not an integer. The value is set to ", ceiling(depth)); depth <- ceiling(depth)}
  if(as.integer(num_split) != num_split) {warning("num_split is not an integer. The value is set to ", ceiling(num_split)); num_split <- ceiling(num_split)}
  if(as.integer(min_num) != min_num) {warning("min_num is not an integer. The value is set to ", ceiling(min_num)); min_num <- ceiling(min_num)}
  
  t <- num_leaf
  
  row <- nrow(data$x)
  if(as.integer(m) != m) {warning("m is not an integer. m is set to ", ceiling(m)); m <- ceiling(m)}
  if (m > row) {warning("m is too big. m is set to " , row); m <- row}
  if(missing(m)) m <- row
  if(m <= 0) {warning("m must be greater than 0. m is set to ", row); m <- row}
  
  # Initialize environment to store the tree and other relevant information
  greedyCla <- new.env()
  greedyCla$dim <- row
  
  dat <- t(data$x)
  colnames(dat) <- paste0('x', 1:ncol(dat))
  tb <- as_tibble(dat)
  if(row == 2){
    greedyCla$values <- bind_cols(as_tibble_col(as.vector(data$x[1, ]), column_name = "x"),
                                  as_tibble_col(as.vector(data$x[2, ]), column_name = "y"))
    greedyCla$values <- bind_cols(greedyCla$values, as_tibble_col(data$y, column_name = "classes"))
  } else{
    greedyCla$values <- bind_cols(tb, as_tibble_col(as.vector(data$y), column_name = "classes"))
  }
  
  X <- lapply(seq_len(ncol(data$x)), function(i) data$x[,i])
  n <- length(data$y)
  tree <- tibble(node = 1, name = "leaf", split_index = NA, split_point = NA, y = list(NULL), A = list(NULL), c_value = 0)
  
  tree[tree$node == 1,]$A[[1]] <- X
  tree[tree$node == 1,]$y[[1]] <- as.list(data$y)
  
  K <- length(unique(data$y))
  
  # Utility functions for splitting data in classification tasks
  A1 <- function(j,s,v){
    set <- tree[tree$node == v, ]$A[[1]]
    A_1 <- list()
    for(i in seq_along(set)){
      if(set[[i]][j] < s){
        A_1[[length(A_1) + 1]] <- set[[i]]
      }
    }
    A_1
  }
  
  A2 <- function(j,s,v){
    set <- tree[tree$node == v, ]$A[[1]]
    A_2 <- list()
    for(i in seq_along(set)){
      if(set[[i]][j] >= s){
        A_2[[length(A_2) + 1]] <- set[[i]]
      }
    }
    A_2
  }
  
  # p calculates the proportion of class k in set A
  p <- function(k,A){
    idx <- 0
    for(i in seq_along(X)){
      if(Position(function(x) identical(x, X[[i]]), A, nomatch = 0) > 0 & data$y[[i]] == k){
        idx <- idx + 1
      }
    }
    if(length(A) == 0) return(0)
    return(idx/length(A))
  }
  
  # Find the most common class in the root node
  obj <- rep(NA,K)
  A_X <- tree[tree$node == 1, ]$A[[1]]
  for(k in 1:K){
    obj[[k]] <- p(k, A_X)
  }
  tree[tree$node == 1, ]$c_value <- which.max(obj)
  
  # c1 and c2 calculate the most common class in the left and right child nodes respectively
  c1 <- function(j,s,v){
    obj <- rep(NA,K)
    A_1 <- A1(j,s,v)
    for(k in 1:K){
      obj[[k]] <- p(k, A_1)
    }
    which.max(obj)
  }
  
  c2 <- function(j,s,v){
    obj <- rep(NA,K)
    A_2 <- A2(j,s,v)
    for(k in 1:K){
      obj[[k]] <- p(k, A_2)
    }
    which.max(obj)
  }
  
  # Find y values associated with data points in a specific node
  find_y <- function(nodes){
    tr <- tree[tree$node == nodes,]$A[[1]]
    Y <- list()
    for(i in seq_along(X)){
      if(Position(function(x) identical(x, X[[i]]), tr, nomatch = 0) > 0){
        Y[[length(Y) + 1]] <- data$y[i]
      }
    }
    Y
  }
  
  # Tree construction loop
  cond <- sapply(tree$A, length)
  if(depth == -1){
    depth_count <- -2
  } else{
    depth_count <- 0
  }
  
  while(!all(cond %in% 0:(num_split - 1)) & length(find_leaf1(tree)) <= t - 1 & depth_count < depth){
    tree1 <- tree  # Save the current state of the tree to check for changes
    leafs <- find_leaf1(tree)  # Find the current leaf nodes
    
    for(v in leafs){
      if(length(tree[tree$node == v,]$A[[1]]) %in% 0:(num_split - 1)) next  # Skip if node is too small
      if(unique){
        if(length(unique(tree[tree$node == v,]$y[[1]])) == 1) next  # Skip if all y values in the node are the same
      }
      
      # Minimize function to find the best split point
      minimize <- function(s){
        A_1 <- A1(j,s,v)
        A_2 <- A2(j,s,v)
        length(A_1)*(1-p(c1(j,s,v),A_1)) + length(A_2)*(1-p(c2(j,s,v), A_2))
      }
      
      # Find the best split among selected features
      op <- rep(NA, d)
      value <- rep(NA, d)
      t1 <- tree[tree$node == v,]$A[[1]]
      
      S <- sample(1:d,m)  # Randomly select m features
      S <- sort(S)
      
      for(k in S){
        j <- k
        idx <- sapply(t1, function(x) x[j])
        if(length(unique(idx)) == 1){
          optimum <- minimize(idx[[1]])
          op[k] <- optimum
          value[k] <- idx[[1]]
        } else{
          optimum <- optimize(minimize, c(min(idx), max(idx)))
          op[k] <- optimum$objective
          value[k] <- optimum$minimum
        }
      }
      
      opt <- c()
      min <- which(op == min(op))
      if(length(min) >= 2){
        x <- sample(min, 1)
        opt[1] <- x
        opt[2] <- value[x]
      } else{
        opt[1] <- which.min(op)
        opt[2] <- value[which.min(op)]
      }
      
      c_1 <- c1(opt[1],opt[2],v)
      c_2 <- c2(opt[1],opt[2],v)
      
      A_1 <- A1(opt[1],opt[2],v)
      A_2 <- A2(opt[1],opt[2],v)
      num_leafs <- length(find_leaf1(tree))
      
      # Split the node only if both resulting child nodes have at least min_num elements
      if(length(A_1) >= min_num & length(A_2) >= min_num){
        if(num_leafs == t){
          break
        } else{
          tree %>%
            add_row(node = 2*v, split_index = opt[1], split_point = opt[2], c_value = c_1) %>%
            add_row(node = 2*v + 1, split_index = opt[1], split_point = opt[2], c_value = c_2) -> tree
          tree[tree$node == 2*v,]$A[[1]] <- A1(opt[1],opt[2],v)
          tree[tree$node == 2*v + 1,]$A[[1]] <- A2(opt[1],opt[2],v)
          tree[tree$node == 2*v,]$y[[1]] <- find_y(2*v)
          tree[tree$node == 2*v + 1,]$y[[1]] <- find_y(2*v + 1)
          
          tree %>%
            mutate(name = ifelse(node == v, "inner node", ifelse(node == 2*v, "leaf", ifelse(node == 2*v + 1, "leaf", name)))) -> tree
        }
      }
    }
    
    tree %>%
      filter(name == "leaf") -> leaf_tree
    cond <- sapply(leaf_tree$A, length)
    if(depth != -1){
      depth_count <- depth_count + 1
    }
    
    if(isTRUE(all.equal(tree, tree1))) break  # Stop if no change in the tree
  }
  
  # Mark the root node
  tree %>%
    mutate(name = ifelse(node == 1, "root", name)) -> tree
  
  greedyCla$tree <- tree
  
  return(greedyCla)
}

#' Greedy Algorithm
#'
#' Greedy algorithm for either regression or classification data.
#'
#' @param x Column/list name(s) of the x value(s).
#' @param y Column/list name of the y value.
#' @param data Tibble or named list with data.
#' @param type "reg" for regression tree, "class" for classification tree. If `type` is missing, the function tries to guess the type.
#' @param num_leaf Condition to end: the tree has `num_leaf` leaves. Must be greater than or equal to 1. The default value is the maximum number of leaves (the number of data points).
#' @param depth Condition to end: the tree has depth `depth`. Must be greater than or equal to 0. The default value is the maximum achievable depth.
#' @param num_split Split only nodes which contain at least `num_split` elements. Must be greater than or equal to 2.
#' @param min_num Only split a node if both child nodes have at least `min_num` elements. Must be greater than or equal to 1.
#' @param m Parameter for Random Forest algorithm: positive number of coordinates used in each iteration. The default value is the dimension of the data.
#' @param unique Parameter for classification data: if `unique` is set to TRUE, nodes where all data points in this node have the same class (y value) are not split. The default value is FALSE.
#'
#' @return An environment containing the elements `dim`, `values`, and `tree`.
#' \itemize{
#'   \item `dim`: The dimension of the data.
#'   \item `values`: Data in a tibble format.
#'   \item `tree`: Decision tree in the form of a tibble. It has the following columns:
#'     \itemize{
#'       \item `node`: Node identifier, where node n has child nodes 2n and 2n + 1.
#'       \item `name`: Type of the node (root, inner node, leaf).
#'       \item `split_index`: Index at which data is split.
#'       \item `split_point`: Value at which data is split.
#'       \item `y`: y values of data contained in this node.
#'       \item `A`: x values of data contained in this node.
#'       \item `c_value`: The approximate value for the data elements in a node.
#'     }
#' }
#' @export
#' 
#' @examples
#' X <- runif(100,0,1)
#' e <- rnorm(100,0,0.2)
#' Y <- sin(2*pi*X) + e
#' data <- list(a = X, b = Y)
#' val <- greedy_cart(x = a, y = b, data = data, type = "reg")
#' val$values
#' val$tree
#'
#' X1 <- runif(200,0,1)
#' X2 <- runif(200,0,1)
#' e <- rnorm(200,0,0.05)
#' k <- function(x,y) (x-0.5)*(y-0.5)
#' g <- function(x,y,e){
#'   Y <- c()
#'   for(i in seq_along(x)){
#'    if(k(X1[i],X2[i]) - e[i] <= 0){
#'      Y[i] <- 1
#'     } else{
#'       Y[i] <- 2
#'     }
#'   }
#'   Y
#' }
#' tbl <- tibble(x1 = X1, x2 = X2, y = g(X1,X2,e))
#' val <- greedy_cart(x = c(x1,x2), y = y, data = tbl, type = "class", depth = 3)
#' val$values
#' val$tree

greedy_cart <- function(x, y, data, type = NULL, num_leaf = NULL ,depth = NULL, num_split = 2, min_num = 1, m = 0, unique = FALSE){
  x1 <- enexpr(x)
  y1 <- enexpr(y)
  data1 <- eval_tidy(x1,data)
  data2 <- eval_tidy(y1,data)
  
  stopifnot("x must be numeric" = is.numeric(data1))
  stopifnot("y must be numeric" = is.numeric(data2))
  stopifnot("y must be one-dimensional" = NCOL(data2) == 1)
  stopifnot("x and y don't have compatible length" = as.integer(length(data1)/length(data2))*length(data2) == length(data1))
  
  mat <- matrix(data1, nrow = length(data1)/length(data2), byrow = TRUE)
  dat <- list(x = mat, y = data2)
  row <- nrow(mat)
  if(missing(m)) m <- row
  
  if(is.null(type)){
    y_int <- as.integer(data2)
    if(all(y_int == data2) & all(y_int >= 1)){
      type <- "class"
      warning("Type was forgotten. Type was set to classification")
    } else{
      type <- "reg"
      warning("Type was forgotten. Type was set to regression")
    }
  }
  
  if(type == "reg"){
    return(greedy_cart_regression(dat, num_leaf = num_leaf, depth = depth, num_split = num_split, min_num = min_num, m = m))
  } else if(type == "class"){
    return(greedy_cart_classification(dat, num_leaf = num_leaf, depth = depth, num_split = num_split, min_num = min_num, m = m, unique = unique))
  } else{
    stop("Invalid type!")
  }
}
