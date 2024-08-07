greedy_cart_regression <- function(data, num_leaf = NULL, depth = NULL, num_split = 2, min_num = 1, m = 0) {
  split_node <- function(data, depth) {
    if (nrow(data$x) < num_split || (!is.null(depth) && depth == 0)) {
      return(list(prediction = mean(data$y)))
    }
    
    best_split <- find_best_split_regression(data)
    if (is.null(best_split)) {
      return(list(prediction = mean(data$y)))
    }
    
    left_indices <- which(data$x[, best_split$feature] <= best_split$value)
    right_indices <- which(data$x[, best_split$feature] > best_split$value)
    
    if (length(left_indices) < min_num || length(right_indices) < min_num) {
      return(list(prediction = mean(data$y)))
    }
    
    left_data <- list(x = data$x[left_indices, , drop = FALSE], y = data$y[left_indices])
    right_data <- list(x = data$x[right_indices, , drop = FALSE], y = data$y[right_indices])
    
    return(list(
      feature = best_split$feature,
      value = best_split$value,
      left = split_node(left_data, if (is.null(depth)) NULL else depth - 1),
      right = split_node(right_data, if (is.null(depth)) NULL else depth - 1)
    ))
  }
  
  tree <- split_node(data, depth)
  return(list(tree = tree))
}

find_best_split_regression <- function(data) {
  best_split <- NULL
  best_loss <- Inf
  for (feature in seq_len(ncol(data$x))) {
    for (value in unique(data$x[, feature])) {
      left_indices <- which(data$x[, feature] <= value)
      right_indices <- which(data$x[, feature] > value)
      if (length(left_indices) > 0 && length(right_indices) > 0) {
        left_mean <- mean(data$y[left_indices])
        right_mean <- mean(data$y[right_indices])
        loss <- sum((data$y[left_indices] - left_mean)^2) + sum((data$y[right_indices] - right_mean)^2)
        if (loss < best_loss) {
          best_loss <- loss
          best_split <- list(feature = feature, value = value)
        }
      }
    }
  }
  return(best_split)
}


greedy_cart_classification <- function(data, num_leaf = NULL, depth = NULL, num_split = 2, min_num = 1, m = 0, unique = FALSE) {
  split_node <- function(data, depth) {
    if (nrow(data$x) < num_split || (unique && length(unique(data$y)) == 1) || (!is.null(depth) && depth == 0)) {
      return(list(prediction = Mode(data$y)))
    }
    
    best_split <- find_best_split_classification(data)
    if (is.null(best_split)) {
      return(list(prediction = Mode(data$y)))
    }
    
    left_indices <- which(data$x[, best_split$feature] <= best_split$value)
    right_indices <- which(data$x[, best_split$feature] > best_split$value)
    
    if (length(left_indices) < min_num || length(right_indices) < min_num) {
      return(list(prediction = Mode(data$y)))
    }
    
    left_data <- list(x = data$x[left_indices, , drop = FALSE], y = data$y[left_indices])
    right_data <- list(x = data$x[right_indices, , drop = FALSE], y = data$y[right_indices])
    
    return(list(
      feature = best_split$feature,
      value = best_split$value,
      left = split_node(left_data, if (is.null(depth)) NULL else depth - 1),
      right = split_node(right_data, if (is.null(depth)) NULL else depth - 1)
    ))
  }
  
  tree <- split_node(data, depth)
  return(list(tree = tree))
}

find_best_split_classification <- function(data) {
  best_split <- NULL
  best_gini <- Inf
  for (feature in seq_len(ncol(data$x))) {
    for (value in unique(data$x[, feature])) {
      left_indices <- which(data$x[, feature] <= value)
      right_indices <- which(data$x[, feature] > value)
      if (length(left_indices) > 0 && length(right_indices) > 0) {
        left_gini <- 1 - sum((table(data$y[left_indices]) / length(left_indices))^2)
        right_gini <- 1 - sum((table(data$y[right_indices]) / length(right_indices))^2)
        gini <- left_gini * length(left_indices) + right_gini * length(right_indices)
        if (gini < best_gini) {
          best_gini <- gini
          best_split <- list(feature = feature, value = value)
        }
      }
    }
  }
  return(best_split)
}

Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}
