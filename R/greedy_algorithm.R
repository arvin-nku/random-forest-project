# Greedy CART Regression Algorithm

#' Greedy CART Regression
#'
#' Diese Funktion implementiert den Greedy CART Algorithmus für Regressionsbäume.
#' @param data Eine Liste mit x (Prädiktoren) und y (Zielvariable).
#' @param num_leaf Anzahl der Blätter im Baum (optional).
#' @param depth Maximale Tiefe des Baumes (optional).
#' @param num_split Minimale Anzahl von Datenpunkten zum Splitten eines Knotens.
#' @param min_num Minimale Anzahl von Datenpunkten in einem Blatt.
#' @param m Anzahl der Variablen, die für die Erstellung des Baumes verwendet werden.
#' @return Eine Liste mit dem Entscheidungsbaum.
#' @export
greedy_cart_regression <- function(data, num_leaf = NULL, depth = NULL, num_split = 2, min_num = 1, m = 0) {
  
  # Funktion zur Aufteilung eines Knotens
  split_node <- function(data, depth) {
    # Abbruchbedingungen: 
    # Keine ausreichenden Datenpunkte zum Splitten oder maximale Tiefe erreicht
    if (nrow(data$x) < num_split || (!is.null(depth) && depth == 0)) {
      return(list(prediction = mean(data$y)))
    }
    
    # Suche nach dem besten Split im aktuellen Knoten
    best_split <- find_best_split_regression(data, m)
    if (is.null(best_split)) {
      return(list(prediction = mean(data$y)))
    }
    
    # Aufteilung der Daten basierend auf dem besten Split
    left_indices <- which(data$x[, best_split$split_index] <= best_split$split_point)
    right_indices <- which(data$x[, best_split$split_index] > best_split$split_point)
    
    # Abbruchbedingung: Keine ausreichenden Datenpunkte in den Kindknoten
    if (length(left_indices) < min_num || length(right_indices) < min_num) {
      return(list(prediction = mean(data$y)))
    }
    
    # Rekursive Aufteilung der linken und rechten Kindknoten
    left_data <- list(x = data$x[left_indices, , drop = FALSE], y = data$y[left_indices])
    right_data <- list(x = data$x[right_indices, , drop = FALSE], y = data$y[right_indices])
    
    return(list(
      split_index = best_split$split_index,
      split_point = best_split$split_point,
      left = split_node(left_data, if (is.null(depth)) NULL else depth - 1),
      right = split_node(right_data, if (is.null(depth)) NULL else depth - 1),
      value = best_split$split_point  # Ensure the split value is stored here
    ))
  }
  
  # Aufruf der rekursiven Split-Funktion, um den Entscheidungsbaum zu erstellen
  tree <- split_node(data, depth)
  tree$name <- "root"  # Root node explicitly named
  return(list(tree = tree))
}

# Funktion zur Suche des besten Splits für Regression
find_best_split_regression <- function(data, m) {
  best_split <- NULL
  best_loss <- Inf
  
  # Anzahl der zu berücksichtigenden Merkmale (m)
  num_features <- if (m == 0) ncol(data$x) else m
  
  # Überprüfung aller möglichen Splits in den ausgewählten Merkmalen
  for (split_index in sample(seq_len(ncol(data$x)), num_features)) {
    for (split_point in unique(data$x[, split_index])) {
      # Aufteilung der Daten basierend auf dem aktuellen Split
      left_indices <- which(data$x[, split_index] <= split_point)
      right_indices <- which(data$x[, split_index] > split_point)
      
      # Berechnung des Verlusts (Summe der quadratischen Abweichungen)
      if (length(left_indices) > 0 && length(right_indices) > 0) {
        left_mean <- mean(data$y[left_indices])
        right_mean <- mean(data$y[right_indices])
        loss <- sum((data$y[left_indices] - left_mean)^2) + sum((data$y[right_indices] - right_mean)^2)
        
        # Aktualisierung des besten Splits, wenn der Verlust minimiert wird
        if (loss < best_loss) {
          best_loss <- loss
          best_split <- list(split_index = split_index, split_point = split_point)
        }
      }
    }
  }
  return(best_split)
}

# Greedy CART Classification Algorithm

#' Greedy CART Classification
#'
#' Diese Funktion implementiert den Greedy CART Algorithmus für Klassifikationsbäume.
#' @param data Eine Liste mit x (Prädiktoren) und y (Zielvariable).
#' @param num_leaf Anzahl der Blätter im Baum (optional).
#' @param depth Maximale Tiefe des Baumes (optional).
#' @param num_split Minimale Anzahl von Datenpunkten zum Splitten eines Knotens.
#' @param min_num Minimale Anzahl von Datenpunkten in einem Blatt.
#' @param m Anzahl der Variablen, die für die Erstellung des Baumes verwendet werden.
#' @param unique Boolean, der angibt, ob nur eindeutige Zielwerte in einem Blatt erlaubt sind.
#' @return Eine Liste mit dem Entscheidungsbaum.
#' @export
greedy_cart_classification <- function(data, num_leaf = NULL, depth = NULL, num_split = 2, min_num = 1, m = 0, unique = FALSE) {
  
  # Funktion zur Aufteilung eines Knotens
  split_node <- function(data, depth) {
    # Abbruchbedingungen: 
    # Keine ausreichenden Datenpunkte zum Splitten, alle Zielwerte gleich oder maximale Tiefe erreicht
    if (nrow(data$x) < num_split || (unique && length(unique(data$y)) == 1) || (!is.null(depth) && depth == 0)) {
      return(list(prediction = Mode(data$y)))
    }
    
    # Suche nach dem besten Split im aktuellen Knoten
    best_split <- find_best_split_classification(data, m)
    if (is.null(best_split)) {
      return(list(prediction = Mode(data$y)))
    }
    
    # Aufteilung der Daten basierend auf dem besten Split
    left_indices <- which(data$x[, best_split$split_index] <= best_split$split_point)
    right_indices <- which(data$x[, best_split$split_index] > best_split$split_point)
    
    # Abbruchbedingung: Keine ausreichenden Datenpunkte in den Kindknoten
    if (length(left_indices) < min_num || length(right_indices) < min_num) {
      return(list(prediction = Mode(data$y)))
    }
    
    # Rekursive Aufteilung der linken und rechten Kindknoten
    left_data <- list(x = data$x[left_indices, , drop = FALSE], y = data$y[left_indices])
    right_data <- list(x = data$x[right_indices, , drop = FALSE], y = data$y[right_indices])
    
    return(list(
      split_index = best_split$split_index,
      split_point = best_split$split_point,
      left = split_node(left_data, if (is.null(depth)) NULL else depth - 1),
      right = split_node(right_data, if (is.null(depth)) NULL else depth - 1),
      value = best_split$split_point  # Ensure the split value is stored here
    ))
  }
  
  # Aufruf der rekursiven Split-Funktion, um den Entscheidungsbaum zu erstellen
  tree <- split_node(data, depth)
  tree$name <- "root"  # Root node explicitly named
  return(list(tree = tree))
}

# Funktion zur Suche des besten Splits für Klassifikation
find_best_split_classification <- function(data, m) {
  best_split <- NULL
  best_gini <- Inf
  
  # Anzahl der zu berücksichtigenden Merkmale (m)
  num_features <- if (m == 0) ncol(data$x) else m
  
  # Überprüfung aller möglichen Splits in den ausgewählten Merkmalen
  for (split_index in sample(seq_len(ncol(data$x)), num_features)) {
    for (split_point in unique(data$x[, split_index])) {
      # Aufteilung der Daten basierend auf dem aktuellen Split
      left_indices <- which(data$x[, split_index] <= split_point)
      right_indices <- which(data$x[, split_index] > split_point)
      
      # Berechnung des Gini-Index für die Aufteilung
      if (length(left_indices) > 0 && length(right_indices) > 0) {
        left_gini <- 1 - sum((table(data$y[left_indices]) / length(left_indices))^2)
        right_gini <- 1 - sum((table(data$y[right_indices]) / length(right_indices))^2)
        gini <- left_gini * length(left_indices) + right_gini * length(right_indices)
        
        # Aktualisierung des besten Splits, wenn der Gini-Index minimiert wird
        if (gini < best_gini) {
          best_gini <- gini
          best_split <- list(split_index = split_index, split_point = split_point)
        }
      }
    }
  }
  return(best_split)
}

# Hilfsfunktion zur Bestimmung des häufigsten Werts (Modus)
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}
