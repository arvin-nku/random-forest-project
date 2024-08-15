# Funktion zur Berechnung der Varianz für Regression
calculate_variance <- function(y) {
  return(var(y) * (length(y) - 1))
}

# Funktion zur Berechnung der Gini-Unreinheit für Klassifikation
calculate_gini <- function(y) {
  prob <- table(y) / length(y)
  return(sum(prob * (1 - prob)))
}

# Funktion zur Bestimmung des besten Splits (Greedy Algorithmus)
greedy_best_split <- function(X, y, type = "regression") {
  n <- nrow(X)
  p <- ncol(X)
  
  if (n == 0 || p == 0) {
    stop("X darf nicht leer sein")
  }
  
  if (length(y) != n) {
    stop("Die Länge von y muss der Anzahl der Zeilen von X entsprechen")
  }
  
  best_split <- list("variable" = NULL, "value" = NULL, "score" = Inf)
  
  for (j in 1:p) {
    unique_values <- unique(X[, j])
    if (length(unique_values) == 1) {
      next  # Kein Split möglich, wenn alle Werte gleich sind
    }
    for (s in unique_values) {
      left_indices <- which(X[, j] < s)
      right_indices <- which(X[, j] >= s)
      
      if (length(left_indices) > 0 && length(right_indices) > 0) {
        if (type == "regression") {
          left_variance <- calculate_variance(y[left_indices])
          right_variance <- calculate_variance(y[right_indices])
          if (!is.na(left_variance) && !is.na(right_variance)) {
            score <- (length(left_indices) * left_variance + length(right_indices) * right_variance) / n
          } else {
            score <- Inf
          }
        } else if (type == "classification") {
          left_gini <- calculate_gini(y[left_indices])
          right_gini <- calculate_gini(y[right_indices])
          score <- (length(left_indices) * left_gini + length(right_indices) * right_gini) / n
        }
        
        if (!is.na(score) && score < best_split$score) {
          best_split$variable <- j
          best_split$value <- s
          best_split$score <- score
        }
      }
    }
  }
  
  return(best_split)
}

greedy_cart <- function(data, depth = 0, max_depth = 5, type = "regression", num_split = 2, min_num = 1, m = NULL) {
  # Drehe die Matrix `data$x`, um die Dimensionen zu korrigieren
  X <- t(data$x)  # Transponiere die Matrix, um Beobachtungen als Zeilen zu haben
  y <- data$y
  
  n <- nrow(X)
  p <- ncol(X)
  
  # Sicherstellen, dass `m` korrekt gesetzt ist
  if (is.null(m) || m > p || m <= 0) {
    m <- p  # Wenn `m` nicht gesetzt ist oder ungültig ist, alle Merkmale verwenden
  }
  
  # Sicherstellen, dass depth und max_depth keine NULL-Werte sind
  if (is.null(depth)) depth <- 0
  if (is.null(max_depth)) max_depth <- Inf  # Unbegrenzte Tiefe, wenn max_depth nicht angegeben ist
  
  # Wenn n nicht definiert oder NA ist, beende mit einer Fehlermeldung
  if (is.na(n) || is.na(depth) || is.na(max_depth)) {
    stop("Ungültige Werte in n, depth oder max_depth.")
  }
  
  # Wenn alle y-Werte gleich sind oder maximale Tiefe erreicht ist, return Blattknoten
  if (length(unique(y)) == 1 || n <= 1 || depth >= max_depth) {
    return(list("value" = mean(y), "is_leaf" = TRUE))
  }
  
  # Zufällige Auswahl von `m` Merkmalen
  selected_features <- sample(1:p, m)
  
  # Überprüfe die Dimensionen von X und y vor dem Aufruf von greedy_best_split
  if (n != length(y)) {
    stop(paste("Dimension mismatch: nrow(X) =", n, "but length(y) =", length(y)))
  }
  
  best_split <- greedy_best_split(X[, selected_features, drop = FALSE], y, type)
  
  # Wenn kein Split gefunden wird (z.B. alle Werte sind gleich)
  if (is.null(best_split$variable)) {
    return(list("value" = mean(y), "is_leaf" = TRUE))
  }
  
  # Umwandlung der `best_split$variable` in den ursprünglichen Index
  best_split$variable <- selected_features[best_split$variable]
  
  # Teile die Daten basierend auf dem besten Split
  left_indices <- which(X[, best_split$variable] < best_split$value)
  right_indices <- which(X[, best_split$variable] >= best_split$value)
  
  left_data <- list(x = t(X[left_indices, , drop = FALSE]), y = y[left_indices])
  right_data <- list(x = t(X[right_indices, , drop = FALSE]), y = y[right_indices])
  
  # Rekursiv die linken und rechten Unterbäume erstellen
  left_tree <- greedy_cart(left_data, depth + 1, max_depth, type, num_split, min_num, m)
  right_tree <- greedy_cart(right_data, depth + 1, max_depth, type, num_split, min_num, m)
  
  # Rückgabe des Knotens mit seinen linken und rechten Kindern
  return(list(
    "variable" = best_split$variable,
    "value" = best_split$value,
    "left" = left_tree,
    "right" = right_tree,
    "is_leaf" = FALSE
  ))
}

# Funktion zum Formatieren und Drucken des Entscheidungsbaums
print_tree <- function(node, indent = "", split_value = NULL, direction = NULL) {
  if (!is.null(direction)) {
    cat(indent, direction, " (", split_value, ")\n", sep = "")
  }
  if (node$is_leaf) {
    cat(indent, "Leaf: ", node$value, "\n", sep = "")
  } else {
    cat(indent, "Split on variable ", node$variable, " at value ", node$value, "\n", sep = "")
    cat(indent, "Left:\n")
    print_tree(node$left, indent = paste0(indent, "  "), split_value = paste("<", node$value), direction = "Left")
    cat(indent, "Right:\n")
    print_tree(node$right, indent = paste0(indent, "  "), split_value = paste(">=", node$value), direction = "Right")
  }
}
