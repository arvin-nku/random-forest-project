library(data.tree)
library(dplyr)
library(DiagrammeR)

tree_plot <- function(data){
  if (!"split_point" %in% colnames(data$tree) || !"split_index" %in% colnames(data$tree)) {
    warning("split_point oder split_index fehlen in den Daten.")
    return(NULL)
  }
  
  parent <- c()
  child <- c()
  
  for (i in 1:nrow(data$tree)) {
    node_cur <- data$tree[i, ]
    
    child_l <- data$tree %>% filter(node == node_cur$node*2)
    child_r <- data$tree %>% filter(node == (node_cur$node*2)+1)
    
    if(nrow(child_l) > 0){
      parent <- c(parent, paste0("Position: ", node_cur$node, "\n", "j = ",
                                 node_cur$split_index, ", s = ", round(node_cur$split_point, digits = 2))
      )
      if(child_l$name == "leaf"){
        child <- c(child, paste0("Position: ", child_l$node, "\n", "y = ",
                                 round(child_l$c_value, digits = 2))
        )
      } else {
        child <- c(child, paste0("Position: ", child_l$node, "\n", "j = ",
                                 child_l$split_index, ", s = ", round(child_l$split_point, digits = 2)))
      }
    }
    
    if(nrow(child_r) > 0){
      parent <- c(parent, paste0("Position: ", node_cur$node, "\n", "j = ",
                                 node_cur$split_index, ", s = ", round(node_cur$split_point, digits = 2))
      )
      if(child_r$name == "leaf"){
        child <- c(child, paste0("Position: ", child_r$node, "\n", "y = ",
                                 round(child_r$c_value, digits = 2))
        )
      } else {
        child <- c(child, paste0("Position: ", child_r$node, "\n", "j = ",
                                 child_r$split_index, ", s = ", round(child_r$split_point, digits = 2)))
      }
    }
  }
  
  tree_data <- data.frame(parent = parent, child = child)
  tree <- data.tree::FromDataFrameNetwork(tree_data)
  #set graphic
  SetGraphStyle(tree, rankdir = "TB")
  SetEdgeStyle(tree, arrowhead = "vee", arrowsize = 0.5, color = "black")
  SetNodeStyle(tree, style = "filled",fillcolor = "#E0E0E0", shape = "box", height = 0.1, fontsize = 10)
  
  plot(tree)
}

tree_plot_random_forest <- function(data) {
  
  parent <- c()
  child <- c()
  
  for (i in 1:nrow(data)) {
    node_cur <- data[i, ]
    
    child_l <- data %>% filter(node == node_cur$node*2)
    child_r <- data %>% filter(node == (node_cur$node*2)+1)
    
    if(nrow(child_l) > 0){
      parent <- c(parent, paste0("Position: ", node_cur$node, "\n", "j = ",
                                 node_cur$split_index, ", s = ", round(node_cur$split_point, digits = 2))
      )
      if(child_l$name == "leaf"){
        child <- c(child, paste0("Position: ", child_l$node, "\n", "y = ",
                                 round(child_l$c_value, digits = 2))
        )
      } else {
        child <- c(child, paste0("Position: ", child_l$node, "\n", "j = ",
                                 child_l$split_index, ", s = ", round(child_l$split_point, digits = 2)))
      }
    }
    
    if(nrow(child_r) > 0){
      parent <- c(parent, paste0("Position: ", node_cur$node, "\n", "j = ",
                                 node_cur$split_index, ", s = ", round(node_cur$split_point, digits = 2))
      )
      if(child_r$name == "leaf"){
        child <- c(child, paste0("Position: ", child_r$node, "\n", "y = ",
                                 round(child_r$c_value, digits = 2))
        )
      } else {
        child <- c(child, paste0("Position: ", child_r$node, "\n", "j = ",
                                 child_r$split_index, ", s = ", round(child_r$split_point, digits = 2)))
      }
    }
  }
  
  tree_data <- data.frame(parent = parent, child = child)
  tree <- FromDataFrameNetwork(tree_data)
  
  #set graphic
  SetGraphStyle(tree, rankdir = "TB")
  SetEdgeStyle(tree, arrowhead = "vee", arrowsize = 0.5, color = "black")
  SetNodeStyle(tree, style = "filled",fillcolor = "#E0E0E0", shape = "box", height = 0.1, fontsize = 10)
  
  #greate plot
  plot(tree)
}
