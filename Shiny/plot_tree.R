#help function

update_split <- function(data){
  for (i in 1:nrow(data$tree)) {
    node_cur <- data$tree[i, ]
    child_l <- data$tree %>% filter(node == node_cur$node * 2)
    
    if(nrow(child_l) > 0){
      data$tree[i, ]$split_point <- child_l$split_point
      data$tree[i, ]$split_index <- child_l$split_index
    }
  }
  data$tree[data$tree$name == "leaf", ]$split_point <- NA
  data$tree[data$tree$name == "leaf", ]$split_index <- NA
  
  return(data)
}

tree_plot <- function(data){
  data <- update_split(data)
  
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
  data.tree::SetGraphStyle(tree, rankdir = "TB")
  data.tree::SetEdgeStyle(tree, arrowhead="normal", arrowsize=0.3)
  data.tree::SetNodeStyle(tree, style = "box", shape = "box", height=0.05, fontsize=8)
  plot(tree)
}

install.packages("data.tree")
install.packages("DiagrammeR")
library(data.tree)
library(dplyr)
library(DiagrammeR)
tree_plot(val) 
