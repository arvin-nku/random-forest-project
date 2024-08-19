library(shiny)
library(dplyr)
library(data.tree)
library(DiagrammeR)
library(ggplot2)

#loade function
source("create_ran_sam.R")
source("plot_tree.R")
source("alg1.R")
source("alg2.R")

# Define UI for application that draws a histogram
# Define UI for application
ui <- fluidPage(
  
  # Application title
  titlePanel("Shiny App for Random Forest"),
  
  # Layout with panels for inputs
  fluidRow(
    column(
      width = 4,
      wellPanel( 
        h4("Settings"),
        numericInput("seed", "Random Seed:", value = 123, min = 1),
        selectInput("Algorithm", "Algorithm Type:",
                    choices = c("Greedy CART" = "greedy_cart", 
                                "Bagging" = "bagging", 
                                "Random Forest" = "random_forest"))
      )
    ),
    column(
      width = 4,
      wellPanel(
        h4("Create Sample"),
        numericInput("size", "Sample Size:", value = 50, min = 1),
        selectInput("type", "Sample Type:",
                    choices = c("Regression" = "reg", 
                                "Classification" = "cla"))
      )
    ),
    column(
      width = 4,
      wellPanel(
        h4("Settings for Algorithm"),
        numericInput("num_leaf", "Number of leaves:", value = 10, min = 1),
        numericInput("depth", "Depth of the tree:", value = 5, min = 0),
        numericInput("num_split", "Split nodes with at least elements:", value = 2, min = 2),
        numericInput("min_num", "Only splits nodes with at least child nodes:", value = 1, min = 1),
        numericInput("m", "Number of coordinates used in each iteration:", value = 1, min = 1),
        numericInput("B", "Number of Bootstrap samples:", value = 10, min = 1),
        numericInput("A", "Number of boostrap samples to be used", value = 5, min = 1)
      )
    )
  ),
  
  column(
    width = 4,
    wellPanel(
      h4("Estimated values"),
      textInput("values", "Input Values (comma separated):", value = "0")
    )
  ),
  column(
    width = 4,
    wellPanel(
      h4("Choose a tree"),
      textInput("tree_num", "Which tree do you want to see (Baggin/Random Forest genrate more than one tree):", value = "1")
    )
  ),
  
  # Buttons for actionsx
  fluidRow(
    column(
      width = 12,
      actionButton("predict", "Make a prediction", class = "btn-primary"),
      actionButton("plot", "Make a plot of a tree", class = "btn-primary"),
      actionButton("test", "Show test for bagging", class = "btn-primary"),
    )
  ),
  
  # Main panel for displaying the plot
  mainPanel(
    fluidRow(column(width = 12, align = "center", h2(textOutput("caption")))),
    fluidRow(column(width = 12, align = "center", h2(textOutput("prediction")))),
    fluidRow(column(width = 12, align = "center", grVizOutput("plot", width = "100%", height = "100%")))
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  observeEvent(input$predict, {
    
    output$plot <- renderGrViz({
      NULL
    })
    
    output$prediction <- renderText({
      NULL
    })
    
    #Input
    algo <- input$Algorithm
    type <- input$type
    num_leaf <- input$num_leaf
    depth <- input$depth
    num_split <- input$num_split
    min_num <- input$min_num
    m <- input$m
    B <- input$B
    A <- input$A
    seed <- input$seed
    size <- input$size
    #list_x as matrix for perdiction()
    list_x <- strsplit(input$values, ",")[[1]]
    list_x <- as.numeric(trimws(list_x))
    list_x_matrix <- matrix(list_x, nrow = 2)
    
    #Caption for Output
    output$caption <- renderText({
      "Prediction"
    })
    
    #choose algorithem
    switch(algo,
           
           "greedy_cart" = {
             if (type == "reg") {
               data <- create_ran_sample_reg(seed, size)
               #data_greedy <- RandomForestPackage::greedy_cart_regression(data, num_leaf, depth, num_split, min_num, m)
               data_greedy <- greedy_cart_regression(data, num_leaf, depth, num_split, min_num, m)
               
          
               pred_value <- prediction(data_greedy, list_x_matrix, "reg")
               
               output$prediction <- renderText({
                 paste("Predicted Values Greedy CART:", paste(pred_value, collapse = ", "))
               })
               
             } else if (type == "cla") {
               data <- create_ran_sample_cla(seed, size)
               #data_greedy <- RandomForestPackage::greedy_cart_classification(data, num_leaf, depth, num_split, min_num, m)
               data_greedy <- greedy_cart_classification(data, num_leaf, depth, num_split, min_num, m)
               
               pred_value <- prediction(data_greedy, list_x_matrix, "reg")
               
               output$prediction <- renderText({
                 paste("Predicted Values Greedy CART:", paste(pred_value, collapse = ", "))
               })
               
             }
             
           },
           
           "bagging" = {
             if (type == "reg") {
               data <- create_ran_sample_reg(seed, size)
               #data_bagging <- RandomForestPackage::bagging_regression(data$x, data$y, B)
               data_bagging <- bagging_regression(data$x, data$y, B)
               
               pred_value <- prediction(data_bagging, list_x_matrix, "reg")
               output$prediction <- renderText({
                 paste("Predicted Values Bagging:", paste(pred_value, collapse = ", "))
               })
               
             } else if (type == "cla") {
               data <- create_ran_sample_cla(seed, size)
               #data_bagging <- RandomForestPackage::bagging_classification(data$x, data$y, B)
               data_bagging <- bagging_classification(data$x, data$y, B)
               
               pred_value <- prediction(data_bagging, list_x_matrix, "reg")
               output$prediction <- renderText({
                 paste("Predicted Values Bagging:", paste(pred_value, collapse = ", "))
               })
               
             }
             
           },
           
           "random_forest" = {
             if (type == "reg") {
               data <- create_ran_sample_reg(seed, size)
               #data_rf <- RandomForestPackage::random_forest_regression(data, B)
               
               data_rf <- random_forest_regression(data = data, B = B, A = A, m = m,
                                                   num_leaf = num_leaf, depth = depth,
                                                   num_split = num_split, min_num = min_num)
               pred_value <- prediction(data_rf, list_x_matrix, "reg")
        
               output$prediction <- renderText({
                 paste("Predicted Values Random Forest:", paste(pred_value, collapse = ", "))
               })
               
               
             } else if (type == "cla") {
               data <- create_ran_sample_cla(seed, size)
               #data_rf <- RandomForestPackage::random_forest_classification(data, B)
               data_rf <- random_forest_classification(data, B = B, A = A)
               pred_value <- prediction(data_rf, list_x_matrix, "cla")
               
               output$prediction <- renderText({
                 paste("Predicted Values Random Forest:", paste(pred_value, collapse = ", "))
               })
               
             }
             
           }
    )
    
  })
  
  observeEvent(input$plot, {
    
    output$plot <- renderGrViz({
      NULL
    })
    
    output$prediction <- renderText({
      NULL
    })
    
    algo <- input$Algorithm
    type <- input$type
    num_leaf <- input$num_leaf
    depth <- input$depth
    num_split <- input$num_split
    min_num <- input$min_num
    m <- input$m
    B <- input$B
    A <- input$A
    seed <- input$seed
    size <- input$size
    tree_num <- as.integer(input$tree_num)
    
    if(tree_num > B){
      warning(paste("Chosen tree must be less or equal to", B, ". Tree is set to 1"))
      tree_num <- 1
    }
    
    # Caption for Output
    output$caption <- renderText({
      "Plot:"
    })
    
    # Choose algorithm
    switch(algo,
           
           "greedy_cart" = {
             if(tree_num > 1){
               warning(paste("Only one tree is generated by Greedy CART. Tree is set to 1"))
               tree_num <- 1
             }
             if (type == "reg") {
               data <- create_ran_sample_reg(seed, size)
               data_greedy <- greedy_cart_regression(data, num_leaf, depth, num_split, min_num, m)
               output$plot <- renderGrViz({
                 tree_plot(data_greedy)
               })
             } else if (type == "cla") {
               data <- create_ran_sample_cla(seed, size)
               data_greedy <- greedy_cart_classification(data, num_leaf, depth, num_split, min_num, m)
               output$plot <- renderGrViz({
                 tree_plot(data_greedy)
               })
             }
           },
           
           "bagging" = {
             if (type == "reg") {
               data <- create_ran_sample_reg(seed, size)
               #(bagging_regression)
               data_bagging <- bagging_regression(data$x, data$y, B)
               output$plot <- renderGrViz({
                 plot(data$x, data$y, pch = 21, bg = 'lightgrey', main = "Bagging regression with 1 tree", xlab = "x1", ylab = "y")
                 #tree_plot_random_forest(data_bagging[[tree_num]])
               })
             } else if (type == "cla") {
               data <- create_ran_sample_cla(seed, size)
               data_bagging <- bagging_classification(data$x, data$y, B)
               output$plot <- renderGrViz({
                 tree_plot_random_forest(data_bagging[[tree_num]])
               })
             }
           },
           
           "random_forest" = {
             if (type == "reg") {
               data <- create_ran_sample_reg(seed, size)
               data_rf <- random_forest_regression(data, B = B, A = A)
               output$plot <- renderGrViz({
                 tree_plot_random_forest(data_rf[[tree_num]])
               })
             } else if (type == "cla") {
               data <- create_ran_sample_cla(seed, size)
               data_rf <- random_forest_classification(data, B = B, A = A)
               output$plot <- renderGrViz({
                 tree_plot_random_forest(data_rf[[tree_num]])
               })
             }
           }
    )
  })
  
  observeEvent(input$test, {
    algo <- input$Algorithm
    type <- input$type
    output$plot <- renderGrViz({
      NULL
    })
    if (algo == "bagging")
    {
    if (type == "reg")
    {
      print("sdfsdf")
      set.seed(123)
      n <- 200
      x <- runif(n, 0, 1)
      y <- sin(2 * pi * x) + rnorm(n, 0, 0.1)
      data <- list(x = matrix(x, nrow = 1, byrow = TRUE), y = y)

      X <- data$x
      Y <- data$y

      single_model <- greedy_cart_regression(data = data, num_split = 5)
      print(single_model$tree)
      pred_single <- bagging_regression_prediction(list(single_model), X)
      print("sdfsdf")
      output$plot <- renderGrViz({
        par(mfrow = c(2, 2))
        plot(data$x, data$y,pch = 21, bg = 'lightgrey', main = "Bagging regression with 1 tree", xlab = "x1", ylab = "y")
        
        lines(sort(data$x), pred_single[order(data$x)], col = 'blue')
        lines(sort(data$x), sin(2 * pi * sort(data$x)), col = 'red')
        #debug(bagging_regression)
        models <- bagging_regression(X, Y, B = 10)
        
        pred_bagging <- bagging_regression_prediction(models, X)
        print("sdfsdf")
        plot(data$x, data$y, pch = 21, bg = 'lightgrey', main = "Bagging regression with 10 trees", xlab = "x1", ylab = "y")
        lines(sort(data$x), pred_bagging[order(data$x)], col = 'blue')
        lines(sort(data$x), sin(2 * pi * sort(data$x)), col = 'red')
        par(mfrow = c(1, 1))
      })
    }
    
    if (type == "cla")
    {
      set.seed(123)
      n <- 100
      x1 <- runif(n, 0, 1)
      x2 <- runif(n, 0, 1)
      y <- ifelse(sin(2 * pi * x1) + rnorm(n, 0, 0.1) > x2, 1, 2)
      data <- list(x = matrix(c(x1, x2), nrow = 2, byrow = T), y = as.numeric(as.factor(y)))
      X = data$x
      Y = data$y
      plot_data <- data.frame(x1 = x1, x2 = x2, y = as.numeric(as.factor(y)))
      models <- bagging_classification(X, Y, B = 5, x_vector = c(x1, x2))
      plot1 <- plot_decision_boundary(plot_data, models, "Bagging classification with B = 5 trees")
      
      
      one_tree <- bagging_classification(X, Y, B = 1, x_vector = c(x1, x2))
      plot2 <- plot_decision_boundary(plot_data, one_tree, "Bagging classification with B = 1 tree")
      output$plot <- renderGrViz({
       #plot_grid(plot1, plot2, ncol = 1)
        plot(plot_grid(plot1, plot2, ncol = 1))
        #plot1
      })
    }
    }
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

