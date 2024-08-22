library(shiny)
library(dplyr)
library(data.tree)
library(DiagrammeR)
library(ggplot2)

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
        numericInput("depth", "Depth of the tree:", value = 5, min = 1),
        numericInput("num_split", "Split nodes with at least elements:", value = 2, min = 2),
        numericInput("min_num", "Only splits nodes with at least child nodes:", value = 1, min = 1),
        numericInput("m", "Number of coordinates used in each iteration:", value = 1, min = 1),
        conditionalPanel(
          condition = "input.Algorithm != 'greedy_cart'",
          numericInput("B", "Number of Bootstrap samples:", value = 10, min = 1),
          numericInput("A", "Number of bootstrap samples to be used:", value = 5, min = 1)
        )
      )
    )
  ),
  
  # Input for Estimated values
  column(
    width = 4,
    wellPanel(
      h4("Estimated values"),
      textInput("values", "Input Values (comma separated):", value = "0")
    )
  ),
  
  # Input for choosing a tree (hidden for Greedy CART)
  conditionalPanel(
    condition = "input.Algorithm != 'greedy_cart'",
    column(
      width = 4,
      wellPanel(
        h4("Choose a tree"),
        textInput("tree_num", "Which tree do you want to see:", value = "1")
      )
    )
  ),
  
  # Buttons for actions
  fluidRow(
    column(
      width = 12,
      actionButton("predict", "Make a prediction", class = "btn-primary"),
      actionButton("plot", "Make a plot of a tree", class = "btn-primary"),
      actionButton("bagging", "Show test for bagging", class = "btn-primary")
    )
  ),
  
  # Main panel for displaying the plot
  mainPanel(
    fluidRow(column(width = 12, align = "center", h2(textOutput("caption")))),
    fluidRow(column(width = 12, align = "center", h2(textOutput("prediction")))),
    fluidRow(column(width = 12, align = "center", grVizOutput("plot", width = "100%", height = "100%")))
  )
)

# Define server logic
server <- function(input, output) {
  
  observeEvent(input$predict, {
    
    output$plot <- renderGrViz({
      NULL
    })
    
    output$prediction <- renderText({
      NULL
    })
    
    # Input
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
    
    # data_x as matrix for prediction
    data_values <- as.numeric(strsplit(input$values, ",")[[1]])
    data_x <- matrix(data_values, nrow = 2, byrow = TRUE)
    
    # Caption for Output
    output$caption <- renderText({
      "Prediction"
    })
    
    # Choose algorithm
    switch(algo,
           
           "greedy_cart" = {
             if (type == "reg") {
               data <- create_ran_sample_reg(seed, size)
               data_greedy <- greedy_cart_regression(data, num_leaf, depth, num_split, min_num, m)
               
               pred_value <- prediction(list(data_greedy$tree), data_x, "reg")
               
               output$prediction <- renderText({
                 paste("Predicted Values Greedy CART:", paste(pred_value, collapse = ", "))
               })
               
             } else if (type == "cla") {
               data <- create_ran_sample_cla(seed, size)
               data_greedy <- greedy_cart_classification(data, num_leaf, depth, num_split, min_num, m)
               
               pred_value <- prediction(list(data_greedy$tree), data_x, "cla")
               
               output$prediction <- renderText({
                 paste("Predicted Values Greedy CART:", paste(pred_value, collapse = ", "))
               })
               
             }
             
           },
           
           "bagging" = {
             if (type == "reg") {
               data <- create_ran_sample_reg(seed, size)
               data_bagging <- bagging_regression(data$x, data$y, B)
               
               pred_value <- prediction(data_bagging, data_x, "reg")
               output$prediction <- renderText({
                 paste("Predicted Values Bagging:", paste(pred_value, collapse = ", "))
               })
               
             } else if (type == "cla") {
               data <- create_ran_sample_cla(seed, size)
               data_bagging <- bagging_classification(data$x, data$y, B)
               
               pred_value <- prediction(data_bagging, data_x, "cla")
               output$prediction <- renderText({
                 paste("Predicted Values Bagging:", paste(pred_value, collapse = ", "))
               })
               
             }
             
           },
           
           "random_forest" = {
             if (type == "reg") {
               data <- create_ran_sample_reg(seed, size)
               data_rf <- random_forest_regression(data, B = B, A = A)
               
               pred_value <- prediction(data_rf, data_x, "reg")
               
               output$prediction <- renderText({
                 paste("Predicted Values Random Forest:", paste(pred_value, collapse = ", "))
               })
               
               
             } else if (type == "cla") {
               data <- create_ran_sample_cla(seed, size)
               data_rf <- random_forest_classification(data, B = B, A = A)
               pred_value <- prediction(data_rf, data_x, "cla")
               
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
    
    # Caption for Output
    output$caption <- renderText({
      "Plot:"
    })
    
    # Choose algorithm
    switch(algo,
           
           "greedy_cart" = {
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
               data_bagging <- bagging_regression(data$x, data$y, B)
               output$plot <- renderGrViz({
                 tree_plot_random_forest(data_bagging[[tree_num]])
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
}

# Run the application 
shinyApp(ui = ui, server = server)
