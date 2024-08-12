library(shiny)

#loade function
source("create_ran_sam.R")

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
      h4("Predicvalues"),
      textInput("values", "Input Values (comma separated):", value = "0")
    )
  ),
  
  # Buttons for actions
  fluidRow(
    column(
      width = 12,
      actionButton("predict", "Make Prediction", class = "btn-primary"),
    )
  ),
  
  # Main panel for displaying the plot
  mainPanel(
    fluidRow(column(width = 12, algin = "center", h2(textOutput("caption1")))),
    fluidRow(column(width = 12, algin = "center", h2(textOutput("prediction"))))
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  observeEvent(input$predict, {
    
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
    output$caption1 <- renderText({
      "Prediction"
    })
    
    #choose algorithem
    switch(algo,
           
           "greedy_cart" = {
             if (type == "reg") {
               data <- create_ran_sample_reg(seed, size)
               #data_greedy <- RandomForestPackage::greedy_cart_regression(data, num_leaf, depth, num_split, min_num, m)
               data_greedy <- greedy_cart_regression(data, num_leaf, depth, num_split, min_num, m)
               
               print(data_greedy$tree)
          
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
               data_rf <- random_forest_regression(data, B = B, A = A)
               
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
}

# Run the application 
shinyApp(ui = ui, server = server)
