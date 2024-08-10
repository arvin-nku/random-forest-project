library(shiny)

#loade function
source("create_ran_sam.R")

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Shiny App for random forest"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
      sidebarPanel(
        numericInput("seed", "Random seed:", value = 123, min = 1),
        numericInput("size", "Sample size:", value = 100, min = 1),
        selectInput("type", "Sample type:", choices = c("Regression" = "reg", "Classification" = "cla"))
      ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("sampleTable")
        )
    )
)

# Define server logic required to draw a histogram
# Define server logic
server <- function(input, output) {
  output$sampleTable <- renderTable({
    print(input$seed)
    print(input$size)
    # Abhängig vom Typ der Stichprobe die passende Funktion aufrufen
    if (input$type == "reg") {
      data <- create_ran_sample_reg(input$seed, input$size)
      sample_data <- data.frame(x1 = data$x[1, ], x2 = data$x[2, ], y = data$y)
    } else {
      data <- create_ran_sample_cla(input$seed, input$size)
      sample_data <- data.frame(x1 = data$x[1, ], x2 = data$x[2, ], y = factor(data$y))
    }
    
    # Rückgabe der Daten als Tabelle
    str(sample_data)
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
