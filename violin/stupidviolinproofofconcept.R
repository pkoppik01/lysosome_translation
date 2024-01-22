library(shiny)
library(ggplot2)
library(dplyr)

# Define the UI
ui <- fluidPage(
  titlePanel("Nested Violin Plot with Shiny"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("dataFile", "Upload a CSV file"),
      actionButton("plotButton", "Generate Plot")
    ),
    
    mainPanel(
      plotOutput("violinPlot")
    )
  )
)

# Define the server
server <- function(input, output) {
  data <- reactive({
    req(input$dataFile)
    read.csv(input$dataFile$datapath, header = TRUE)
  })
  
  observeEvent(input$plotButton, {
    output$violinPlot <- renderPlot({
      req(data())
      df <- data()
      df_long <- df %>%
        gather(key = "Condition", value = "Value")
      
      ggplot(df_long, aes(x = Condition, y = Value, fill = Condition)) +
        geom_violin(trim = FALSE) +
        geom_boxplot(width = 0.2, fill = "white", color = "black", outlier.shape = NA) +
        geom_point(stat = "summary", fun.y = "median", color = "red", size = 3, shape = 5) +
        labs(title = "Nested Violin Plot with Medians") +
        theme_minimal()
    })
  }
}

# Run the Shiny app
shinyApp(ui = ui, server = server)

