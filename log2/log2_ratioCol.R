# Install and load the required packages
if (!require(shiny)) install.packages("shiny")
if (!require(dplyr)) install.packages("dplyr")
if (!require(DT)) install.packages("DT")

library(shiny)
library(dplyr)
library(DT)

# Define the UI
ui <- fluidPage(
  titlePanel("Log2 Ratio Transformation App"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Choose CSV File"),
      selectInput("numerator", "Select Numerator Column", choices = NULL),
      selectInput("denominator", "Select Denominator Column", choices = NULL),
      actionButton("go", "Go"),
      downloadButton("downloadCSV", "Download Transformed CSV")
    ),
    
    mainPanel(
      h4("Transformation Result"),
      textOutput("message"),
      DTOutput("table")
    )
  )
)

# Define the server logic
server <- function(input, output, session) {
  transformed_data <- reactiveVal(NULL)
  
  observeEvent(input$file, {
    file <- input$file
    if (!is.null(file)) {
      data <- read.csv(file$datapath)
      numerical_columns <- sapply(data, is.numeric)
      numerical_column_names <- names(data)[numerical_columns]
      updateSelectInput(session, "numerator", choices = numerical_column_names)
      updateSelectInput(session, "denominator", choices = numerical_column_names)
    }
  })
  
  observeEvent(input$go, {
    file <- input$file
    numerator_column <- input$numerator
    denominator_column <- input$denominator
    
    if (is.null(file) || nchar(numerator_column) == 0 || nchar(denominator_column) == 0) {
      output$message <- renderText("Please select a CSV file and enter both numerator and denominator columns.")
      return()
    }
    
    tryCatch({
      data <- read.csv(file$datapath)
      
      if (!(numerator_column %in% colnames(data)) || !(denominator_column %in% colnames(data))) {
        output$message <- renderText("One or both selected columns not found in the CSV file.")
        return()
      }
      
      # Calculate the log2 ratio
      log2_ratio_column <- paste(numerator_column, "/", denominator_column)
      data[log2_ratio_column] <- log2(data[[numerator_column]] / data[[denominator_column]])
      transformed_data(data)
      
      output$table <- renderDT({
        datatable(transformed_data())
      })
      
      output$message <- renderText("Transformation complete.")
    }, error = function(e) {
      output$message <- renderText(paste("Error:", e$message))
    })
  })
  
  output$downloadCSV <- downloadHandler(
    filename = function() {
      filename <- paste("log2_", input$numerator, "_", input$denominator, "_", Sys.Date(), ".csv", sep = "")
      return(filename)
    },
    content = function(file) {
      write.csv(transformed_data(), file)
    }
  )
}

# Create a Shiny app
shinyApp(ui, server)
