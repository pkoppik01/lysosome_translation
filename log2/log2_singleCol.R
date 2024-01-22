# Install and load the required packages
if (!require(shiny)) install.packages("shiny")
if (!require(dplyr)) install.packages("dplyr")
if (!require(DT)) install.packages("DT")

library(shiny)
library(dplyr)
library(DT)

# Define the UI
ui <- fluidPage(
  titlePanel("Log2 Transformation App"),  # Update the app title
  
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Choose CSV File"),
      selectizeInput("columns", "Select Numerical Columns", choices = NULL, multiple = TRUE),
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
  options(shiny.maxRequestSize=30*1024^2)
  transformed_data <- reactiveVal(NULL)
  
  observeEvent(input$file, {
    file <- input$file
    if (!is.null(file)) {
      data <- read.csv(file$datapath, na.strings = c("", "NA", "NULL"))
      numerical_columns <- sapply(data, is.numeric)
      numerical_column_names <- names(data)[numerical_columns]
      updateSelectizeInput(session, "columns", choices = numerical_column_names)
    }
  })
  
  observeEvent(input$go, {
    file <- input$file
    selected_columns <- input$columns
    
    if (is.null(file) || length(selected_columns) == 0) {
      output$message <- renderText("Please select a CSV file and at least one column.")
      return()
    }
    
    tryCatch({
      data <- read.csv(file$datapath, na.strings = c("", "NA", "NULL"))
      
      missing_columns <- setdiff(selected_columns, colnames(data))
      if (length(missing_columns) > 0) {
        output$message <- renderText(paste("Columns not found: ", paste(missing_columns, collapse = ", ")))
        return()
      }
      
      # Perform the log2 transformation for selected columns
      for (col in selected_columns) {
        data[paste(col, "_log2", sep = "")] <- log2(data[[col]])
      }
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
      filename <- paste("log2_", paste(input$columns, collapse = "_"), "_", Sys.Date(), ".csv", sep = "")
      return(filename)
    },
    content = function(file) {
      write.csv(transformed_data(), file)
    }
  )
}


# Create a Shiny app
shinyApp(ui, server)
