
if (!require(shiny)) install.packages("shiny")
if (!require(dplyr)) install.packages("dplyr")
if (!require(DT)) install.packages("DT")

library(shiny)
library(dplyr)
library(DT)

ui <- fluidPage(
  titlePanel("Median Column Addition App"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Choose CSV File"),
      selectInput("num_columns", "Select Number of Columns", choices = 1:10),
      uiOutput("column_inputs"),
      actionButton("go", "Go"),
      downloadButton("downloadCSV", "Download Transformed CSV")
    ),
    
    mainPanel(
      h4("Median Columns Included"),
      textOutput("message"),
      DTOutput("table")
    )
  )
)

server <- function(input, output, session) {
  transformed_data <- reactiveVal(NULL)
  numerical_column_names <- reactiveVal(NULL) 
  median_columns <- reactiveVal(NULL)
  
  observeEvent(input$file, {
    file <- input$file
    if (!is.null(file)) {
      data <- read.csv(file$datapath)
      numerical_columns <- sapply(data, is.numeric)
      numerical_column_names(names(data)[numerical_columns])
      updateSelectInput(session, "num_columns", choices = 1:length(numerical_column_names()))
    }
  })
  
  observe({
    num_columns <- as.integer(input$num_columns)
    column_inputs <- lapply(1:num_columns, function(i) {
      selectInput(paste0("column_", i), paste("Select Column", i), choices = numerical_column_names())
    })
    output$column_inputs <- renderUI({ column_inputs })
  })
  
  observeEvent(input$go, {
    file <- input$file
    num_columns <- as.integer(input$num_columns)
    selected_columns <- character(num_columns)
    
    for (i in 1:num_columns) {
      selected_columns[i] <- input[[paste0("column_", i)]]
    }
    
    if (is.null(file) || any(nchar(selected_columns) == 0)) {
      output$message <- renderText("Please select a CSV file and select values for all columns.")
      return()
    }
    
    tryCatch({
      data <- read.csv(file$datapath)
      
      if (!all(selected_columns %in% colnames(data))) {
        output$message <- renderText("One or more selected columns not found in the CSV file.")
        return()
      }
      
      median_column_name <- paste("medians_", paste(selected_columns, collapse = "_"))
      data[median_column_name] <- apply(data[, selected_columns], 1, median)
      transformed_data(data)
      
      # Store the median column names for later download
      median_columns(c(median_columns(), median_column_name))
      
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
      filename <- paste("mediansIncluded_", paste(median_columns(), collapse = "_"), "_", Sys.Date(), ".csv", sep = "")
      return(filename)
    },
    content = function(file) {
      write.csv(transformed_data(), file)
    }
  )
}

shinyApp(ui, server)
