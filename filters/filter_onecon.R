# Install and load the required packages
if (!require(shiny)) install.packages("shiny")
if (!require(dplyr)) install.packages("dplyr")
if (!require(DT)) install.packages("DT")

library(shiny)
library(dplyr)
library(DT)

# Define the UI
ui <- fluidPage(
  titlePanel("CSV Data Filter App"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Choose CSV File"),
      selectInput("col", "Select Column", choices = NULL),
      textInput("value", "Filter Value (Numeric or Exponent Notation)", value = ""),
      radioButtons("op", "Select Operation:",
                   choices = c("Greater Than" = "gt", "Less Than" = "lt"),
                   selected = "gt"),
      actionButton("filterButton", "Apply Filters"),
      downloadButton("downloadCSV", "Download Filtered CSV")
    ),
    
    mainPanel(
      h4("Filtered Data"),
      DTOutput("table")
    )
  )
)

# Define the server logic
server <- function(input, output, session) {
  filtered_data <- reactiveVal(NULL)
  
  observeEvent(input$file, {
    file <- input$file
    if (!is.null(file)) {
      data <- read.csv(file$datapath)
      numerical_columns <- sapply(data, is.numeric)
      numerical_column_names <- names(data)[numerical_columns]
      updateSelectInput(session, "col", choices = numerical_column_names)
    }
  })
  
  observeEvent(input$filterButton, {
    file <- input$file
    col <- input$col
    value_text <- input$value
    op <- input$op
    
    if (is.null(file) || nchar(col) == 0 || nchar(value_text) == 0) {
      output$message <- renderText("Please select a CSV file, a column, and enter a filter value.")
      return()
    }
    
    tryCatch({
      data <- read.csv(file$datapath)
      
      if (!(col %in% colnames(data))) {
        output$message <- renderText("Selected column not found in the CSV file.")
        return()
      }
      
      # Parse numeric values or exponent notation
      value <- eval(parse(text = gsub("\\^", "**", value_text)))
      
      # Handle NA values from failed conversion
      if (is.na(value)) {
        output$message <- renderText("Invalid filter value format. Please use numeric or exponent notation.")
        return()
      }
      
      filtered <- data %>%
        filter(
          (.data[[col]] >= value & op == "gt") | (.data[[col]] <= value & op == "lt")
        )
      
      filtered_data(filtered)
      
      output$table <- renderDT({
        datatable(filtered_data())
      })
      
      output$message <- renderText("Filters applied successfully.")
    }, error = function(e) {
      output$message <- renderText(paste("Error:", e$message))
    })
  })
  
  output$downloadCSV <- downloadHandler(
    filename = function() {
      file_name <- input$file$name
      file_extension <- tools::file_ext(file_name)
      base_name <- tools::file_path_sans_ext(file_name)
      filtered_file_name <- paste0("filtered_", base_name, "_", Sys.Date(), ".", file_extension)
      return(filtered_file_name)
    },
    content = function(file) {
      write.csv(filtered_data(), file)
    }
  )
}

# Create a Shiny app
shinyApp(ui, server)
