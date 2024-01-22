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
            selectInput("col1", "Select Column 1", choices = NULL),
            textInput("value1", "Filter Value for Column 1 (Numeric or Exponent Notation)", value = ""),
            radioButtons("op1", "Select Operation for Column 1:",
                         choices = c("Greater Than" = "gt", "Less Than" = "lt"),
                         selected = "gt"),
            selectInput("col2", "Select Column 2", choices = NULL),
            textInput("value2", "Filter Value for Column 2 (Numeric or Exponent Notation)", value = ""),
            radioButtons("op2", "Select Operation for Column 2:",
                         choices = c("Greater Than" = "gt", "Less Than" = "lt"),
                         selected = "gt"),
            radioButtons("condition", "Filter Condition:",
                         choices = c("AND" = "and", "OR" = "or"),
                         selected = "and"),
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
            updateSelectInput(session, "col1", choices = numerical_column_names)
            updateSelectInput(session, "col2", choices = numerical_column_names)
        }
    })
    
    observeEvent(input$filterButton, {
        file <- input$file
        col1 <- input$col1
        value1_text <- input$value1
        op1 <- input$op1
        col2 <- input$col2
        value2_text <- input$value2
        op2 <- input$op2
        condition <- input$condition # Get the selected condition (AND or OR)
        
        if (is.null(file) || nchar(col1) == 0 || nchar(col2) == 0 || nchar(value1_text) == 0 || nchar(value2_text) == 0) {
            output$message <- renderText("Please select a CSV file, both columns, and enter filter values.")
            return()
        }
        
        tryCatch({
            data <- read.csv(file$datapath)
            
            if (!(col1 %in% colnames(data)) || !(col2 %in% colnames(data))) {
                output$message <- renderText("One or both selected columns not found in the CSV file.")
                return()
            }
            
            # Parse numeric values or exponent notation
            value1 <- eval(parse(text = gsub("\\^", "**", value1_text)))
            value2 <- eval(parse(text = gsub("\\^", "**", value2_text)))
            
            # Handle NA values from failed conversion
            if (is.na(value1) || is.na(value2)) {
                output$message <- renderText("Invalid filter value format. Please use numeric or exponent notation.")
                return()
            }
            
            filtered <- data
            if (condition == "and") {
                filtered <- filtered %>%
                    filter(
                        (.data[[col1]] >= value1 & op1 == "gt") | (.data[[col1]] <= value1 & op1 == "lt"),
                        (.data[[col2]] >= value2 & op2 == "gt") | (.data[[col2]] <= value2 & op2 == "lt")
                    )
            } else {
                filtered <- filtered %>%
                    filter(
                        (.data[[col1]] >= value1 & op1 == "gt") |
                            (.data[[col1]] <= value1 & op1 == "lt") |
                            (.data[[col2]] >= value2 & op2 == "gt") |
                            (.data[[col2]] <= value2 & op2 == "lt")
                    )
            }
            
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
