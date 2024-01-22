library(shiny)
library(DT)
library(ggplot2)
library(dplyr)
library(purrr)

ui <- fluidPage(
  titlePanel("Nested Violin SuperPlot"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Upload CSV File"),
      numericInput("num_conditions", "Number of Conditions", value = 1, min = 1),
      uiOutput("condition_inputs"),
      actionButton("generate_plot", "Generate Plot!"),
      downloadButton("download_plot", "Download Plot as PDF")
    ),
    mainPanel(
      verbatimTextOutput("medians_output"),
      plotOutput("violin_plot")
    )
  )
)

server <- function(input, output, session) {
  # Define reactive expression to read the uploaded CSV file
  data <- reactive({
    req(input$file)
    read.csv(input$file$datapath)
  })
  
  # Dynamically generate inputs for user-defined condition names
  observe({
    req(input$num_conditions)
    condition_names <- lapply(1:input$num_conditions, function(i) {
      textInput(inputId = paste0("condition_", i), label = paste("Condition", i), value = "")
    })
    output$condition_inputs <- renderUI({ condition_names })
  })
  
  # Create a reactive list to store selected columns for each condition
  condition_columns <- reactiveVal(list())
  
  # Observe changes in condition names and selected columns
  observe({
    req(input$num_conditions)
    conditions <- lapply(1:input$num_conditions, function(i) {
      condition_name <- input[[paste0("condition_", i)]]
      selected_columns <- input[[paste0("columns_", i)]]
      
      # Filter out selected columns that do not exist in the data frame
      existing_columns <- intersect(selected_columns, colnames(data()))
      
      if (length(existing_columns) == 0) {
        return(NULL)
      }
      
      list(name = condition_name, columns = existing_columns)
    })
    
    # Filter out conditions with no valid columns
    conditions <- Filter(function(x) !is.null(x$columns), conditions)
    condition_columns(conditions)
  })
  
  # Dynamically generate column selection inputs based on user-defined conditions
  observe({
    req(input$num_conditions)
    for (i in 1:input$num_conditions) {
      columns <- colnames(data())
      column_selector <- selectInput(inputId = paste0("columns_", i), label = paste("Select columns for Condition", i),
                                     choices = columns, multiple = TRUE)
      insertUI(selector = paste0("#condition_", i), where = "afterEnd", ui = column_selector)
    }
  })
  # Calculate and display medians when the "Generate Plot!" button is clicked
  observeEvent(input$generate_plot, {
    conditions <- condition_columns()
    if (length(conditions) == 0) {
      return(NULL)
    }
    
    # Create a list to store the medians and condition names
    medians_list <- list()
    
    # Ensure all data frames have the same structure
    data_structure <- data.frame(Column = character(0), Condition = character(0), Values = numeric(0))
    
    for (i in seq_along(conditions)) {
      condition_name <- conditions[[i]]$name
      selected_columns <- conditions[[i]]$columns
      
      # Extract the values from selected columns
      values <- data()[, selected_columns]
      
      # Create a data frame with the values, condition, and column
      data_frame <- data.frame(
        Column = selected_columns,
        Condition = condition_name,
        Values = values
      )
      
      # Ensure the data frame structure is consistent
      if (!identical(names(data_structure), names(data_frame))) {
        stop("Data frames have different structures. Please check that the columns match.")
      }
      
      medians <- sapply(data_frame$Values, median)
      
      # Store medians along with column names
      medians_data <- data.frame(
        Column = selected_columns,
        Condition = condition_name,
        Medians = medians
      )
      
      medians_list[[i]] <- medians_data
    }
    
    # Prepare data for violin plot
    all_violin_data <- do.call(rbind, lapply(medians_list, function(x) x[c("Column", "Condition", "Medians")]))  # Combined data from all conditions
    
    # Create the violin plot by 'Condition' with means and standard deviation error bars
    p <- ggplot(all_violin_data, aes(x = Condition, y = Medians, fill = Condition)) +
      geom_violin(trim = FALSE, fill = "lightgray", alpha = 0.7) +
      theme_minimal() +
      theme(axis.title.x = element_blank(),
            legend.position = "none",
            axis.text.y = element_text(size = 12),
            axis.text.x = element_text(size = 12))
    
    output$violin_plot <- renderPlot({
      print(p)
    })
  })
}

shinyApp(ui, server)
