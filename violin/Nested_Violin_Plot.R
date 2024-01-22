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
      actionButton("generate_medians", "Generate Plot!"),
      textInput("plot_title", "Plot Title", value = "Violin Plot of Medians by Condition"),
      downloadButton("download_plot", "Download Plot as PDF")
    ),
    mainPanel(
      verbatimTextOutput("medians_output"),
      plotOutput("violin_plot")
    )
  )
)

server <- function(input, output, session) {
  data <- reactive({
    req(input$file)
    read.csv(input$file$datapath)
  })
  
  observe({
    req(input$num_conditions)
    condition_names <- lapply(1:input$num_conditions, function(i) {
      textInput(inputId = paste0("condition_", i), label = paste("Condition", i), value = "")
    })
    output$condition_inputs <- renderUI({ condition_names })
  })
  
  condition_columns <- reactiveVal(list())
  
  observe({
    req(input$num_conditions)
    conditions <- lapply(1:input$num_conditions, function(i) {
      condition_name <- input[[paste0("condition_", i)]]
      selected_columns <- input[[paste0("columns_", i)]]
      
      existing_columns <- intersect(selected_columns, colnames(data()))
      
      if (length(existing_columns) == 0) {
        return(NULL)
      }
      
      list(name = condition_name, columns = existing_columns)
    })
    
    conditions <- Filter(function(x) !is.null(x$columns), conditions)
    condition_columns(conditions)
  })
  
  observe({
    req(input$num_conditions)
    for (i in 1:input$num_conditions) {
      columns <- colnames(data())
      column_selector <- selectInput(inputId = paste0("columns_", i), label = paste("Select columns for Condition", i),
                                     choices = columns, multiple = TRUE)
      insertUI(selector = paste0("#condition_", i), where = "afterEnd", ui = column_selector)
    }
  })
  
  p <- reactiveVal(NULL)
  
  observeEvent(input$generate_medians, {
    conditions <- condition_columns()
    if (length(conditions) == 0) {
      return(NULL)
    }
    
    medians_list <- list()
    
    for (i in seq_along(conditions)) {
      condition_name <- conditions[[i]]$name
      selected_columns <- conditions[[i]]$columns
      
      medians <- sapply(data()[, selected_columns], median)
      
      medians_data <- data.frame(
        Column = selected_columns,
        Condition = condition_name,
        Medians = medians
      )
      
      medians_list[[i]] <- medians_data
    }
    
    all_violin_data <- do.call(rbind, medians_list)
    
    condition_stats <- all_violin_data %>%
      group_by(Condition) %>%
      summarize(Mean = mean(Medians),
                SD = sd(Medians))
    
    p(
      ggplot(all_violin_data, aes(x = Condition, y = Medians, fill = Condition)) +
        geom_violin(trim = FALSE, fill = "lightgray", alpha = 0.7) +
        geom_jitter(aes(x = Condition, y = Medians), color = "black", width = 0.2, alpha = 0.7) +
        geom_crossbar(data = condition_stats, aes(x = Condition, y = Mean, ymin = Mean, ymax = Mean),
                      color = "black", width = 0.3, size = 0.5) +
        geom_errorbar(data = condition_stats, aes(x = Condition, y = Mean, ymin = Mean - SD, ymax = Mean + SD),
                      color = "black", width = 0.1, size = 0.5) +
        labs(title = input$plot_title) +
        theme_minimal() +
        theme(axis.title.x = element_blank(),
              legend.position = "none",
              axis.text.y = element_text(size = 12),
              axis.text.x = element_text(size = 12)
        )
    )
  })
  
  output$violin_plot <- renderPlot({
    if (!is.null(p())) {
      print(p())
    }
  })
  
  output$download_plot <- downloadHandler(
    filename = function() {
      paste("violin_plot_", Sys.Date(), ".pdf", sep = "")
    },
    content = function(file) {
      if (!is.null(p())) {
        pdf(file, width = 10, height = 6)
        print(p())
        dev.off()
      }
    }
  )
}

shinyApp(ui, server)
