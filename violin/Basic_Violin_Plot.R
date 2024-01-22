
library(shiny)
library(ggplot2)
library(tidyverse)
library(ggbeeswarm)

# Define the UI
ui <- fluidPage(
  titlePanel("Violin Plot App"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Choose a CSV file",
                multiple = FALSE,
                accept = c(".csv")
      ),
      selectInput("y_columns", "Select Column(s):", choices = NULL, multiple = TRUE),
      textInput("x_label", "Enter X-axis Label:", value = "Columns"),  # X-axis label input
      textInput("y_label", "Enter Y-axis Label:", value = "Values"),  # Y-axis label input
      textInput("plot_title", "Enter Plot Title:", value = "Violin Plot"),  # Plot title input
      downloadButton("download_plot", "Download Plot"),
      uiOutput("column_order_ui")  # Add a UI element for the column order
    ),
    mainPanel(
      plotOutput("violin_plot")
    )
  )
)

# Define the server logic
server <- function(input, output, session) {
  
  # Update the Y-axis column choices based on the uploaded CSV file
  observe({
    req(input$file)
    df <- read.csv(input$file$datapath)
    column_choices <- names(df)
    updateSelectInput(session, "y_columns", choices = column_choices)
  })
  
  output$violin_plot <- renderPlot({
    req(input$file, input$y_columns)
    df <- read.csv(input$file$datapath)
    
    # Create a data frame with selected Y-axis columns
    selected_columns <- df %>%
      select(all_of(input$y_columns))
    
    # Gather data to long format for plotting
    df_long <- gather(selected_columns, key = "variable", value = "value")
    
    # Get the selected column order
    selected_order <- input$column_order
    
    # Reorder the "variable" column based on the selected order
    df_long$variable <- factor(df_long$variable, levels = selected_order)
    
    y_limits <- c(-2,2)
    
    # Create the violin plot with gray fill color
    p <- ggplot(df_long, aes(x = variable, y = value)) +
      geom_beeswarm(shape = 16, size = 2, color = "gray", alpha = 0.9) +
      theme(panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank()) +
      scale_y_continuous(limits = y_limits) +
      stat_summary(fun = "median",
                   geom = "crossbar", 
                   width = 0.5,
                   fatten = 1,
                   colour = "black",
                   aes(x = variable, y = value),
                   alpha = 0.8) 
      labs(title = input$plot_title,
           x = input$x_label,
           y = input$y_label) +
      theme_bw() +
      scale_fill_brewer(palette = "Set1") +
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            panel.background = element_blank(), axis.line = element_line(colour = "black"), 
            legend.position="none")

    
    print(p)
  })
  
  
  # Download the plot as a PNG file
  output$download_plot <- downloadHandler(
    filename = function() {
      paste("plot", ".pdf", sep = "")
    },
    content = function(file) {
      req(input$file, input$y_columns)  # Ensure that the plot is generated
      
      df <- read.csv(input$file$datapath)
      
      # Create a data frame with selected Y-axis columns
      selected_columns <- df %>%
        select(all_of(input$y_columns))
      
      # Gather data to long format for plotting
      df_long <- gather(selected_columns, key = "variable", value = "value")
      
      # Get the selected column order
      selected_order <- input$column_order
      
      # Reorder the "variable" column based on the selected order
      df_long$variable <- factor(df_long$variable, levels = selected_order)
      
      p <- ggplot(df_long, aes(x = variable, y = value)) +
        geom_beeswarm(shape = 16, size = 2, color = "gray", alpha = 0.9) +
        theme(panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank()) +
        stat_summary(fun = "median",
                     geom = "crossbar", 
                     width = 0.5,
                     fatten = 1.0,
                     colour = "black",
                     aes(x = variable, y = value),
                     alpha = 0.8) 
      labs(title = input$plot_title,
           x = input$x_label,
           y = input$y_label) +
      theme_bw() +
      scale_fill_brewer(palette = "Set1") +
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            panel.background = element_blank(), axis.line = element_line(colour = "black"), 
            legend.position="none")
      
      # Save the plot as a PDF
      ggsave(file, plot = p, width = 7, height = 7, device = "pdf")
    }
  )
  
  # Define a reactive element for the sortable list of columns
  output$column_order_ui <- renderUI({
    req(input$y_columns)
    # Create a sortable list of columns
    selectizeInput(
      "column_order",
      "Change Column Order:",
      choices = input$y_columns,
      multiple = TRUE,
      options = list(sortable = TRUE)
    )
  })
}

# Run the Shiny app
shinyApp(ui, server)

