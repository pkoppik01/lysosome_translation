library(shiny)
library(ggplot2)
library(tidyverse)
library(ggridges)

# Define the UI
ui <- fluidPage(
  titlePanel("Ridgeline Plot App"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Choose a CSV file",
                multiple = FALSE,
                accept = c(".csv")
      ),
      selectInput("y_columns", "Select Y-axis Column(s):", choices = NULL, multiple = TRUE),
      textInput("x_label", "Enter X-axis Label:", value = "Values"),  # X-axis label input
      textInput("y_label", "Enter Y-axis Label:", value = "Columns"),  # Y-axis label input
      textInput("plot_title", "Enter Plot Title:", value = "Ridgeline Plot"),  # Plot title input
      uiOutput("column_order_ui"),  # Add a UI element for column order
      downloadButton("download_plot", "Download Plot")
    ),
    mainPanel(
      plotOutput("ridgeline_plot")
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
    updateSelectizeInput(session, "column_order", choices = column_choices, selected = column_choices)
  })
  
  # Generate the ridgeline plot
  output$ridgeline_plot <- renderPlot({
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
    
    # Define a custom color gradient
    n_ridges <- length(input$y_columns)
    color_gradient <- colorRampPalette(c("gray38", "gray90"))(n_ridges)
    
    #x_limits <- c(-20,10)
    
    # Create the ridgeline plot with gradient colors
    p <- ggplot(df_long, aes(x = value, y = variable, fill = as.factor(variable))) +
      stat_density_ridges(quantile_lines = FALSE, scale = 2, 
                          rel_min_height = 0.001, color = "white", size = 1.5,
                          linetype = 1) +
      scale_fill_manual(values = color_gradient) +
      #scale_x_continuous(limits = x_limits) +
      labs(title = input$plot_title,
           x = input$x_label,
           y = input$y_label) +
      theme_minimal() +
      theme(panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank()) +
      guides(fill = FALSE)  # Remove the legend for the fill color
    
    print(p)
  })
  
  # Download the plot as a PNG file
  output$download_plot <- downloadHandler(
    filename = function() {
      paste("ridgeline_plot", ".pdf", sep = "")
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
      
      n_ridges <- length(input$y_columns)
      color_gradient <- colorRampPalette(c("gray38", "gray90"))(n_ridges)
      
      #x_limits <- c(-7,3)
      
      # Create the ridgeline plot with gradient colors
      p <- ggplot(df_long, aes(x = value, y = variable, fill = as.factor(variable))) +
        stat_density_ridges(quantile_lines = FALSE, scale = 2, 
                            rel_min_height = 0.001, color = "white", size = 1.5,
                            linetype = 1) +
        scale_fill_manual(values = color_gradient) +
        #scale_x_continuous(limits = x_limits) +
        labs(title = input$plot_title,
             x = input$x_label,
             y = input$y_label) +
        theme_minimal() +
        theme(panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank()) +
        guides(fill = FALSE)  # Remove the legend for the fill color
      
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
