library(shiny)
library(ggplot2)
library(dplyr)

options(shiny.maxRequestSize = 10*1024^2)

ui <- fluidPage(
  
  titlePanel("Volcano Plotting"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("foreground_file", "Choose a Primary File", accept = c("csv", ".csv")),
      fileInput("background_file", "Choose an Additional File (Optional)", accept = c("csv", ".csv")),
      selectInput("x_axis", "X-axis:", choices = NULL, multiple = FALSE),
      selectInput("y_axis", "Y-axis:", choices = NULL, multiple = FALSE),
      actionButton("button", "Run!"),
    ),
    
    # Show volcano plot based on inputs
    mainPanel(
      plotOutput("volcanoPlot"),
      downloadButton("downloadPlot", "Download Plot")
    )
  )
)

server <- function(input, output, session) {
  
  foreground_df <- reactive({
    req(input$foreground_file)
    dat <- read.csv(input$foreground_file$datapath)
    return(dat)
  })
  
  background_df <- reactive({
    if (!is.null(input$background_file)) {
      dat <- read.csv(input$background_file$datapath)
      return(dat)
    } else {
      return(NULL)
    }
  })
  
  observeEvent(foreground_df(), {
    # Update the choices for X and Y axes based on numerical columns in the foreground data
    numericalNames <- names(foreground_df())[sapply(foreground_df(), is.numeric)]
    updateSelectInput(session, "x_axis", "X-axis (Numerical):", choices = numericalNames)
    updateSelectInput(session, "y_axis", "Y-axis (Numerical):", choices = numericalNames)
  })
  
  observeEvent(input$button, {
    req(input$x_axis, input$y_axis)
    x_col <- input$x_axis
    y_col <- input$y_axis
    
    # Range for filtering outliers
    range_min <- quantile(foreground_df()[[x_col]], 0.01)
    range_max <- quantile(foreground_df()[[x_col]], 0.99) 
    
    # Remove outliers from foreground and background data
    foreground_filtered <- foreground_df() %>%
      filter(!is.na(foreground_df()[[x_col]]), !is.na(foreground_df()[[y_col]]), foreground_df()[[x_col]] >= range_min, foreground_df()[[x_col]] <= range_max, foreground_df()[[y_col]] <= quantile(foreground_df()[[y_col]], 0.99))
    
    # Determine the symmetric x-axis limits
    x_limits <- max(abs(range(foreground_filtered[[x_col]]))) + 0.1
    
    # Calculate y-values for p = 0.05
    p_value_cutoff <- 0.05
    y_cutoff <- -log10(p_value_cutoff)
    
    # Create a volcano plot with foreground (red) and background (gray) points
    volcano_plot <- ggplot() +
      geom_point(data = foreground_filtered, aes_string(x = x_col, y = paste("-log10(", y_col, ")", sep = "")), color = "firebrick3", size = 3, alpha = 0.6, shape = 21, fill = "firebrick3", stroke = 0.5) +
      labs(x = x_col, y = paste("-log10(", y_col, ")", sep = "")) +
      scale_x_continuous(limits = c(-x_limits, x_limits)) +  # Set symmetric x-axis limits
      theme_minimal() +
      theme(legend.position = "none",
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank()) +
      geom_vline(xintercept = 0, linetype = "dashed", color = "black", size = 1.0) + 
      geom_hline(yintercept = y_cutoff, linetype = "dashed", color = "black", size = 0.5) + 
      annotate("text", x = x_limits, y = y_cutoff, label = "p = 0.05", hjust = +21, vjust = -2.0) 
    
    # If background data is provided, add it to the plot
    if (!is.null(background_df())) {
      volcano_plot <- volcano_plot +
        geom_point(data = background_df(), aes_string(x = x_col, y = paste("-log10(", y_col, ")", sep = "")), color = "gray20", size = 3, alpha = 0.5, shape = 21, fill = "gray", stroke = 0.5)
    }
    
    output$volcanoPlot <- renderPlot({
      volcano_plot
    })
    
    # Store the plot as a reactive value for downloading
    output$downloadPlot <- downloadHandler(
      filename = function() {
        paste("volcanoPlot_", x_col, "_", y_col, ".pdf", tolower(input$downloadType), sep = "")
      },
      content = function(file) {
        ggsave(file, plot = volcano_plot, device = ".pdf")
      }
    )
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

