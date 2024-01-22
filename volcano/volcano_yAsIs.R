library(shiny)
library(ggplot2)
library(dplyr)

ui <- fluidPage(
  
  titlePanel("Volcano as is Plotting"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Choose a file", accept = c("csv", ".csv")),
      selectInput("x_axis", "X-axis (Numerical):", choices = NULL, multiple = FALSE),
      selectInput("y_axis", "Y-axis (Numerical):", choices = NULL, multiple = FALSE),
      actionButton("button", "Run!"),
    ),
    
    mainPanel(
      plotOutput("volcanoPlot"),
      downloadButton("downloadPlot", "Download Plot")
    )
  )
)

server <- function(input, output, session) {
  options(shiny.maxRequestSize = 100 * 1024^2)
  
  df <- reactive({
    req(input$file)
    dat <- read.csv(input$file$datapath)
    return(dat)
  })
  
  observeEvent(df(), {
    numericalNames <- names(df())[sapply(df(), is.numeric)]
    updateSelectInput(session, "x_axis", "X-axis (Numerical):", choices = numericalNames)
    updateSelectInput(session, "y_axis", "Y-axis (Numerical):", choices = numericalNames)
  })
  
  observeEvent(input$button, {
    req(input$x_axis, input$y_axis)
    x_col <- input$x_axis
    y_col <- input$y_axis
    
    df_filtered <- df()
    
    # Filter out the top and bottom 5% of data based on the y-axis input
    y_percentile_95 <- quantile(df_filtered[[y_col]], 0.95)
    y_percentile_05 <- quantile(df_filtered[[y_col]], 0.05)
    df_filtered <- df_filtered[df_filtered[[y_col]] >= y_percentile_05 & df_filtered[[y_col]] <= y_percentile_95, ]
    
    # Calculate the y_cutoff based on p-value cutoff (0.05)
    #y_cutoff <- -log10(0.05)
    
    # Create the volcano plot
    volcano_plot <- ggplot(df_filtered, aes_string(x = x_col, y = y_col)) +
      geom_point(size = 3, alpha = 0.2, shape = 21, fill = "black", color = "gray", stroke = 0.1) +
      labs(x = x_col, y = y_col) +
      scale_x_continuous(limits = c(-20, 20)) +  # Set symmetric x-axis limits
      theme_minimal() +
      theme(legend.position = "none",
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank()) +
      geom_vline(xintercept = 0, linetype = "dashed", color = "black", size = 1.0) #+
      #geom_hline(yintercept = y_cutoff, linetype = "dashed", color = "black", size = 0.5) +
      #annotate("text", x = 0, y = y_cutoff, label = "p = 0.05", hjust = +10, vjust = -2.0)
    
    output$volcanoPlot <- renderPlot({
      volcano_plot
    })
    
    output$downloadPlot <- downloadHandler(
      filename = function() {
        paste("volcanoPlot_", x_col, "_", y_col, "pdf", tolower(input$downloadType), sep = "")
      },
      content = function(file) {
        ggsave(file, plot = volcano_plot, device = "pdf")
      }
    )
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
