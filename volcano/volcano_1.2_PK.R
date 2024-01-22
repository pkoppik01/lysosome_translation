library(shiny)
library(ggplot2)
library(dplyr)

ui <- fluidPage(
  
  titlePanel("Volcano Plotting"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Choose a file", accept = c("csv", ".csv")),
      selectInput("x_axis", "X-axis (Numerical):", choices = NULL, multiple = FALSE),
      selectInput("y_axis", "Y-axis (Numerical):", choices = NULL, multiple = FALSE),
      selectInput("color_column", "Color Column:", choices = NULL, multiple = FALSE),
      actionButton("button", "Run!"),
    ),
    
    mainPanel(
      plotOutput("volcanoPlot"),
      downloadButton("downloadPlot", "Download Plot")
    )
  )
)

server <- function(input, output, session) {
  
  df <- reactive({
    req(input$file)
    dat <- read.csv(input$file$datapath)
    return(dat)
  })
  
  observeEvent(df(), {
    numericalNames <- names(df())[sapply(df(), is.numeric)]
    updateSelectInput(session, "x_axis", "X-axis (Numerical):", choices = numericalNames)
    updateSelectInput(session, "y_axis", "Y-axis (Numerical):", choices = numericalNames)
    
    allColumnNames <- names(df())
    updateSelectInput(session, "color_column", "Color Column:", choices = allColumnNames)
  })
  
  observeEvent(input$button, {
    req(input$x_axis, input$y_axis, input$color_column)
    x_col <- input$x_axis
    y_col <- input$y_axis
    color_col <- input$color_column
    
    # Calculate quantiles to exclude the top and bottom 2% of values in the x-axis
    range_min <- quantile(df()[[x_col]], 0.02)
    range_max <- quantile(df()[[x_col]], 0.98)
    
    # Remove outliers based on the quantile range
    df_filtered <- df() %>%
      filter(!is.na(df()[[x_col]]), !is.na(df()[[y_col]]), df()[[x_col]] >= range_min, df()[[x_col]] <= range_max)
    
    # Convert '1' to 'red' and 'NA' to 'gray' when mapping the color aesthetic
    df_filtered <- df_filtered %>%
      mutate(color_col = ifelse(df_filtered[[color_col]] == 1, "red", "gray"))
    
    # Determine the symmetric x-axis limits
    x_limits <- max(abs(range(df_filtered[[x_col]]))) + 0.1
    
    p_value_cutoff <- 0.05
    y_cutoff <- -log10(p_value_cutoff)
    
    volcano_plot <- ggplot(df_filtered, aes_string(x = x_col, y = paste("-log10(", y_col, ")", sep = ""), color = "color_col")) +
      geom_point(size = 3, alpha = 0.65, shape = 21, aes(fill = color_col), stroke = 0.5) +
      scale_fill_manual(values = c("red" = "red", "gray" = "gray"), na.value = "gray") +
      labs(x = x_col, y = paste("-log10(", y_col, ")", sep = "")) +
      scale_x_continuous(limits = c(-x_limits, x_limits)) +
      theme_minimal() +
      theme(legend.position = "none",
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank()) +
      geom_vline(xintercept = 0, linetype = "dashed", color = "black", size = 1.0) +
      geom_hline(yintercept = y_cutoff, linetype = "dashed", color = "black", size = 0.5) +
      annotate("text", x = x_limits, y = y_cutoff, label = "p = 0.05", hjust = +21, vjust = -2.0)
    
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
