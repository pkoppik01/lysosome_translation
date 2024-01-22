library(shiny)
library(ggplot2)
library(dplyr)

ui <- fluidPage(
  titlePanel("Volcano log10 Plotting"),
  
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
    updateSelectInput(session, "color_column", "Color Column:", choices = names(df()))
  })
  
  observeEvent(input$button, {
    req(input$x_axis, input$y_axis, input$color_column)
    x_col <- input$x_axis
    y_col <- input$y_axis
    
    df_filtered <- df()
    
    # Filter and adjust x-values: if less than -3, set to -3; otherwise, leave unchanged
    df_filtered[[x_col]][df_filtered[[x_col]] < -3] <- -3
    df_filtered[[x_col]][df_filtered[[x_col]] > 3] <- 3
    
    # Set the x-axis limits from -3 to 3
    x_limits <- c(-3.2, 3)
    y_limits <- c(0, 6)
    
    p_value_cutoff <- 0.05
    y_cutoff <- -log10(p_value_cutoff)
    
    # Convert '1' to 'red' and 'NA' to 'gray' when mapping the color aesthetic
    df_filtered <- df_filtered %>%
      mutate(
        color_col = ifelse(df_filtered[[input$color_column]] == 1, "red", "gray"),
        alpha = ifelse(color_col == "red", 1, 0.75)  # Set alpha to 1 for red points, 0.75 for others
      )
    
    volcano_plot <- ggplot(df_filtered, aes_string(x = x_col, y = paste("-log10(", y_col, ")", sep = ""), fill = "color_col")) +
      geom_point(size = 3, shape = 21, stroke = 0.1, aes(alpha = alpha)) +
      labs(x = x_col, y = paste("-log10(", y_col, ")", sep = "")) +
      scale_x_continuous(limits = x_limits) +
      scale_y_continuous(limits = y_limits) +
      scale_fill_manual(values = c("red" = "red", "gray" = "darkgray")) +
      scale_alpha_continuous(range = c(0.75, 1)) +  # Adjust alpha range
      theme_minimal() +
      theme(legend.position = "none",
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank()) +
      geom_vline(xintercept = -3.2, linetype = "dashed", color = "black", size = 1.0) +
      geom_hline(yintercept = y_cutoff, linetype = "dashed", color = "black", size = 0.5) +
      annotate("text", x = 0, y = y_cutoff, label = "p = 0.05", hjust = +10, vjust = -2.0)
    
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

