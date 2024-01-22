# Install and load the required packages
if (!require("shiny")) {
  install.packages("shiny")
}
if (!require("readr")) {
  install.packages("readr")
}
library(shiny)
library(readr)

# Define the user interface
ui <- fluidPage(
  titlePanel("Nested ANOVA Analysis"),
  tabsetPanel(
    tabPanel("Analysis", 
             sidebarLayout(
               sidebarPanel(
                 fileInput("file", "Choose CSV File"),
                 selectInput("numeric_col", "Numeric Column", ""),
                 selectInput("condition_col", "Condition Column", ""),
                 selectInput("nested_col", "Nested Column", ""),
                 actionButton("analyze", "Analyze")
               ),
               mainPanel(
                 verbatimTextOutput("p_value_output")
               )
             )
    ),
    tabPanel("QQ Plot",
             mainPanel(
               plotOutput("qqplot")
             )
    )
  )
)
# Define the server logic
server <- function(input, output, session) {
  data <- reactive({
    req(input$file)
    read.csv(input$file$datapath)
  })
  
  observe({
    updateSelectInput(session, "numeric_col", choices = names(data()))
    updateSelectInput(session, "condition_col", choices = names(data()))
    updateSelectInput(session, "nested_col", choices = names(data()))
  })
  
  # Perform nested ANOVA analysis and return the Tukey results
  analyze_data <- eventReactive(input$analyze, {
    req(input$numeric_col, input$condition_col, input$nested_col)
    
    nest <- aov(data()[[input$numeric_col]] ~ data()[[input$condition_col]] / data()[[input$nested_col]])
    
    # Perform a Tukey post hoc test
    tukey_result <- TukeyHSD(nest, which = "data()[[input$condition_col]]")
    
    tukey_result
  })
  
  # Display the Tukey results
  output$p_value_output <- renderPrint({
    tukey_result <- analyze_data()
    tukey_result
  })
  
  # Create QQ plot
  output$qqplot <- renderPlot({
    req(input$numeric_col)
    numeric_data <- data()[[input$numeric_col]]
    qqnorm(numeric_data)
    qqline(numeric_data)
  })
}

# Run the Shiny app
shinyApp(ui, server)
