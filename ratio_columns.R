library(shiny)
library(readr)
library(dplyr)

# Define UI
ui <- fluidPage(
  titlePanel("Column Ratio Calculator"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Choose CSV File"),
      selectInput("numerator_col", "Select Numerator Column", ""),
      selectInput("denominator_col", "Select Denominator Column", ""),
      actionButton("calculate", "Calculate Ratios"),
      downloadButton("download", "Download Result")  # Moved download button here
    ),
    
    mainPanel(
      tableOutput("result")
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  options(shiny.maxRequestSize = 100 * 1024^2)
  data <- reactive({
    req(input$file)
    df <- read_csv(input$file$datapath)
    return(df)
  })
  
  observe({
    col_names <- colnames(data())
    updateSelectInput(session, "numerator_col", choices = col_names)
    updateSelectInput(session, "denominator_col", choices = col_names)
  })
  
  ratios_calculated <- reactiveVal(FALSE)
  
  observeEvent(input$calculate, {
    ratios_calculated(TRUE)
  })
  
  ratio_data <- reactive({
    req(ratios_calculated())
    req(input$numerator_col, input$denominator_col)
    df <- data()
    df <- df %>%
      mutate(Ratio = !!sym(input$numerator_col) / !!sym(input$denominator_col))
    return(df)
  })
  
  output$result <- renderTable({
    req(ratios_calculated())
    ratio_data()
  })
  
  # Modified download handler to use ratio_data()
  output$download <- downloadHandler(
    filename = function() {
      "ratio_result.csv"
    },
    content = function(file) {
      write_csv(ratio_data(), file)
    }
  )
}

# Run the Shiny app
shinyApp(ui, server)

