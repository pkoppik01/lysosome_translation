# Load necessary libraries
library(shiny)

# Define the UI
ui <- fluidPage(
  titlePanel("CSV Column Selector"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Choose a CSV file"),
      uiOutput("column_selector")
    ),
    
    mainPanel(
      tableOutput("selected_data")
    )
  )
)

# Define the server logic
server <- function(input, output, session) {
  observe({
    req(input$file)
    df <- read.csv(input$file$datapath, stringsAsFactors = FALSE)
    output$column_selector <- renderUI({
      checkboxGroupInput("columns", "Select Columns", choices = names(df), selected = names(df))
    })
  })
  
  output$selected_data <- renderTable({
    req(input$file)
    req(input$columns)
    
    df <- read.csv(input$file$datapath, stringsAsFactors = FALSE)
    selected_cols <- df[, input$columns, drop = FALSE]
    
    return(selected_cols)
  })
}

shinyApp(ui, server)

