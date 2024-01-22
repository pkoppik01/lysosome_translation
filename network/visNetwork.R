# Load required libraries
library(shiny)
library(visNetwork)
library(htmlwidgets)

# Define UI
ui <- fluidPage(
  titlePanel("Network Visualization with visNetwork"),
  sidebarLayout(
    sidebarPanel(
      fileInput("node_file", "Choose Node CSV file"),
      fileInput("edge_file", "Choose Edge CSV file"),
      checkboxInput("freeze", "Freeze Network", value = TRUE),
      downloadButton("download_pdf", "Download as PDF")
    ),
    mainPanel(
      visNetworkOutput("network")
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  
  # Load node and edge data from CSV files
  node_data <- reactive({
    req(input$node_file)
    read.csv(input$node_file$datapath, header = TRUE)
  })
  
  edge_data <- reactive({
    req(input$edge_file)
    read.csv(input$edge_file$datapath, header = TRUE)
  })
  
  # Create the network visualization with blue square nodes
  output$network <- renderVisNetwork({
    visNetwork(
      nodes = node_data(),
      edges = edge_data(),
      width = "100%",
      height = "600px"
    ) %>%
      visOptions(manipulation = TRUE) %>%
      visInteraction(navigationButtons = TRUE) %>%
      visPhysics(
        solver = "barnesHut",
        stabilization = TRUE,
        enabled = !input$freeze
      ) %>%
      visEvents(select = "function(event, properties) {Shiny.onInputChange('selected_nodes', properties.nodes);}") %>%
      visNodes(
        shape = "square",
        color = "blue",
        size = 25
      )
  })
  
  # Create a downloadable PDF
  output$download_pdf <- downloadHandler(
    filename = function() {
      paste("network_plot", ".pdf", sep = "")
    },
    content = function(file) {
      saveWidget(
        visNetwork(
          nodes = node_data(),
          edges = edge_data(),
          width = "100%",
          height = "800px"
        ),
        file,
        selfcontained = TRUE,
        title = "Network Plot"
      )
    }
  )
}

# Run the Shiny app
shinyApp(ui, server)
