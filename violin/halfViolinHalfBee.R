#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(ggbeeswarm)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Nested Statistical Analysis"),
  
  # Sidebar with file selection and input
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Choose a file", accept=c("csv", ".csv")),
      selectInput("condition", "Condition:", choices = NULL, multiple = FALSE),
      uiOutput("checkboxesCondition"),
      selectInput("nestedParameter", "Nested Parameter:", choices = NULL, multiple = FALSE),
      uiOutput("checkboxesNested"),
      selectInput("medians", "Medians Input:", choices=NULL, multiple=FALSE),
      selectInput("numerical", "Numerical Input:", choices=NULL, multiple=FALSE),
      selectInput("displayOption", "Display Option:",
                  choices = c("Violin Plot" = "violin", "Individual Points" = "points"),
                  selected = "violin"),
      actionButton("button", "Run!"),
    ),
    
    # Show violin plot based on inputs
    mainPanel(
      verbatimTextOutput("anovaSummary"),
      plotOutput("violinPlot"),
      uiOutput("downloadTypePanel"),
      uiOutput("downloadButton"),
    )
  )
)

server <- function(input, output, session) {
  
  df <- reactive({
    req(input$file)
    dat <- read.csv(input$file$datapath) #get CSV uploaded and convert to dataframe
    return(dat)
  })
  
  frame <- NULL
  
  observeEvent(df(), {
    frame <<- df() #define frame as global variable
    if ("Filename" %in% names(frame)) { #gets well letter and well number from filename
      wellNames <- grep("Well", unlist(strsplit(frame[["Filename"]], '_')), value = TRUE)
      frame[["Well Letter"]] <<- substr(wellNames, start = 5, stop=5)
      frame[["Well Number"]] <<- as.numeric(substr(wellNames, start = 6, stop=nchar(wellNames)))
    }
    #finds what column contains the Seq data
    colWithSeqData <- names(frame)[sapply(frame, function(f) any(grepl("Seq",f)))]
    #tries to get the Seq data if possible, adds it to frame
    if (length(colWithSeqData) == 1) {
      seqIndices <- regexpr("Seq", frame[[colWithSeqData]])
      frame[["Seq"]] <<- as.numeric(substr(frame[[colWithSeqData]], seqIndices+3, seqIndices+6))
    }
    #all columns with numeric data
    numericalNames <- names(frame)[sapply(frame, is.numeric)]
    updateSelectInput(session, "numerical", "Numerical Input:", choices=numericalNames)
    #all columns where there is more than 1 level
    conditionNames <- names(frame)[sapply(frame, function(x) length(unique(x)) > 1)]
    updateSelectInput(session, "condition", "Condition:", choices=conditionNames)
    #all columns where there is more than 1 level
    nestedNames <- names(frame)[sapply(frame, function(x) length(unique(x)) > 1)]
    updateSelectInput(session, "nestedParameter", "Nested Parameter:", choices=nestedNames)
    #all columns where there is more than 1 level
    mediansNames <- names(frame)[sapply(frame, function(x) length(unique(x)) > 1)]
    updateSelectInput(session, "medians", "Medians Input:", choices=mediansNames)
    #all columns where there is more than 1 level
    #alldataNames <- names(frame)[sapply(frame, is.numeric)]
    #updateSelectInput(session, "alldata", "Data Input:", choices=alldataNames)
  })
  
  reactivePlot <- reactiveValues()
  
  #Nested ANOVA Analysis printed
  observeEvent(input$button, {
    req(input$condition, input$medians, input$numerical)
    
    output$anovaSummary <- renderText({
      #filter frame by what condition checkboxes were selected
      filteredFrame <- frame[frame[[input$condition]] %in% input$checkboxesCondition, ]
      #filter frame by what nested parameter checkboxes were selected
      #filteredFrame <- filteredFrame[filteredFrame[[input$nestedParameter]] %in% input$checkboxesNested, ]
      numericCol <- filteredFrame[[input$numerical]]
      conditionCol <- filteredFrame[[input$condition]]
      #nestedCol <- filteredFrame[[input$nestedParameter]]
      mediansCol <- filteredFrame[[input$medians]]
      #nested ANOVA function from https://www.statology.org/nested-anova-in-r/
      nest <- aov(numericCol ~ conditionCol / factor(mediansCol))
      
      summary <- capture.output(print(TukeyHSD(nest, conf.level=.95)))
      #pastes nested ANOVA into the renderText on the shiny app display's mainPanel
      paste(summary, collapse = "\n")
    })
  })
  
  observeEvent(input$button, {
    req(input$condition, input$nestedParameter, input$medians, input$numerical)
    #filter frame by what condition checkboxes were selected
    filteredFrame <- frame[frame[[input$condition]] %in% input$checkboxesCondition, ]
    #filter frame by what nested parameter checkboxes were selected
    filteredFrame <- filteredFrame[filteredFrame[[input$nestedParameter]] %in% input$checkboxesNested, ]
    conditionCol <- factor(filteredFrame[[input$condition]])
    nestedCol <- factor(filteredFrame[[input$nestedParameter]])
    numericCol <- filteredFrame[[input$numerical]]
    mediansCol <- filteredFrame[[input$medians]]
    #dataCol <- filteredFrame[[input$alldata]]
    
    nest <- aov(numericCol ~ conditionCol / mediansCol)
    
    #y_limits <- c(-25,10)
    
    
    #output grouped violin plot
    output$violinPlot <- renderPlot({
      options(repr.plot.width = 50, repr.plot.height = 2)
      
      if (input$displayOption == "violin") {
        # Display the violin plot
        plot <- ggplot(filteredFrame, aes(x = factor(conditionCol, levels = unique(conditionCol), ordered = TRUE), y = numericCol, fill = factor(nestedCol, levels = unique(nestedCol), ordered = TRUE), alpha = 0.8)) +
          geom_violin(fill = '#d3d3d3', color = "black", aes(fill = variable), size = 1.15, lwd = 0.5) +
          labs(x = input$condition, y = input$numerical, fill = input$nestedParameter) +
          theme_bw() +
          #scale_y_continuous(limits = y_limits) +
          stat_summary(
            fun = "median",
            geom = "point",
            shape = 16,
            size = 4,
            aes(group = mediansCol),
            color = "black",
            width = 0.1
          ) +
          stat_summary(fun = "median",
                       geom = "crossbar", 
                       width = 0.5,
                       fatten = 1.5,
                       colour = "black",
                       aes(group = conditionCol),
                       alpha = 0.8) +
          scale_fill_brewer(palette = "Set1") +
          theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                panel.background = element_blank(), axis.line = element_line(colour = "black"), 
                legend.position="none")
      } else {
        # Display individual data points
        plot <- ggplot(filteredFrame, aes(x = factor(conditionCol, levels = unique(conditionCol), ordered = TRUE), y = numericCol, fill = factor(nestedCol, levels = unique(nestedCol), ordered = TRUE), alpha = 0.8)) +
          geom_beeswarm(shape = 16, size = 2, color = "gray", alpha = 0.9) +
          labs(x = input$condition, y = input$numerical, fill = input$nestedParameter) +
          theme_bw() +
          scale_y_continuous(limits = y_limits) +
          stat_summary(
            fun = "median",
            geom = "point",
            shape = 16,
            size = 4,
            aes(group = mediansCol),
            color = "black"
          ) +
          stat_summary(fun = "mean",
                       geom = "crossbar", 
                       width = 0.5,
                       fatten = 2.5,
                       colour = "black",
                       aes(group = conditionCol),
                       alpha = 0.9) +
          scale_fill_brewer(palette = "Set1") +
          theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                panel.background = element_blank(), axis.line = element_line(colour = "black"), 
                legend.position="none")
      }
      
      reactivePlot$plot <<- plot
      plot
    })
    
  })
  
  #display checkboxes for condition
  observeEvent(input$condition, {
    conditionCol <- factor(frame[[input$condition]])
    if (!is.null(input$condition) && input$condition != "") {
      output$checkboxesCondition <- renderUI({
        checkboxGroupInput(
          "checkboxesCondition",
          "Filter by Condition: (select at least 2, only 36 can be shown on the plot)",
          choices = unique(conditionCol),
          selected = unique(conditionCol)
        )
      })
    } else {
      output$checkboxesCondition <- renderUI({
        NULL
      })
    }
  })
  
  #display checkboxes for nested parameter
  observeEvent(input$nestedParameter, {
    nestedCol <- factor(frame[[input$nestedParameter]])
    if (!is.null(input$nestedParameter) && input$nestedParameter != "") {
      output$checkboxesNested <- renderUI({
        checkboxGroupInput(
          "checkboxesNested",
          "Filter by Nested Parameter: (select at least 2)",
          choices = unique(nestedCol),
          selected = unique(nestedCol)
        )
      })
    } else {
      output$checkboxesCondition <- renderUI({
        NULL
      })
    }
  })
  
  observeEvent(input$button, {
    req(input$condition, input$nestedParameter, input$numerical)
    output$downloadTypePanel <- renderUI({
      selectInput("downloadType", "File Type:", choices = c("TIFF", "PNG", "PDF"), multiple = FALSE)
    }
    )
    output$downloadButton <- renderUI(
      downloadHandler(
        filename = function() { #gives it a file extension according to the choice
          paste("nestedANOVA_", input$condition, "_", input$nestedParameter, "_", input$numerical, ".", tolower(input$downloadType), sep = "")
        },
        content = function(file) {
          if (input$downloadType=="PDF") {
            pdf(file, width = 10.5, height = 7)
            print(reactivePlot$plot)
            dev.off()
          }
          else {
            png(file, width = 10.5, height = 7, units = "in", res = 1200)
            print(reactivePlot$plot)
            dev.off()
          }
        }
      )
    )
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)