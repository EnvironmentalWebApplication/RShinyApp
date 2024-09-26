#TODO: Add heatmap, continue cleaning up code, hard set y axis

library(shiny)
library(ggplot2)

# Load Lake Data
lakeData <- read.csv("Cleaned_LongPond_08082024.csv")

# Define UI
ui <- fluidPage(
  titlePanel("Placeholder Title"),
  sidebarLayout(
    sidebarPanel(
      selectInput("graphSelect", "Select Graph",
                  choices = c("DO Plot", "Heatmap", "Heat Scatterplot")),
      uiOutput("graphParameters")
    ),
    mainPanel(
      plotOutput("selectedPlot")
    )
  )
)

# Define server logic
server <- function(input, output) {

  #Filter the data into sensor types
  doData <- lakeData[lakeData$sensor_type == "DO", ]
  heatData <- lakeData[lakeData$sensor_type == "Temperature", ]
  
  #Find all depths of the different sensors
  doDepthChoices <- sort(unique(doData$meter), decreasing = FALSE)
  heatDepthChoices <- sort(unique(heatData$meter), decreasing = FALSE)
  
  # Graph settings (sliders)
  output$graphParameters <- renderUI({
    if (input$graphSelect == "DO Plot") {
      checkboxGroupInput(
        "doDepth",
        "Select Depths (Meters)",
        choices = doDepthChoices,
        selected = min(doDepthChoices)
      )
    } else if (input$graphSelect == "Heatmap") {
      
    } else if (input$graphSelect == "Heat Scatterplot") {
      checkboxGroupInput(
        "heatDepth",
        "Select Depths (Meters)",
        choices = heatDepthChoices,
        selected = min(heatDepthChoices)
      )
    }
    
  })
  
  # Select data based on user input
  selectedData <- reactive({
    # DO data
    if (input$graphSelect == "DO Plot") {
      selected <- doData[doData$meter == input$doDepth, ]
    # Heatmap data
    } else if (input$graphSelect == "Heatmap") {
      
    # Heat scatterplot data
    } else if (input$graphSelect == "Heat Scatterplot") {
      selected <- heatData[heatData$meter == input$heatDepth, ]
    }
    
    return(selected)
  })
  
  # Graphs
  output$selectedPlot <- renderPlot({
    if (input$graphSelect == "DO Plot") {
      ggplot(selectedData(), aes(x = date, y = Value)) +
        geom_point() +
        labs(title = paste("DO Sensor at", input$meter, "Meters")) +
        xlab("Date") + ylab("DO (mg/L)") +
        theme_bw()
    } else if (input$graphSelect == "Heatmap") {
      
    } else if (input$graphSelect == "Heat Scatterplot") {
      ggplot(selectedData(), aes(x = date, y = Value)) +
        geom_point() +
        labs(title = paste("DO Sensor at", input$meter, "Meters")) +
        xlab("Date") + ylab("Temperature (°C)") +
        theme_bw()
    }
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
