library(shiny)
library(ggplot2)

# Load Lake Data
lakeData <- read.csv("Cleaned_LongPond_08082024.csv")

# Define UI
ui <- fluidPage(
  titlePanel("Placeholder Title"),
  sidebarLayout(
    sidebarPanel(
      selectInput("graphSelect", "Select Graph:",
                  choices = c("DO Plot", "Sensor Heatmap", "Heat Scatterplot")),
      uiOutput("graphParameters")
    ),
    mainPanel(
      plotOutput("selectedPlot")
    )
  )
)

# Define server logic
server <- function(input, output) {

  # Graph settings (sliders)
  output$graphParameters <- renderUI({
    if (input$graphSelect == "DO Plot") {
      sliderInput("doDepth", 
                  "Select Depth (Meters):", 
                  min = 1, 
                  max = 7, 
                  value = 1, 
                  step = 5)
    } else if (input$graphSelect == "Sensor Heatmap") {
      
    } else if (input$graphSelect == "Heat Scatterplot") {
      sliderInput("heatDepth", 
                  "Select Depth (Meters):", 
                  min = 1, 
                  max = 7, 
                  value = c(1, 7))
    }
    
  })
  
  filteredData <- reactive({
    if (input$graphSelect == "DO Plot") {
      filtered <- lakeData[lakeData$sensor_type == "DO" & lakeData$meter == input$doDepth, ]
    } else if (input$graphSelect == "Sensor Heatmap") {
      
    } else if (input$graphSelect == "Heat Scatterplot") {
      filtered <- lakeData[lakeData$sensor_type == "Temperature" & 
                             lakeData$meter == input$heatDepth, ]
    }
    
    return(filtered)
  })
  
  # Graphs
  output$selectedPlot <- renderPlot({
    if (input$graphSelect == "DO Plot") {
      ggplot(filteredData(), aes(x = date, y = Value)) +
        geom_point() +
        labs(title = paste("DO Sensor at", input$meter, "Meters")) +
        xlab("Date") + ylab("DO (mg/L)") +
        theme_bw()
    } else if (input$graphSelect == "Sensor Heatmap") {
      
    } else if (input$graphSelect == "Heat Scatterplot") {
      ggplot(filteredData(), aes(x = date, y = Value)) +
        geom_point() +
        labs(title = paste("DO Sensor at", input$meter, "Meters")) +
        xlab("Date") + ylab("Temperature (°C)") +
        theme_bw()
    }
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
