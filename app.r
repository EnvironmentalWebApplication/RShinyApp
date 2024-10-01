#TODO: Improve heatmap, continue cleaning up code, hard set y axis, fix date

library(shiny)
library(ggplot2)
library(colorRamps)

# Load Lake Data
lakeData <- read.csv("Cleaned_LongPond_08082024.csv")

# Define UI
ui <- fluidPage(
  titlePanel("Placeholder Title"),
  sidebarLayout(
    sidebarPanel(
      selectInput("graphSelect", "Select Graph",
                  choices = c("DO Plot", "Heatmap", "Heat Scatterplot")),
      uiOutput("graphParameters"),
      uiOutput("dateParameters")
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
  
  #Get all the unique days
  doData$date <- as.Date(doData$date, format = "%m/%d/%y %H:%M")
  heatData$date <- as.Date(heatData$date, format = "%m/%d/%y %H:%M")
  
  # Graph settings (check boxes)
  output$graphParameters <- renderUI({
    if (input$graphSelect == "DO Plot") {
      checkboxGroupInput(
        "doDepth",
        "Select Depths (Meters)",
        choices = doDepthChoices,
        selected = min(doDepthChoices),
      )
    } else if (input$graphSelect == "Heatmap") {
      # Space to add graph parameters to heatmap
    } else if (input$graphSelect == "Heat Scatterplot") {
      checkboxGroupInput(
        "heatDepth",
        "Select Depths (Meters)",
        choices = heatDepthChoices,
        selected = min(heatDepthChoices)
      )
    }
    
  })
  
  # Date settings (Calander)
  output$dateParameters <- renderUI({
    if (input$graphSelect == "DO Plot") {
      dateRangeInput(
        "doDates",
        "Select Date Range",
        start = min(doData$date),
        end = max(doData$date),
        format = "mm/dd/yyyy"
      )
    } else if (input$graphSelect == "Heatmap" || input$graphSelect == "Heat Scatterplot") {
      dateRangeInput(
        "heatDates",
        "Select Date Range",
        start = min(heatData$date),
        end = max(heatData$date),
        format = "mm/dd/yyyy"
      )
    }
    
  })
  
  # Select data based on user input
  selectedData <- reactive({
    # DO data
    if (input$graphSelect == "DO Plot") {
      selected <- doData[doData$meter == input$doDepth & doData$date == input$doDates, ]
    # Heatmap data
    } else if (input$graphSelect == "Heatmap") {
      selected <- heatData
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
        labs(title = paste("DO Sensor at", input$doDepth, "Meters")) +
        xlab("Date") + ylab("DO (mg/L)") +
        theme_bw()
    } else if (input$graphSelect == "Heatmap") {
        ggplot(selectedData(), aes(x = date, y = meter, fill = Value)) +
        geom_tile() +
        scale_fill_gradientn(colours=matlab.like(10), na.value = 'gray', name="Water\nTemperature \n(?C)", limits = c(0, 35), breaks=c(0,5,10,15,20,25,30,35))  +
        xlab("Date") + ylab("Depth (m)") +
        theme_bw()
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
