#TODO: Improve heatmap, continue cleaning up code, hard set y axis, improve colors

library(shiny)
library(ggplot2)
library(colorRamps)

# Load Lake Data
lakeData <- read.csv("./data/Cleaned_LongPond_08082024.csv")

# Define UI
ui <- fluidPage(
  titlePanel("Placeholder"),
  
  # Create a single tabsetPanel to hold multiple tabPanel elements
  tabsetPanel(
    
    # First tab
    tabPanel("Tab 1", 
             sidebarLayout(
               sidebarPanel(
                 selectInput(
                   "graphSelect",
                   "Select Graph",
                   choices = c("DO Plot", "Heatmap", "Heat Scatterplot")
                 ),
                 uiOutput("graphParameters"),
                 uiOutput("dateParameters")
               ),
               mainPanel(
                 plotOutput("selectedPlot")
               )
             )
    ),
    
    # Second tab
    tabPanel("Tab 2", 
             sidebarLayout(
               sidebarPanel(
                 selectInput(
                   "test",
                   "Select Test Graph",
                   choices = c("Test1", "Test2")
                 )
               ),
               mainPanel(
                 plotOutput("testPlot")
               )
             )
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
  
  #Format days
  doData$date <- as.Date(doData$date, format = "%m/%d/%y")
  heatData$date <- as.Date(heatData$date, format = "%m/%d/%y")
  
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
        min = min(doData$date),
        max = max(doData$date),
        format = "mm/dd/yyyy"
      )
    } else if (input$graphSelect == "Heatmap" ||
               input$graphSelect == "Heat Scatterplot") {
      dateRangeInput(
        "heatDates",
        "Select Date Range",
        start = min(heatData$date),
        end = max(heatData$date),
        min = min(heatData$date),
        max = max(heatData$date),
        format = "mm/dd/yyyy"
      )
    }
  })
  
  # Select data based on user input
  selectedData <- reactive({
    # DO data
    if (input$graphSelect == "DO Plot") {
      selected <- doData[doData$meter == input$doDepth &
                           as.Date(doData$date) >= input$doDates[1] &
                           as.Date(doData$date) <= input$doDates[2], ]
      # Heatmap data
    } else if (input$graphSelect == "Heatmap") {
      selected <- heatData[as.Date(heatData$date) >= input$heatDates[1] &
                             as.Date(heatData$date) <= input$heatDates[2], ]
      # Heat scatterplot data
    } else if (input$graphSelect == "Heat Scatterplot") {
      selected <- heatData[heatData$meter == input$heatDepth &
                             as.Date(heatData$date) >= input$heatDates[1] &
                             as.Date(heatData$date) <= input$heatDates[2], ]
    }
    
    return(selected)
  })
  
  # Graphs
  output$selectedPlot <- renderPlot({
    if (input$graphSelect == "DO Plot") {
      ggplot(selectedData(), aes(x = date, y = Value, color = factor(meter))) +
        geom_point() +
        labs(title = paste("DO Sensor at", input$doDepth, "Meters")) +
        scale_y_continuous(limits = c(min(doData$Value), max(doData$Value))) +
        labs(
          x = "",
          y = "DO (mg/L)",
          color = "Meter"
        ) +
        theme_bw()

    } else if (input$graphSelect == "Heatmap") {
      ggplot(selectedData(), aes(x = date, y = meter, fill = Value)) +
        geom_raster(interpolate = T) +
        scale_y_continuous(
          trans = "reverse",
          breaks = min(heatDepthChoices):max(heatDepthChoices)
        ) +
        scale_fill_viridis_c() +
        scale_x_date(position = "top") +
        labs(
          x = "",
          y = "Depth (m)",
          title = "Long Pond (Average) Sensor Temperature Plot (1m - 7m)",
          fill = "Temp (C)"
        ) +
        theme_classic()
    } else if (input$graphSelect == "Heat Scatterplot") {
      ggplot(selectedData(), aes(x = date, y = Value, color = factor(meter))) +
        geom_point() +
        labs(title = paste("DO Sensor at", input$meter, "Meters")) +
        labs(
          x = "",
          y = "Temperature (°C)",
          color = "Meter"
        ) +
        scale_y_continuous(limits = c(min(heatData$Value), max(heatData$Value))) +
        theme_bw()
    }
  })
}

# Run the application
shinyApp(ui = ui, server = server)
