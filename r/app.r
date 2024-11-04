library(shiny)
library(ggplot2)
library(colorRamps)

# Load Lake Data
lakeData <- read.csv("./r/data/Cleaned_LongPond_08082024.csv")

# Define UI
ui <- fluidPage(
  titlePanel(""),

  # Create a single tabsetPanel to hold multiple tabPanel elements
  tabsetPanel(

    # First tab
    tabPanel("Tab 1",
             sidebarLayout(
               sidebarPanel(
                 selectInput(
                   "graphSelect",
                   "Select Graph",
                   choices = c("Heatmap", "DO at Depth", "Temperature at Depth")
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
  doData <- lakeData[lakeData$sensor_type == "DO",]
  heatData <- lakeData[lakeData$sensor_type == "Temperature",]

  #Find all depths of the different sensors
  doDepthChoices <- sort(unique(doData$meter), decreasing = FALSE)
  heatDepthChoices <- sort(unique(heatData$meter), decreasing = FALSE)

  #Format days
  doData$date <- as.Date(doData$date, format = "%m/%d/%y")
  heatData$date <- as.Date(heatData$date, format = "%m/%d/%y")

  # Graph settings (check boxes)
  output$graphParameters <- renderUI({
      if (input$graphSelect == "Heatmap") {
      # Space to add graph parameters to heatmap
      }
      else if (input$graphSelect == "DO at Depth") {
        checkboxGroupInput(
        "doDepth",
        "Select Depths (Meters)",
        choices = doDepthChoices,
        selected = min(doDepthChoices),
        )
      } else if (input$graphSelect == "Temperature at Depth") {
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
    if (input$graphSelect == "DO at Depth") {
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
      input$graphSelect == "Temperature at Depth") {
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
    if (input$graphSelect == "Heatmap") {
      selected <- heatData[as.Date(heatData$date) >= input$heatDates[1] &
                             as.Date(heatData$date) <= input$heatDates[2],]
      # Heatmap data
    } else if (input$graphSelect == "DO at Depth") {
      selected <- doData[doData$meter == input$doDepth &
                           as.Date(doData$date) >= input$doDates[1] &
                           as.Date(doData$date) <= input$doDates[2],]
      # Heat scatterplot data
    } else if (input$graphSelect == "Temperature at Depth") {
      selected <- heatData[heatData$meter == input$heatDepth &
                             as.Date(heatData$date) >= input$heatDates[1] &
                             as.Date(heatData$date) <= input$heatDates[2],]
    }

    return(selected)
  })

  # Manual color setting for depths
  depthColors <- c("1" = "#f87a71", "2" = "#c39b24", "3" = "#54b321", "4" = "#05be95",
                   "5" = "#08b4e8", "6" = "#a68bfc", "7" = "#fb66d5")

  # Graphs
  output$selectedPlot <- renderPlot({
    if (input$graphSelect == "Heatmap") {
      ggplot(selectedData(), aes(x = date, y = meter, fill = Value)) +
        geom_raster(interpolate = T) +
        scale_y_continuous(
          trans = "reverse",
          breaks = min(heatDepthChoices):max(heatDepthChoices)
        ) +
        scale_fill_viridis_c(limits = c(5, 30), breaks = seq(5, 30, by = 5)) +
        scale_x_date(position = "top") +
        labs(
          x = "",
          y = "Depth (m)",
          fill = "Temp (°C)"
        ) +
        theme_classic()
    } else if (input$graphSelect == "DO at Depth") {
      ggplot(selectedData(), aes(x = date, y = Value, color = factor(meter))) +
        geom_point() +
        scale_color_manual(values = depthColors) +
        scale_y_continuous(breaks = seq(0, 15, by = 3),
                           limits = c(0, 15)) +
        labs(
          x = "",
          y = "DO (mg/L)",
          color = "Meter"
        ) +
        theme_bw()
    } else if (input$graphSelect == "Temperature at Depth") {
      ggplot(selectedData(), aes(x = date, y = Value, color = factor(meter))) +
        geom_point() +
        scale_color_manual(values = depthColors) +
        scale_y_continuous(breaks = seq(0, 30, by = 5),
                           limits = c(0, 30)) +
        labs(
          x = "",
          y = "Temperature (°C)",
          color = "Meter"
        ) +
        theme_bw()
    }
  })
}

# Run the application
shinyApp(ui = ui, server = server)
