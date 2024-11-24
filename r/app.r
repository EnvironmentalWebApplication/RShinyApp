library(shiny)
library(ggplot2)
library(colorRamps)

# Load Lake Data
# lakeData <- read.csv("./r/data/Cleaned_LongPond_08082024.csv") OLD DATA
dailyLakeData <- read.table("./r/data/DailyAverage.txt", header = TRUE, sep = "\t")
subDailyLakeData <- read.table("./r/data/SubDailyAverage.txt", header = TRUE, sep = "\t")

# Standardize column names
colnames(dailyLakeData) <- c("sensorType", "meter", "date", "value", "STD", "var", "n")
colnames(subDailyLakeData) <- c("date", "value", "meter", "sensorType")

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
                   choices = c("Heatmap", "DO at Depth", "Temperature at Depth"),
                   selected = "DO at Depth"
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
  DailyDO <- dailyLakeData[dailyLakeData$sensorType == "DO",]
  DailyHeat <- dailyLakeData[dailyLakeData$sensorType == "Temp",]
  subDailyDO <- subDailyLakeData[subDailyLakeData$sensorType == "DO",]
  subDailyHeat <- subDailyLakeData[subDailyLakeData$sensorType == "Temp",]

  #Find all depths of the different sensors
  doDepthChoices <- sort(unique(DailyDO$meter), decreasing = FALSE)
  heatDepthChoices <- sort(unique(DailyHeat$meter), decreasing = FALSE)

  #Format days
  DailyDO$date <- as.Date(DailyDO$date)
  DailyHeat$date <- as.Date(DailyHeat$date)
  subDailyDO$date <- as.POSIXct(subDailyDO$date, format = "%Y-%m-%d %H:%M:%S")
  subDailyHeat$date <- as.POSIXct(subDailyHeat$date, format = "%Y-%m-%d %H:%M:%S")

  # Graph settings (check boxes)
  output$graphParameters <- renderUI({
    if (input$graphSelect == "Heatmap") {
      # Space to add graph parameters to heatmap
    }
    else if (input$graphSelect == "DO at Depth") {
      tagList(
        selectInput(
          "frequencyDO",
          "Select Display Frequency",
          choices = c("Daily Average", "Sub Daily"),
          selected = "Daily Average"
        ),
        checkboxGroupInput(
          "doDepth",
          "Select Depths (Meters)",
          choices = doDepthChoices,
          selected = min(doDepthChoices),
        )
      )
    } else if (input$graphSelect == "Temperature at Depth") {
      tagList(
        selectInput(
          "frequencyHeat",
          "Select Display Frequency",
          choices = c("Daily Average", "Sub Daily"),
          selected = "Daily Average"
        ),
        checkboxGroupInput(
          "heatDepth",
          "Select Depths (Meters)",
          choices = heatDepthChoices,
          selected = min(heatDepthChoices)
        )
      )
    }

  })

  # Date settings (Calander)
  output$dateParameters <- renderUI({
    if (input$graphSelect == "DO at Depth") {
      dateRangeInput(
        "doDates",
        "Select Date Range",
        start = min(DailyDO$date),
        end = max(DailyDO$date),
        min = min(DailyDO$date),
        max = max(DailyDO$date),
        format = "mm/dd/yyyy"
      )
    } else if (input$graphSelect == "Heatmap" || input$graphSelect == "Temperature at Depth") {
      dateRangeInput(
        "heatDates",
        "Select Date Range",
        start = min(DailyHeat$date),
        end = max(DailyHeat$date),
        min = min(DailyHeat$date),
        max = max(DailyHeat$date),
        format = "mm/dd/yyyy"
      )
    }
  })

  # Select data based on user input
  selectedData <- reactive({
    # DO data
    if (input$graphSelect == "Heatmap") {
      selected <- DailyHeat[as.Date(DailyHeat$date) >= input$heatDates[1] &
                              as.Date(DailyHeat$date) <= input$heatDates[2],]
    }
    # Heatmap data
    else if (input$graphSelect == "DO at Depth") {
      if (input$frequencyDO == "Daily Average") {
        selected <- DailyDO[DailyDO$meter %in% input$doDepth &
                              as.Date(DailyDO$date) >= input$doDates[1] &
                              as.Date(DailyDO$date) <= input$doDates[2],]
      }
      else if (input$frequencyDO == "Sub Daily") {
        selected <- subDailyDO[subDailyDO$meter %in% input$doDepth &
                              as.Date(subDailyDO$date) >= input$doDates[1] &
                              as.Date(subDailyDO$date) <= input$doDates[2],]
      }
    }
    # Heat scatterplot data
    else if (input$graphSelect == "Temperature at Depth") {
      if (input$frequencyHeat == "Daily Average") {
        selected <- DailyHeat[DailyHeat$meter %in% input$heatDepth &
                                as.Date(DailyHeat$date) >= input$heatDates[1] &
                                as.Date(DailyHeat$date) <= input$heatDates[2],]
      }
      else if (input$frequencyHeat == "Sub Daily") {
        selected <- subDailyHeat[subDailyHeat$meter %in% input$heatDepth &
                                as.Date(subDailyHeat$date) >= input$heatDates[1] &
                                as.Date(subDailyHeat$date) <= input$heatDates[2],]
      }
    }

    return(selected)
  })

  # Manual color setting for depths
  depthColors <- c("1.52" = "#f87a71", "2.52" = "#c39b24", "3.52" = "#54b321", "4.52" = "#05be95",
                   "5.52" = "#08b4e8", "6.52" = "#a68bfc", "7.52" = "#fb66d5")

  # Graphs
  output$selectedPlot <- renderPlot({
    if (input$graphSelect == "Heatmap") {
      ggplot(selectedData(), aes(x = date, y = meter, fill = value)) +
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
      ggplot(selectedData(), aes(x = date, y = value, color = factor(meter))) +
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
      ggplot(selectedData(), aes(x = date, y = value, color = factor(meter))) +
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
