library(shiny)
library(ggplot2)
library(colorRamps)

# Load Lake Data
# lakeData <- read.csv("./r/data/Cleaned_LongPond_08082024.csv") OLD DATA
dailyLakeData <- read.table("./r/data/DailyAverage.txt", header = TRUE, sep = "\t")
subDailyLakeData <- read.table("./r/data/SubDailyAverage.txt", header = TRUE, sep = "\t")
YSI <- read.table("./r/data/ysi.txt", header = TRUE, sep = "\t")
WQ <- read.table("./r/data/wq.txt", header = TRUE, sep = "\t")

# Standardize column names for high frequency data
colnames(dailyLakeData) <- c("sensorType", "meter", "date", "value", "STD", "var", "n")
colnames(subDailyLakeData) <- c("date", "value", "meter", "sensorType")
colnames(WQ) <- c("date", "site", "tot_phos", "tot_nit", "ammonium", "react_phos", "chlor_a", "iron")

# Define UI
ui <- fluidPage(
  titlePanel(""),

  # Create a single tabsetPanel to hold multiple tabPanel elements
  tabsetPanel(

    # High-frequency data tab
    tabPanel("High-Frequency Data",
             sidebarLayout(
               sidebarPanel(
                 selectInput(
                   "graphSelect",
                   "Select Graph",
                   choices = c("Heatmap", "DO at Depth", "Temperature at Depth"),
                   selected = "Temperature at Depth"
                 ),
                 uiOutput("graphParameters"),
                 uiOutput("dateParameters")
               ),
               mainPanel(
                 plotOutput("HFPlot")
               )
             )
    ),

    # Manual sampling tab
    tabPanel("Manual Sampling",
             sidebarLayout(
               sidebarPanel(
                 selectInput(
                   "msTab",
                   "Select Graph",
                   choices = c("YSI", "Water Quality"),
                   selected = "Water Quality"
                 ),
                 uiOutput("manualSamplingParameters")
               ),
               mainPanel(
                 plotOutput("MSPlot")
               )
             )
    ),
    selected = "Manual Sampling"
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
      # Placeholder to add graph parameters to heatmap
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
          selected = c("1.52", "2.52", "3.52", "4.52", "5.52", "6.52", "7.52")
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
          selected = c("1.52", "2.52", "3.52", "4.52", "5.52", "6.52", "7.52")
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

  output$manualSamplingParameters <- renderUI({
    if (input$msTab == "YSI") {
      # YSI Placeholder parameters
    }
    else if (input$msTab == "Water Quality") {
      selectInput(
        "WQSelect",
        "Select Water Quality Graphs",
        choices = c("tot-phos", "tot_nit", "ammonium", "react_phos", "chlor_a", "iron"),
        selected = "tot-phos"
      )
    }
  })

  # Select data based on user input
  selectedHF <- reactive({
    req(input$graphSelect)

    # DO data
    if (input$graphSelect == "Heatmap") {
      selected <- DailyHeat[as.Date(DailyHeat$date) >= input$heatDates[1] &
                              as.Date(DailyHeat$date) <= input$heatDates[2],]
    }
      # Heatmap data
    else if (input$graphSelect == "DO at Depth") {
      if (input$frequencyDO == "Daily Average") {
        selected <- DailyDO[DailyDO$meter %in% input$doDepth &
                              DailyDO$date >= input$doDates[1] &
                              DailyDO$date <= input$doDates[2],]
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
                                DailyHeat$date >= input$heatDates[1] &
                                DailyHeat$date <= input$heatDates[2],]
      }
      else if (input$frequencyHeat == "Sub Daily") {
        selected <- subDailyHeat[subDailyHeat$meter %in% input$heatDepth &
                                   as.Date(subDailyHeat$date) >= input$heatDates[1] &
                                   as.Date(subDailyHeat$date) <= input$heatDates[2],]
      }
    }

    return(selected)
  })

  selectedMS <- reactive({
    req(input$msTab)

    if (input$msTab == "YSI") {
      selected <- YSI
    }

    else if (input$msTab == "Water Quality") {
      selected <- WQ
    }

    return(selected)
  })

  # Manual color settings for depths
  depthColors <- c("1.52" = "#f87a71", "2.52" = "#c39b24", "3.52" = "#54b321", "4.52" = "#05be95",
                   "5.52" = "#08b4e8", "6.52" = "#a68bfc", "7.52" = "#fb66d5")

  # Manual shape settings for depths
  depthShapes <- c("1.52" = 21,  # Circle
                   "2.52" = 22,  # Square
                   "3.52" = 23,  # Diamond
                   "4.52" = 24,  # Triangle point-up
                   "5.52" = 4,   # X letter
                   "6.52" = 3,   # + sign
                   "7.52" = 25)  # Triangle point-down

  # MS Plots
  output$MSPlots <- renderUI({
    if (input$msTab == "YSI") {
      plotOutput("MSPlot")
    } else if (input$msTab == "Water Quality") {
      fluidRow(
        column(6, plotOutput("WQPlot1")),  # Left graph
        column(6, plotOutput("WQPlot2"))   # Right graph
      )
    }
  })

  # Graphs
  output$HFPlot <- renderPlot({
    if (input$graphSelect == "Heatmap") {
      ggplot(selectedHF(), aes(x = date, y = meter, fill = value)) +
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
      ggplot(selectedHF(), aes(x = date, y = value, color = factor(meter), shape = factor(meter))) +
        geom_point(size = 2) +
        scale_color_manual(values = depthColors) +
        scale_shape_manual(values = depthShapes) +
        scale_y_continuous(breaks = seq(0, 15, by = 3),
                           limits = c(0, 15)) +
        guides(color = guide_legend("Meter"), shape = guide_legend("Meter")) +
        labs(
          x = "",
          y = "DO (mg/L)",
          color = "Meter"
        ) +
        theme_bw()
    } else if (input$graphSelect == "Temperature at Depth") {
      ggplot(selectedHF(), aes(x = date, y = value, color = factor(meter), shape = factor(meter))) +
        geom_point(size = 2) +
        scale_color_manual(values = depthColors) +
        scale_shape_manual(values = depthShapes) +
        scale_y_continuous(breaks = seq(0, 30, by = 5),
                           limits = c(0, 30)) +
        guides(color = guide_legend("Meter"), shape = guide_legend("Meter")) +
        labs(
          x = "",
          y = "Temperature (°C)",
          color = "Meter"
        ) +
        theme_bw()
    }
  })

  #MS Plots
  output$MSPlot <- renderPlot({
    if (input$msTab == "YSI") {
      ggplot(selectedMS(), aes(x = temp, y = meter, group = date, color = date)) +
        geom_path(size = 1) +
        geom_point(size = 3) +
        scale_color_viridis_d() +
        scale_y_reverse() +
        labs(
          x = "Temperature Measurement (°C)",
          y = "Depth (m)",
          color = "Date"
        ) +
        theme_minimal() +
        theme(
          axis.title = element_text(size = 12),
          axis.text = element_text(size = 10)
        )
    }
  })

  # Water Quality Graph 1
  output$WQPlot1 <- renderPlot({
    ggplot(selectedMS(), aes(x = date, y = tot_phos, color = site, shape = site)) +
      geom_point(size = 4) +
      scale_color_manual(values = c("EPI" = "yellow", "HYP" = "black")) +
      scale_shape_manual(values = c("EPI" = 19, "HYP" = 17)) +
      labs(
        x = "",
        y = "Total Phosphorus (µg/L)",
        color = "",
        shape = ""
      ) +
      theme_bw() +
      theme(
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        legend.position = "top"
      )
  })

  # Water Quality Graph 2
  output$WQPlot2 <- renderPlot({
    ggplot(selectedMS(), aes(x = date, y = tot_phos, color = site, shape = site)) +
      geom_point(size = 4) +
      scale_color_manual(values = c("EPI" = "yellow", "HYP" = "black")) +
      scale_shape_manual(values = c("EPI" = 19, "HYP" = 17)) +
      labs(
        x = "",
        y = "Total Phosphorus (µg/L)",
        color = "",
        shape = ""
      ) +
      theme_bw() +
      theme(
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        legend.position = "top"
      )
  })
}

# Run the application
shinyApp(ui = ui, server = server)
