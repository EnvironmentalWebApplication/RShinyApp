library(shiny)
library(ggplot2)
library(colorRamps)
library(dplyr)
library(tidyr)
library(ggh4x)

#TODO: Change WQ y-axis & units, add secchi depth
#TODO: Wedneday meeting: ask about interpolated heat data, check dual YSI x-axis

# Load Lake Data
# lakeData <- read.csv("./r/data/Cleaned_LongPond_08082024.csv") OLD DATA
dailyLakeData <- read.table("./r/data/DailyAverage.txt", header = TRUE, sep = "\t")
subDailyLakeData <- read.table("./r/data/SubDailyAverage.txt", header = TRUE, sep = "\t")
interpolatedLakeData <- read.table("./r/data/interpolate.txt", header = TRUE, sep = "\t")
YSI <- read.table("./r/data/ysi.txt", header = TRUE, sep = "\t")
WQ <- read.table("./r/data/wq.txt", header = TRUE, sep = "\t")
WQDOC <- read.table("./r/data/2024_lng_DOC_for_analysis.txt", header = TRUE, sep = "\t")
WQSecchi <- read.table("./r/data/secchi.txt", header = TRUE, sep = "\t")

# Standardize column names
colnames(dailyLakeData) <- c("sensorType", "meter", "date",
                             "value", "STD", "var", "n")
colnames(subDailyLakeData) <- c("date", "value", "meter", "sensorType")
colnames(interpolatedLakeData) <- c("date", "meter", "dataSource", "temp", "do")
colnames(YSI) <- c("date", "meter", "temp", "do")
colnames(WQ) <- c("date", "site", "Total Phosphorus", "Total Nitrogen",
                  "Ammonium", "Soluble Reactive Phosphorus", "Chlorophyll A", "Iron")
colnames(WQDOC) <- c("date", "lake", "site", "Dissolved Organic Carbon", "n_reps", "min DOC mgl", "max DOC mgl")
colnames(WQSecchi) <- c("date", "Secchi Depth")

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
                   selected = "Heatmap"
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
                 conditionalPanel(
                   condition = "input.msTab == 'YSI'",
                   plotOutput("MSPlot")
                 ),
                 conditionalPanel(
                   condition = "input.msTab == 'Water Quality'",
                   splitLayout(
                     plotOutput('msTabSplitLeft'),
                     plotOutput('msTabSplitRight')
                   )
                 )
               ),
             )
    ),
    # selected = "High-Frequency Data"
    selected = "Manual Sampling"
  )
)


# Define server logic
server <- function(input, output, session) {
  #Filter the data into sensor types
  DailyDO <- dailyLakeData[dailyLakeData$sensorType == "DO",]
  DailyHeat <- dailyLakeData[dailyLakeData$sensorType == "Temp",]
  subDailyDO <- subDailyLakeData[subDailyLakeData$sensorType == "DO",]
  subDailyHeat <- subDailyLakeData[subDailyLakeData$sensorType == "Temp",]

  #Find all depths of the different sensors
  doDepthChoices <- sort(unique(DailyDO$meter), decreasing = FALSE)
  heatDepthChoices <- sort(unique(DailyHeat$meter), decreasing = FALSE)
  interpolatedHeatDepthChoices <- sort(unique(interpolatedLakeData$meter), decreasing = FALSE)

  #Format days
  DailyDO$date <- as.Date(DailyDO$date)
  DailyHeat$date <- as.Date(DailyHeat$date)
  subDailyDO$date <- as.POSIXct(subDailyDO$date, format = "%Y-%m-%d %H:%M:%S")
  subDailyHeat$date <- as.POSIXct(subDailyHeat$date, format = "%Y-%m-%d %H:%M:%S")
  interpolatedLakeData$date <- as.Date(interpolatedLakeData$date)
  # interpolatedLakeData$date <- as.POSIXct(interpolatedLakeData$date, format = "%Y-%m-%d")

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
    } else if (input$graphSelect == "Temperature at Depth") {
      dateRangeInput(
        "heatDates",
        "Select Date Range",
        start = min(DailyHeat$date),
        end = max(DailyHeat$date),
        min = min(DailyHeat$date),
        max = max(DailyHeat$date),
        format = "mm/dd/yyyy"
      )
    } else if (input$graphSelect == "Heatmap") {
      dateRangeInput(
        "interpolatedHeatDates",
        "Select Date Range",
        start = min(interpolatedLakeData$date),
        end = max(interpolatedLakeData$date),
        min = min(interpolatedLakeData$date),
        max = max(interpolatedLakeData$date),
        format = "mm/dd/yyyy"
      )
    }
  })

  # Water quality graph choices
  WQChoices <- c("Total Phosphorus", "Total Nitrogen", "Iron",
                 "Dissolved Organic Carbon", "Soluble Reactive Phosphorus",
                 "Secchi Depth", "Ammonium", "Chlorophyll A")

  output$manualSamplingParameters <- renderUI({
    if (input$msTab == "YSI") {
      tagList(
        selectInput(
          "YSIGraphSelect",
          "Select graph",
          choices = c("Temperature", "DO", "Both"),
          selected = "Temperature"
        ),
        checkboxGroupInput(
          "YSIDateSelect",
          "Select Date Range",
          choices = unique(YSI$date),
          selected = unique(YSI$date)
        )
      )
    }
    else if (input$msTab == "Water Quality") {
      tagList(
        selectInput(
          "leftWQSelect",
          "Select Left Graph",
          # choices = WQChoices[-1],
          # selected = WQChoices[2]
          choices = WQChoices,
          selected = WQChoices[1]
        ),
        selectInput(
          "rightWQSelect",
          "Select Right Graph",
          # choices = WQChoices,
          # selected = WQChoices[1]
          choices = WQChoices,
          selected = WQChoices[2]
        )
      )
    }
  })

  # #TODO: Bug where changes an input doesn't affect the graphs
  # observeEvent(input$leftWQSelect, {
  #   updateSelectInput(session, "rightWQSelect", choices = setdiff(WQChoices, input$leftWQSelect))
  # })
  #
  # observeEvent(input$rightWQSelect, {
  #   updateSelectInput(session, "leftWQSelect", choices = setdiff(WQChoices, input$rightWQSelect))
  # })

  # Select data based on user input
  selectedHF <- reactive({
    req(input$graphSelect)

    # DO data
    if (input$graphSelect == "Heatmap") {
      selected <- interpolatedLakeData[interpolatedLakeData$date >= input$interpolatedHeatDates[1] &
                              interpolatedLakeData$date <= input$interpolatedHeatDates[2],]
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
      if (input$YSIGraphSelect != "Both") {
        selected <- YSI[YSI$date %in% input$YSIDateSelect, ]
      }
      else {
      selected <-
        YSI %>%
          filter(date %in% input$YSIDateSelect) %>%
          pivot_longer(cols = c(temp, do), names_to = "Measurement", values_to = "Value")
      }
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

  # Graphs
  output$HFPlot <- renderPlot({
    if (input$graphSelect == "Heatmap") {
      ggplot(selectedHF(), aes(x = date, y = meter, fill = temp)) +
        geom_raster(interpolate = T) +
        scale_y_continuous(
          trans = "reverse",
          # TODO: uncomment when heatmap bug is fixed
          # breaks = min(interpolatedHeatDepthChoices):max(interpolatedHeatDepthChoices)
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
    if (input$YSIGraphSelect == "Temperature") {
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
        scale_x_continuous(breaks = seq(0, 30, by = 5),
                           limits = c(0, 30)) +
        theme(
          axis.title = element_text(size = 12),
          axis.text = element_text(size = 10)
        )
    }
    else if (input$YSIGraphSelect == "DO") {
      ggplot(selectedMS(), aes(x = do, y = meter, group = date, color = date)) +
        geom_path(size = 1) +
        geom_point(size = 3) +
        scale_color_viridis_d() +
        scale_y_reverse() +
        labs(
          x = "Dissolved Oxygen Measurement (mg/L)",
          y = "Depth (m)",
          color = "Date"
        ) +

        theme_minimal() +
        scale_x_continuous(breaks = seq(0, 15, by = 3),
                           limits = c(0, 15)) +
        theme(
          axis.title = element_text(size = 12),
          axis.text = element_text(size = 10)
        )
    }
    else if (input$YSIGraphSelect == "Both") {
      ggplot(selectedMS(), aes(x = Value, y = meter, color = as.factor(date), group = interaction(date, Measurement))) +
        geom_path(size = 1) +
        geom_point(size = 3) +
        scale_y_reverse() +
        scale_color_viridis_d() +
        facet_wrap(~Measurement, scales = "free_x",
                   labeller = as_labeller(c(temp = "Temperature (°C)", do = "Dissolved Oxygen (mg/L)"))) +
        theme_minimal() +
        labs(x = "", y = "Depth (m)", color = "Date") +
        facetted_pos_scales(
          x = list(
            "do"   = scale_x_continuous(breaks = seq(0, 15, by = 3), limits = c(0, 15)),
            "temp" = scale_x_continuous(breaks = seq(0, 30, by = 5), limits = c(0, 30))
          )
        )
    }
  })

  #TODO: Sechi units are meters

  # Water Quality Graph 1
  output$msTabSplitLeft <- renderPlot({
    if (input$msTab == "Water Quality") {
      # Determine which dataset to use
      data_to_plot <- if (input$leftWQSelect == "Dissolved Organic Carbon") {
        WQDOC
      } else {
        WQ
      }

      # Check if the dataset is empty or if the selected column exists
      if (nrow(data_to_plot) == 0) {
        print("No data available in the selected dataset.")
        return(NULL)
      }

      # Debugging: Check if the selected column exists in the data
      print(paste("Selected Column:", input$leftWQSelect))
      print(names(data_to_plot))

      ggplot(data_to_plot, aes(x = date, y = .data[[input$leftWQSelect]], color = site, shape = site)) +
        geom_point(size = 4) +
        scale_color_manual(values = c("EPI" = "yellow", "HYP" = "black"), labels = c("", "")) +
        scale_shape_manual(values = c("EPI" = 19, "HYP" = 17), labels = c("", "")) +
        labs(
          x = "",
          # y = "Total Phosphorus (µg/L)",
          color = "",
          shape = ""
        ) +
        theme_bw() +
        theme(
          axis.title = element_text(size = 12),
          axis.text = element_text(size = 10),
          legend.position = "top",
          axis.text.x = element_text(angle = 45, hjust = 1)
        ) +
        guides(color = guide_legend(override.aes = list(size = 0, shape = NA)),
               shape = guide_legend(override.aes = list(size = 0, shape = NA)))
    }
  })

  # Water Quality Graph 2
  output$msTabSplitRight <- renderPlot({
    if (input$msTab == "Water Quality") {
      # Determine which dataset to use
      data_to_plot <- if (input$rightWQSelect == "Dissolved Organic Carbon") {
        WQDOC
      } else {
        WQ
      }

      # Check if the dataset is empty or if the selected column exists
      if (nrow(data_to_plot) == 0) {
        print("No data available in the selected dataset.")
        return(NULL)
      }

      # Debugging: Check if the selected column exists in the data
      print(paste("Selected Column:", input$rightWQSelect))
      print(names(data_to_plot))

      ggplot(data_to_plot, aes(x = date, y = .data[[input$rightWQSelect]], color = site, shape = site)) +
        geom_point(size = 4) +
        scale_color_manual(values = c("EPI" = "yellow", "HYP" = "black")) +
        scale_shape_manual(values = c("EPI" = 19, "HYP" = 17)) +
        labs(
          x = "",
          # y = "Total Nitrogen (label)",
          color = "",
          shape = ""
        ) +
        theme_bw() +
        theme(
          axis.title = element_text(size = 12),
          axis.text = element_text(size = 10),
          legend.position = "top",
          axis.text.x = element_text(angle = 45, hjust = 1)
        )
    }
  })



}

# Run the application
shinyApp(ui = ui, server = server)
