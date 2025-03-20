library(shiny)
library(ggplot2)
library(colorRamps)
library(dplyr)
library(tidyr)
library(ggh4x)

#TODO Things left to: swap interpolated data set, swap wq data set adding nitrate
#TODO Meeting topics:

# Load Lake Data
dailyLakeData <- read.table("./data/DailyAverage.txt", header = TRUE, sep = "\t")
subDailyLakeData <- read.table("./data/SubDailyAverage.txt", header = TRUE, sep = "\t")
YSI <- read.table("./data/ysi.txt", header = TRUE, sep = "\t")
WQ <- read.table("./data/wq.txt", header = TRUE, sep = "\t")
WQDOC <- read.table("./data/2024_lng_DOC_for_analysis.txt", header = TRUE, sep = "\t")
WQSecchi <- read.table("./data/secchi.txt", header = TRUE, sep = "\t")

# Standardize column names
colnames(dailyLakeData) <- c("sensorType", "meter", "date",
                             "value", "STD", "var", "n")
colnames(subDailyLakeData) <- c("date", "value", "meter", "sensorType")
colnames(YSI) <- c("date", "meter", "temp", "do")
colnames(WQ) <- c("date", "site", "Total Phosphorus", "Total Nitrogen",
                  "Ammonium", "Soluble Reactive Phosphorus", "Chlorophyll A", "Iron")
colnames(WQDOC) <- c("date", "lake", "site", "Dissolved Organic Carbon", "n_reps", "min DOC mgl", "max DOC mgl")
colnames(WQSecchi) <- c("date", "secchi")

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
    selected = "High-Frequency Data"
    # selected = "Manual Sampling"
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
        "heatmapDateChoices",
        "Select Date Range",
        start = min(DailyHeat$date),
        end = max(DailyHeat$date),
        min = min(DailyHeat$date),
        max = max(DailyHeat$date),
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
          choices = WQChoices,
          selected = WQChoices[1]
        ),
        selectInput(
          "rightWQSelect",
          "Select Right Graph",
          choices = WQChoices,
          selected = WQChoices[2]
        )
      )
    }
  })

  # Select data based on user input
  selectedHF <- reactive({
    req(input$graphSelect)

    # DO data
    if (input$graphSelect == "Heatmap") {
      selected <- DailyHeat[DailyHeat$date >= input$heatmapDateChoices[1] &
                              DailyHeat$date <= input$heatmapDateChoices[2],]
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

  # Y-axis limits for left WQ graphs
  WQYScaleLeft <- reactive({
    req(input$leftWQSelect)

    if (input$leftWQSelect == "Total Phosphorus") {
      scale_y_continuous(breaks = seq(5, 25, by = 5), limits = c(5, 25))
    } else if (input$leftWQSelect == "Total Nitrogen") {
      scale_y_continuous(breaks = seq(0.1, 0.3, by = 0.1), limits = c(0.1, 0.3))
    } else if (input$leftWQSelect == "Iron") {
      scale_y_continuous(breaks = seq(0.04, 0.1, by = 0.02), limits = c(0.04, 0.1))
    } else if (input$leftWQSelect == "Dissolved Organic Carbon") {
      scale_y_continuous(breaks = seq(2, 4, by = 0.5), limits = c(2, 4))
    } else if (input$leftWQSelect == "Soluble Reactive Phosphorus") {
      scale_y_continuous(breaks = seq(0, 8, by = 2), limits = c(0, 8))
    } else if (input$leftWQSelect == "Ammonium") {
      scale_y_continuous(breaks = seq(0, 0.6, by = 0.2), limits = c(0, 0.6))
    } else if (input$leftWQSelect == "Chlorophyll A") {
      scale_y_continuous(breaks = seq(0, 15, by = 5), limits = c(0, 15))
    } else {
      scale_y_continuous()  # Default if no match
    }
  })

  # Y-axis limits for right WQ graphs
  WQYScaleRight <- reactive({
    req(input$rightWQSelect)

    if (input$rightWQSelect == "Total Phosphorus") {
      scale_y_continuous(breaks = seq(5, 25, by = 5), limits = c(5, 25))
    } else if (input$rightWQSelect == "Total Nitrogen") {
      scale_y_continuous(breaks = seq(0.1, 0.3, by = 0.1), limits = c(0.1, 0.3))
    } else if (input$rightWQSelect == "Iron") {
      scale_y_continuous(breaks = seq(0.04, 0.1, by = 0.02), limits = c(0.04, 0.1))
    } else if (input$rightWQSelect == "Dissolved Organic Carbon") {
      scale_y_continuous(breaks = seq(2, 4, by = 0.5), limits = c(2, 4))
    } else if (input$rightWQSelect == "Soluble Reactive Phosphorus") {
      scale_y_continuous(breaks = seq(0, 8, by = 2), limits = c(0, 8))
    } else if (input$rightWQSelect == "Ammonium") {
      scale_y_continuous(breaks = seq(0, 0.6, by = 0.2), limits = c(0, 0.6))
    } else if (input$rightWQSelect == "Chlorophyll A") {
      scale_y_continuous(breaks = seq(0, 15, by = 5), limits = c(0, 15))
    } else {
      scale_y_continuous()  # Default if no match
    }
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

  WQYAxisLabel <- function(selectedParam) {
    if (selectedParam == "Total Phosphorus") {
      "Total Phosphorus (µg/L)"
    } else if (selectedParam == "Total Nitrogen") {
      "Total Nitrogen (mg/L)"
    } else if (selectedParam == "Iron") {
      "Iron (mg/L)"
    } else if (selectedParam == "Dissolved Organic Carbon") {
      "Dissolved Organic Carbon (mg/L)"
    } else if (selectedParam == "Soluble Reactive Phosphorus") {
      "Soluble Reactive Phosphorus (µg/L)"
    } else if (selectedParam == "Ammonium") {
      "Ammonium (mg/L)"
    } else if (selectedParam == "Chlorophyll A") {
      "Chlorophyll A (µg/L)"
    } else {
      selectedParam
    }
  }

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
            "do" = scale_x_continuous(breaks = seq(0, 15, by = 3), limits = c(0, 15)),
            "temp" = scale_x_continuous(breaks = seq(0, 30, by = 5), limits = c(0, 30))
          )
        )
    }
  })

  # Water Quality Graph 1 (Left)
  output$msTabSplitLeft <- renderPlot({
    if (input$msTab == "Water Quality") {
      # Special case for Secchi Depth
      if (input$leftWQSelect == "Secchi Depth") {
        ggplot(WQSecchi, aes(x = date, y = secchi)) +
          geom_point(size = 4, color = "#1f908c") +
          geom_point(aes(fill = " "), alpha = 0) +
          labs(
            x = "",
            y = "Secchi Depth (m)",
            fill = " "
          ) +
          theme_bw() +
          scale_y_continuous(limits = c(0, 10)) +
          scale_fill_manual(values = c("white"), guide = guide_legend()) +
          theme(
            axis.title = element_text(size = 12),
            axis.text = element_text(size = 10),
            axis.text.x = element_text(angle = 45, hjust = 1),
            legend.position = "top"
          )
      } else {
        # Select correct dataset
        data_to_plot <- if (input$leftWQSelect == "Dissolved Organic Carbon") {
          WQDOC
        } else {
          WQ
        }

        ggplot(data_to_plot, aes(x = date, y = .data[[input$leftWQSelect]], color = site, shape = site)) +
          geom_point(size = 4) +
          scale_color_manual(values = c("EPI" = "yellow", "HYP" = "black")) +
          scale_shape_manual(values = c("EPI" = 19, "HYP" = 17)) +
          WQYScaleLeft() +
          labs(
            x = "",
            y = WQYAxisLabel(input$leftWQSelect),
            color = "",
            shape = ""
          ) +
          theme_bw() +
          theme(
            axis.title = element_text(size = 12),
            axis.text = element_text(size = 10),
            axis.text.x = element_text(angle = 45, hjust = 1),
            legend.text = element_text(color = "white"),
            legend.position = "top"
          ) +
          guides(
            color = guide_legend(title = NULL, override.aes = list(size = 0, shape = NA, color = "white")),
            shape = guide_legend(title = NULL, override.aes = list(size = 0, shape = NA))
          )
      }
    }
  })

  # Water Quality Graph 2 (Right)
  output$msTabSplitRight <- renderPlot({
    if (input$msTab == "Water Quality") {
      # Special case for Secchi Depth
      if (input$rightWQSelect == "Secchi Depth") {
        ggplot(WQSecchi, aes(x = date, y = secchi)) +
          geom_point(size = 4, color = "#1f908c") +
          geom_point(aes(fill = " "), alpha = 0) +
          labs(
            x = "",
            y = "Secchi Depth (m)",
            fill = " "
          ) +
          theme_bw() +
          scale_y_continuous(limits = c(0, 10)) +
          scale_fill_manual(values = c("white"), guide = guide_legend()) +
          theme(
            axis.title = element_text(size = 12),
            axis.text = element_text(size = 10),
            axis.text.x = element_text(angle = 45, hjust = 1),
            legend.position = "top"
          )
      } else {
        # Select correct dataset
        data_to_plot <- if (input$rightWQSelect == "Dissolved Organic Carbon") {
          WQDOC
        } else {
          WQ
        }

        ggplot(data_to_plot, aes(x = date, y = .data[[input$rightWQSelect]], color = site, shape = site)) +
          geom_point(size = 4) +
          scale_color_manual(values = c("EPI" = "yellow", "HYP" = "black")) +
          WQYScaleRight() +
          labs(
            x = "",
            y = WQYAxisLabel(input$rightWQSelect),
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
    }
  })
}

# Run the application
shinyApp(ui = ui, server = server)
