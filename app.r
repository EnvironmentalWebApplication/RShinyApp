library(shiny)
library(ggplot2)
library(colorRamps)
library(dplyr)
library(tidyr)
library(ggh4x)

#TODO Mandatory: Redeploy app to team rshinyapps.io account
#TODO Optional: switch heatmap to interpolated data, add new wq data,
#TODO Meeting topics:

# Load all data sets
dailyLakeData <- read.table("./data/DailyAverage.txt", header = TRUE, sep = "\t")
subDailyLakeData <- read.table("./data/SubDailyAverage.txt", header = TRUE, sep = "\t")
YSI <- read.table("./data/ysi.txt", header = TRUE, sep = "\t")
WQ <- read.table("./data/wq.txt", header = TRUE, sep = "\t")
WQDOC <- read.table("./data/2024_lng_DOC_for_analysis.txt", header = TRUE, sep = "\t")
WQSecchi <- read.table("./data/secchi.txt", header = TRUE, sep = "\t")

# Standardize column names on data sets
# If a data set is switched, check the column order.  If the order changes, update the order
# below.  Do not change the names of the columns.  This will effec the app If a new column
# is added, add it below.
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

  # tabsetPanel to hold multiple tabPanel elements
  tabsetPanel(

    # High-frequency data tab
    tabPanel("High-Frequency Data",
             sidebarLayout(
               sidebarPanel(
                 # Selection for HF graphs
                 selectInput(
                   "HFGraphSelect",
                   "Select Graph",
                   choices = c("Heatmap", "DO at Depth", "Temperature at Depth"),
                   selected = "Heatmap"
                 ),
                 uiOutput("HFGraphParameters"),
                 uiOutput("HFDateParameters")
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
                 # Selection for MS graphs
                 selectInput(
                   "msTab",
                   "Select Graph",
                   choices = c("YSI", "Water Quality"),
                   selected = "YSI"
                 ),
                 uiOutput("manualSamplingParameters")
               ),
               mainPanel(
                 # Conditional pannels based on user selected graph
                 # Full pannel if YSI, split pannel if WQ
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
  ),

  # Inject JS to disable keyboard for selectInput() function
  tags$script(HTML("
    document.addEventListener('DOMContentLoaded', function() {
      document.querySelectorAll('.selectize-input input').forEach(input => {
        input.setAttribute('readonly', true);
        input.setAttribute('inputmode', 'none');
      });
    });
  ")),
)

#     "We thank the New York State Parks, Recreation and Historic Preservation Grant #T003655 for funding this work. We thank the following people for assisting with data collection: Lauri Ahrens, Jenna Robinson, Caitlin Williams, Charles Stetler, and Katelyn Stetler. We thank numerous State Park and Grafton Lakes employees for assisting with logistics during the sampling season. We thank Kevin Rose for lending equipment (Turner C6) and sharing lab space.",
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

  #Format days in the data sets
  DailyDO$date <- as.Date(DailyDO$date)
  DailyHeat$date <- as.Date(DailyHeat$date)
  subDailyDO$date <- as.POSIXct(subDailyDO$date, format = "%Y-%m-%d %H:%M:%S")
  subDailyHeat$date <- as.POSIXct(subDailyHeat$date, format = "%Y-%m-%d %H:%M:%S")

  # User selection for HF graph parameters
  output$HFGraphParameters <- renderUI({
    # Heatmap parameter selection
    if (input$HFGraphSelect == "Heatmap") {
      # Placeholder to add graph parameters to heatmap if needed in the future
    }
    # DO plot parameter selection
    else if (input$HFGraphSelect == "DO at Depth") {
      tagList(
        # DO data frequency selection
        selectInput(
          "frequencyDO",
          "Select Display Frequency",
          choices = c("Daily Average", "Sub Daily"),
          selected = "Daily Average"
        ),
        # DO depth selection
        checkboxGroupInput(
          "doDepth",
          "Select Depths (Meters)",
          choices = doDepthChoices,
          selected = c("1.52", "2.52", "3.52", "4.52", "5.52", "6.52", "7.52")
        )
      )
    #Temperature plot parameter selection
    } else if (input$HFGraphSelect == "Temperature at Depth") {
      tagList(
        # Heat data frequency selection
        selectInput(
          "frequencyHeat",
          "Select Display Frequency",
          choices = c("Daily Average", "Sub Daily"),
          selected = "Daily Average"
        ),
        # Heat depth selection
        checkboxGroupInput(
          "heatDepth",
          "Select Depths (Meters)",
          choices = heatDepthChoices,
          selected = c("1.52", "2.52", "3.52", "4.52", "5.52", "6.52", "7.52")
        )
      )
    }

  })

  # User selection for HF data range (dates)
  output$HFDateParameters <- renderUI({
    #DO plot date selection
    if (input$HFGraphSelect == "DO at Depth") {
      dateRangeInput(
        "doDates",
        "Select Date Range",
        start = min(DailyDO$date),
        end = max(DailyDO$date),
        min = min(DailyDO$date),
        max = max(DailyDO$date),
        format = "mm/dd/yyyy"
      )
    # Heat plot date selection
    } else if (input$HFGraphSelect == "Temperature at Depth") {
      dateRangeInput(
        "heatDates",
        "Select Date Range",
        start = min(DailyHeat$date),
        end = max(DailyHeat$date),
        min = min(DailyHeat$date),
        max = max(DailyHeat$date),
        format = "mm/dd/yyyy"
      )
    # Heatmap plot date selection
    } else if (input$HFGraphSelect == "Heatmap") {
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

  #User selection for manual sampling graph parameters
  output$manualSamplingParameters <- renderUI({
    # YSI plots
    if (input$msTab == "YSI") {
      tagList(
        # YSI graph selection
        selectInput(
          "YSIGraphSelect",
          "Select graph",
          choices = c("Temperature", "DO", "Both"),
          selected = "Temperature"
        ),
        # YSI date range selection
        checkboxGroupInput(
          "YSIDateSelect",
          "Select Date Range",
          choices = unique(YSI$date),
          selected = unique(YSI$date)
        )
      )
    }
    # Water quality plots
    else if (input$msTab == "Water Quality") {
      tagList(
        # Left WQ plot selection
        selectInput(
          "leftWQSelect",
          "Select Left Graph",
          choices = WQChoices,
          selected = WQChoices[1]
        ),
        # Right WQ plot selection
        selectInput(
          "rightWQSelect",
          "Select Right Graph",
          choices = WQChoices,
          selected = WQChoices[2]
        )
      )
    }
  })

  # HF data reactive object
  selectedHF <- reactive({
    req(input$HFGraphSelect)

    # DO data
    if (input$HFGraphSelect == "Heatmap") {
      # Data is filtered based on user selected dates
      selected <- DailyHeat[DailyHeat$date >= input$heatmapDateChoices[1] &
                              DailyHeat$date <= input$heatmapDateChoices[2],]
    }
    # Heatmap data
    else if (input$HFGraphSelect == "DO at Depth") {
      req(input$frequencyDO)
      # Data is filtered based on user selected dates and depth
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
    else if (input$HFGraphSelect == "Temperature at Depth") {
      req(input$frequencyHeat)
      # Data is filtered based on user selected dates and depth
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

  # MS data reactive object
  selectedMS <- reactive({
    req(input$msTab, input$YSIGraphSelect, input$YSIDateSelect)

    # Only YSI has a reactive object
    if (input$YSIGraphSelect != "Both") {
      # Temp and DO plots are filtered based on user selected dates
      selected <- YSI[YSI$date %in% input$YSIDateSelect,]
    }
    else {
      # Combined plot is filtered based on data is reformated using pivot longer
      selected <-
        YSI %>%
          filter(date %in% input$YSIDateSelect) %>%
          pivot_longer(cols = c(temp, do), names_to = "Measurement", values_to = "Value")
    }
    return(selected)
  })

  # Reactive object for y-axis limits for left WQ graphs
  # Returns y-axis limits based on user selected plot
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

  # Reactive object for y-axis limits for right WQ graphs
  # Returns y-axis limits based on user selected plot
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

  # Manual color settings for depths in high-frequency data plots
  depthColors <- c("1.52" = "#f87a71", "2.52" = "#c39b24", "3.52" = "#54b321", "4.52" = "#05be95",
                   "5.52" = "#08b4e8", "6.52" = "#a68bfc", "7.52" = "#fb66d5")

  # Manual shape settings for depths in high-frequency data plots
  depthShapes <- c("1.52" = 21,  # Circle
                   "2.52" = 22,  # Square
                   "3.52" = 23,  # Diamond
                   "4.52" = 24,  # Triangle point-up
                   "5.52" = 4,   # X letter
                   "6.52" = 3,   # + sign
                   "7.52" = 25)  # Triangle point-down

  # Functions that returns y-axis lables for the water quality plots
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

  # High-frequency data graphs
  output$HFPlot <- renderPlot({
    req(selectedHF())

    # Heatmap
    if (input$HFGraphSelect == "Heatmap") {
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
    # DO plot
    } else if (input$HFGraphSelect == "DO at Depth") {
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
    # Temperature plot
    } else if (input$HFGraphSelect == "Temperature at Depth") {
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
    req(selectedMS())

    # YSI temperature plot
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
    #YSI DO plot
    } else if (input$YSIGraphSelect == "DO") {
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
    # YSI temperature and DO plot
    } else if (input$YSIGraphSelect == "Both") {
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

  # Water quality graph 1 (left)
  output$msTabSplitLeft <- renderPlot({
    req(input$msTab, input$leftWQSelect)

    if (input$msTab == "Water Quality") {
      # Special case for Secchi depth graph
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
        # Select correct dataset for non secchi graphs
        selectedData <- if (input$leftWQSelect == "Dissolved Organic Carbon") {
          WQDOC
        } else {
          WQ
        }

        # Every graph that isn't secchi, y-axis is what the user selects
        # Axis limits and labels are dynamically updated based on user selection
        ggplot(selectedData, aes(x = date, y = .data[[input$leftWQSelect]], color = site, shape = site)) +
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

  # Water quality graph 2 (right)
  output$msTabSplitRight <- renderPlot({
    req(input$msTab, input$rightWQSelect)

    if (input$msTab == "Water Quality") {
      # Special case for Secchi depth graph
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
        # Select correct dataset for non secchi graphs
        selectedData <- if (input$rightWQSelect == "Dissolved Organic Carbon") {
          WQDOC
        } else {
          WQ
        }

        # Every graph that isn't secchi, y-axis is what the user selects
        # Axis limits and labels are dynamically updated based on user selection
        ggplot(selectedData, aes(x = date, y = .data[[input$rightWQSelect]], color = site, shape = site)) +
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
