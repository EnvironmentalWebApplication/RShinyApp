library(shiny)
library(ggplot2)

# Define UI
ui <- fluidPage(
  titlePanel("Disolved Oxygen (DO) Levels From May to Early July"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("meter", 
                  "Select Depth (Meters):", 
                  min = 1, 
                  max = 7, 
                  value = 1, 
                  step = 5,
                  ticks = TRUE)
    ),
    mainPanel(
      plotOutput("sensorPlot")
    )
  )
)

# Define server logic
server <- function(input, output) {

  filtered_data <- reactive({
    df <- read.csv("Cleaned_LongPond_08082024.csv")
    
    # Filter the data based on the selected meter
    df_filtered <- df[df$sensor_type == "DO" & df$meter == input$meter, ]
    
    return(df_filtered)
  })
  
  # Render the plot reactively
  output$sensorPlot <- renderPlot({
    ggplot(filtered_data(), aes(x = date, y = Value)) +
      geom_point() +
      labs(title = paste("DO Sensor at", input$meter, "Meters")) +
      xlab("Date") + ylab("DO (mg/L)") +
      theme_bw()
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
