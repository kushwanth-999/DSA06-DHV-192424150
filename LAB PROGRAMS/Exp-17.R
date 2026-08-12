#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Old Faithful Geyser Data"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("bins",
                        "Number of bins:",
                        min = 1,
                        max = 50,
                        value = 30)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white',
             xlab = 'Waiting time to next eruption (in mins)',
             main = 'Histogram of waiting times')
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
library(shiny)
library(ggplot2)
library(corrplot)

# Hardcoded Dataset
vehicle <- data.frame(
  VehicleID = c("V1","V2","V3","V4","V5"),
  EngineSize = c(1.5,2.0,3.0,2.5,1.8),
  Horsepower = c(110,150,250,200,130),
  FuelEfficiency = c(18,15,12,14,17),
  TopSpeed = c(180,200,250,220,190),
  SafetyRating = factor(c(4,5,5,4,3))
)

ui <- fluidPage(
  
  titlePanel("Vehicle Performance Analysis Dashboard"),
  
  fluidRow(
    
    column(
      6,
      plotOutput("violinPlot", height = "350px")
    ),
    
    column(
      6,
      plotOutput("scatterPlot", height = "350px")
    )
    
  ),
  
  br(),
  
  fluidRow(
    
    column(
      12,
      plotOutput("heatmapPlot", height = "400px")
    )
    
  ),
  
  br(),
  
  fluidRow(
    
    column(
      12,
      h3("Vehicle Performance Data"),
      tableOutput("vehicleTable")
    )
    
  )
  
)

server <- function(input, output){
  
  # 1. Violin Plot
  output$violinPlot <- renderPlot({
    
    ggplot(vehicle,
           aes(x = SafetyRating,
               y = FuelEfficiency,
               fill = SafetyRating)) +
      
      geom_violin(trim = FALSE) +
      
      geom_boxplot(width = 0.1,
                   fill = "white") +
      
      labs(
        title = "Fuel Efficiency by Safety Rating",
        x = "Safety Rating",
        y = "Fuel Efficiency (km/l)"
      )
    
  })
  
  # 2. Scatter Plot
  output$scatterPlot <- renderPlot({
    
    colors <- c("blue","green","orange","red","purple")
    
    plot(
      vehicle$Horsepower,
      vehicle$TopSpeed,
      pch = 19,
      cex = vehicle$EngineSize,
      col = colors,
      xlab = "Horsepower",
      ylab = "Top Speed (km/h)",
      main = "Horsepower vs Top Speed"
    )
    
    text(
      vehicle$Horsepower,
      vehicle$TopSpeed,
      labels = vehicle$VehicleID,
      pos = 3
    )
    
  })
  
  # 3. Correlation Heatmap
  output$heatmapPlot <- renderPlot({
    
    numericData <- vehicle[,2:5]
    
    corrplot(
      cor(numericData),
      method = "color",
      addCoef.col = "black",
      tl.col = "black",
      number.cex = 0.8
    )
    
  })
  
  # Table
  output$vehicleTable <- renderTable({
    
    vehicle
    
  })
  
}

shinyApp(ui, server)