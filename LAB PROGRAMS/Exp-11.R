
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
library(maps)

# Hardcoded Dataset
geo <- data.frame(
  City = c("City A", "City B", "City C"),
  Population = c(500000, 700000, 600000),
  AvgTemperature = c(75, 68, 80),
  Elevation = c(1000, 800, 1200),
  Latitude = c(40.71, 41.88, 34.05),
  Longitude = c(-74.00, -87.63, -118.24)
)

ui <- fluidPage(
  
  titlePanel("Geographic Data Analysis Dashboard"),
  
  fluidRow(
    
    column(
      6,
      plotOutput("mapChart", height = "350px")
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
      h3("Geographic Data"),
      tableOutput("geoTable")
    )
    
  )
  
)

server <- function(input, output){
  
  # Map Chart
  output$mapChart <- renderPlot({
    
    map("world",
        fill = TRUE,
        col = "lightyellow",
        bg = "lightblue",
        mar = c(0,0,0,0))
    
    points(
      geo$Longitude,
      geo$Latitude,
      pch = 19,
      col = "red",
      cex = 2
    )
    
    text(
      geo$Longitude,
      geo$Latitude,
      labels = geo$City,
      pos = 4,
      cex = 0.9
    )
    
    title("Geographic Distribution of Cities")
    
  })
  
  # Scatter Plot
  output$scatterPlot <- renderPlot({
    
    plot(
      geo$AvgTemperature,
      geo$Population,
      pch = 19,
      col = "blue",
      cex = 2,
      xlab = "Average Temperature (°F)",
      ylab = "Population",
      main = "Temperature vs Population"
    )
    
    text(
      geo$AvgTemperature,
      geo$Population,
      labels = geo$City,
      pos = 3
    )
    
  })
  
  # Table
  output$geoTable <- renderTable({
    
    geo[,1:4]
    
  })
  
}

shinyApp(ui, server)
