

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

# Hardcoded Dataset
energy <- data.frame(
  Sector = c("Residential", "Commercial", "Industrial",
             "Residential", "Commercial", "Industrial"),
  Region = c("North", "South", "West",
             "East", "North", "South"),
  Month = c("Jan", "Jan", "Feb", "Feb", "Mar", "Mar"),
  Temperature = c(15, 24, 20, 18, 28, 30),
  Units = c(320, 540, 880, 350, 610, 920),
  Cost = c(2100, 3600, 5900, 2300, 4100, 6200),
  Renewable = c(22, 18, 12, 25, 20, 15),
  PeakHours = c(4, 6, 8, 5, 7, 9)
)

ui <- fluidPage(
  
  titlePanel("Energy Consumption Analysis Dashboard"),
  
  fluidRow(
    
    column(
      6,
      plotOutput("histDensity", height = "350px")
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
      plotOutput("barChart", height = "350px")
    )
    
  ),
  
  br(),
  
  fluidRow(
    
    column(
      12,
      h3("Energy Consumption Data"),
      tableOutput("energyTable")
    )
    
  )
  
)

server <- function(input, output){
  
  # Histogram + Density Plot
  output$histDensity <- renderPlot({
    
    hist(
      energy$Units,
      probability = TRUE,
      col = "skyblue",
      border = "black",
      main = "Units Consumed Distribution",
      xlab = "Units Consumed (kWh)",
      ylab = "Density"
    )
    
    lines(
      density(energy$Units),
      col = "red",
      lwd = 2
    )
    
    legend(
      "topright",
      legend = c("Density Curve"),
      col = "red",
      lwd = 2
    )
    
  })
  
  # Bubble Scatter Plot
  output$scatterPlot <- renderPlot({
    
    symbols(
      energy$Temperature,
      energy$Units,
      circles = energy$PeakHours/5,
      inches = 0.3,
      bg = rgb(0,0,1,0.4),
      fg = "blue",
      xlab = "Temperature (°C)",
      ylab = "Units Consumed (kWh)",
      main = "Temperature vs Units Consumed"
    )
    
  })
  
  # Bar Chart
  output$barChart <- renderPlot({
    
    avgRenew <- tapply(
      energy$Renewable,
      energy$Sector,
      mean
    )
    
    barplot(
      avgRenew,
      col = c("orange","lightgreen","skyblue"),
      main = "Average Renewable Usage by Sector",
      xlab = "Sector",
      ylab = "Average Renewable Usage (%)"
    )
    
  })
  
  # Table
  output$energyTable <- renderTable({
    
    energy
    
  })
  
}

shinyApp(ui, server)
