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

# Hardcoded Dataset
mobile <- data.frame(
  UserID = c("U01","U02","U03","U04","U05","U06"),
  Gender = c("Male","Female","Male","Female","Male","Female"),
  Age = c(20,22,19,21,23,20),
  ScreenTime = c(4.5,6.0,3.2,7.1,2.8,5.4),
  AppUsage = c(18,25,12,30,10,22),
  DataUsed = c(2.4,3.8,1.6,4.5,1.2,3.1),
  Satisfaction = c(3,5,3,5,2,4),
  UsageDate = as.Date(c(
    "2025-01-08",
    "2025-01-08",
    "2025-02-11",
    "2025-02-11",
    "2025-03-14",
    "2025-03-14"
  ))
)

ui <- fluidPage(
  
  titlePanel("Mobile App Usage Analysis Dashboard"),
  
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
      h3("Mobile App Usage Data"),
      tableOutput("usageTable")
    )
    
  )
  
)

server <- function(input, output){
  
  # 1. Histogram + Density Plot
  output$histDensity <- renderPlot({
    
    hist(
      mobile$ScreenTime,
      probability = TRUE,
      col = "skyblue",
      border = "black",
      main = "Screen Time Distribution",
      xlab = "Screen Time (Hours)",
      ylab = "Density"
    )
    
    lines(
      density(mobile$ScreenTime),
      col = "red",
      lwd = 2
    )
    
    legend(
      "topright",
      legend = "Density Curve",
      col = "red",
      lwd = 2
    )
    
  })
  
  # 2. Scatter Plot with Trend Line
  output$scatterPlot <- renderPlot({
    
    colors <- ifelse(mobile$Gender == "Male","blue","red")
    
    plot(
      mobile$DataUsed,
      mobile$ScreenTime,
      pch = 19,
      col = colors,
      xlab = "Data Used (GB)",
      ylab = "Screen Time (Hours)",
      main = "Data Used vs Screen Time"
    )
    
    abline(
      lm(ScreenTime ~ DataUsed, data = mobile),
      col = "darkgreen",
      lwd = 2
    )
    
    legend(
      "topleft",
      legend = c("Male","Female"),
      col = c("blue","red"),
      pch = 19
    )
    
  })
  
  # 3. Average Satisfaction Bar Chart
  output$barChart <- renderPlot({
    
    avgSat <- tapply(
      mobile$Satisfaction,
      mobile$Gender,
      mean
    )
    
    bp <- barplot(
      avgSat,
      col = c("orange","lightgreen"),
      ylim = c(0,6),
      main = "Average Satisfaction by Gender",
      xlab = "Gender",
      ylab = "Average Satisfaction"
    )
    
    text(
      bp,
      avgSat + 0.2,
      labels = round(avgSat,2),
      cex = 1
    )
    
  })
  
  # Dataset Table
  output$usageTable <- renderTable({
    
    mobile
    
  })
  
}

shinyApp(ui, server)