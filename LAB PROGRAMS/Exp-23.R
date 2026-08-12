#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
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
airline <- data.frame(
  PassengerID = c(1, 2, 3, 4, 5),
  Age = c(28, 45, 33, 52, 39),
  FlightHours = c(2, 8, 5, 10, 6),
  Satisfaction = c("High", "Medium", "High", "Low", "Medium")
)

ui <- fluidPage(
  
  titlePanel("Airline Passenger Analysis Dashboard"),
  
  fluidRow(
    
    column(
      6,
      plotOutput("histPlot", height = "350px")
    ),
    
    column(
      6,
      plotOutput("pieChart", height = "350px")
    )
    
  ),
  
  br(),
  
  fluidRow(
    
    column(
      6,
      plotOutput("barChart", height = "350px")
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
      h3("Airline Passenger Dataset"),
      tableOutput("airlineTable")
    )
    
  )
  
)

server <- function(input, output){
  
  # 1. Histogram of Passenger Ages
  output$histPlot <- renderPlot({
    
    hist(
      airline$Age,
      col = "skyblue",
      border = "black",
      main = "Distribution of Passenger Ages",
      xlab = "Age",
      ylab = "Frequency"
    )
    
  })
  
  # 2. Pie Chart of Satisfaction Levels
  output$pieChart <- renderPlot({
    
    satisfactionCount <- table(airline$Satisfaction)
    
    pie(
      satisfactionCount,
      col = c("lightgreen", "gold", "tomato"),
      main = "Passenger Satisfaction Levels"
    )
    
  })
  
  # 3. Bar Chart of Flight Hours by Passenger
  output$barChart <- renderPlot({
    
    bp <- barplot(
      airline$FlightHours,
      names.arg = airline$PassengerID,
      col = "orange",
      ylim = c(0, max(airline$FlightHours) + 2),
      main = "Flight Hours by Passenger",
      xlab = "Passenger ID",
      ylab = "Flight Hours"
    )
    
    text(
      bp,
      airline$FlightHours + 0.5,
      labels = airline$FlightHours
    )
    
  })
  
  # 4. Scatter Plot of Age vs Flight Hours
  output$scatterPlot <- renderPlot({
    
    plot(
      airline$Age,
      airline$FlightHours,
      pch = 19,
      col = "blue",
      xlab = "Age",
      ylab = "Flight Hours",
      main = "Age vs Flight Hours"
    )
    
    text(
      airline$Age,
      airline$FlightHours,
      labels = airline$PassengerID,
      pos = 3
    )
    
  })
  
  # Dataset Table
  output$airlineTable <- renderTable({
    
    airline
    
  })
  
}

shinyApp(ui, server)