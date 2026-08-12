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
fitness <- data.frame(
  UserID = c(1, 2, 3, 4, 5),
  Steps = c(7000, 10000, 8500, 12000, 6500),
  CaloriesBurned = c(250, 400, 320, 500, 220),
  ActiveMinutes = c(40, 60, 50, 75, 35)
)

# Create Activity Level Categories
fitness$ActivityLevel <- cut(
  fitness$ActiveMinutes,
  breaks = c(0, 40, 60, Inf),
  labels = c("Low", "Moderate", "High"),
  include.lowest = TRUE
)

ui <- fluidPage(
  
  titlePanel("Fitness Tracker Analysis Dashboard"),
  
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
      h3("Daily Fitness Activity Dataset"),
      tableOutput("fitnessTable")
    )
    
  )
  
)

server <- function(input, output){
  
  # 1. Histogram of Daily Steps
  output$histPlot <- renderPlot({
    
    hist(
      fitness$Steps,
      col = "skyblue",
      border = "black",
      main = "Distribution of Daily Steps",
      xlab = "Steps",
      ylab = "Frequency"
    )
    
  })
  
  # 2. Pie Chart of Activity Levels
  output$pieChart <- renderPlot({
    
    activityCount <- table(fitness$ActivityLevel)
    
    pie(
      activityCount,
      col = c("lightgreen", "gold", "tomato"),
      main = "Activity Level Distribution"
    )
    
  })
  
  # 3. Bar Chart of Calories Burned by User
  output$barChart <- renderPlot({
    
    bp <- barplot(
      fitness$CaloriesBurned,
      names.arg = fitness$UserID,
      col = "orange",
      ylim = c(0, max(fitness$CaloriesBurned) + 100),
      main = "Calories Burned by User",
      xlab = "User ID",
      ylab = "Calories Burned"
    )
    
    text(
      bp,
      fitness$CaloriesBurned + 15,
      labels = fitness$CaloriesBurned
    )
    
  })
  
  # 4. Scatter Plot of Steps vs Calories Burned
  output$scatterPlot <- renderPlot({
    
    plot(
      fitness$Steps,
      fitness$CaloriesBurned,
      pch = 19,
      col = "blue",
      xlab = "Steps",
      ylab = "Calories Burned",
      main = "Steps vs Calories Burned"
    )
    
    text(
      fitness$Steps,
      fitness$CaloriesBurned,
      labels = fitness$UserID,
      pos = 3
    )
    
  })
  
  # Dataset Table
  output$fitnessTable <- renderTable({
    
    fitness
    
  })
  
}

shinyApp(ui, server)