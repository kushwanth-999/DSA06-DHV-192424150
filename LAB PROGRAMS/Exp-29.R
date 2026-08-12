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
water <- data.frame(
  SampleID = c(1, 2, 3, 4, 5),
  pHLevel = c(7.2, 6.8, 7.5, 6.5, 7.1),
  Turbidity = c(3, 5, 2, 7, 4),
  Quality = c("Good", "Fair", "Good", "Poor", "Fair")
)

ui <- fluidPage(
  
  titlePanel("Water Quality Monitoring Dashboard"),
  
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
      h3("Water Quality Dataset"),
      tableOutput("waterTable")
    )
    
  )
  
)

server <- function(input, output){
  
  # 1. Histogram of pH Levels
  output$histPlot <- renderPlot({
    
    hist(
      water$pHLevel,
      col = "skyblue",
      border = "black",
      main = "Distribution of pH Levels",
      xlab = "pH Level",
      ylab = "Frequency"
    )
    
  })
  
  # 2. Pie Chart of Water Quality Categories
  output$pieChart <- renderPlot({
    
    qualityCount <- table(water$Quality)
    
    pie(
      qualityCount,
      col = c("lightgreen", "gold", "tomato"),
      main = "Water Quality Categories"
    )
    
  })
  
  # 3. Bar Chart of Turbidity by Sample
  output$barChart <- renderPlot({
    
    bp <- barplot(
      water$Turbidity,
      names.arg = water$SampleID,
      col = "orange",
      ylim = c(0, max(water$Turbidity) + 2),
      main = "Turbidity by Sample",
      xlab = "Sample ID",
      ylab = "Turbidity"
    )
    
    text(
      bp,
      water$Turbidity + 0.2,
      labels = water$Turbidity
    )
    
  })
  
  # 4. Scatter Plot of pH Level vs Turbidity
  output$scatterPlot <- renderPlot({
    
    plot(
      water$pHLevel,
      water$Turbidity,
      pch = 19,
      col = "blue",
      xlab = "pH Level",
      ylab = "Turbidity",
      main = "pH Level vs Turbidity"
    )
    
    text(
      water$pHLevel,
      water$Turbidity,
      labels = water$SampleID,
      pos = 3
    )
    
  })
  
  # Dataset Table
  output$waterTable <- renderTable({
    
    water
    
  })
  
}

shinyApp(ui, server)