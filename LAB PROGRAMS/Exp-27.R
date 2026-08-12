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
plant <- data.frame(
  PlantID = c(1, 2, 3, 4, 5),
  Output = c(120, 150, 100, 170, 110),
  Temperature = c(65, 70, 60, 75, 62),
  Status = c("Active", "Active", "Maintenance", "Active", "Maintenance")
)

ui <- fluidPage(
  
  titlePanel("Energy Plant Monitoring Dashboard"),
  
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
      h3("Power Plant Operations Dataset"),
      tableOutput("plantTable")
    )
    
  )
  
)

server <- function(input, output){
  
  # 1. Histogram of Power Output
  output$histPlot <- renderPlot({
    
    hist(
      plant$Output,
      col = "skyblue",
      border = "black",
      main = "Distribution of Power Output",
      xlab = "Output (MW)",
      ylab = "Frequency"
    )
    
  })
  
  # 2. Pie Chart of Plant Status
  output$pieChart <- renderPlot({
    
    statusCount <- table(plant$Status)
    
    pie(
      statusCount,
      col = c("lightgreen", "tomato"),
      main = "Plant Status Distribution"
    )
    
  })
  
  # 3. Bar Chart of Temperature by Plant
  output$barChart <- renderPlot({
    
    bp <- barplot(
      plant$Temperature,
      names.arg = plant$PlantID,
      col = "orange",
      ylim = c(0, max(plant$Temperature) + 10),
      main = "Temperature by Plant",
      xlab = "Plant ID",
      ylab = "Temperature (°C)"
    )
    
    text(
      bp,
      plant$Temperature + 2,
      labels = plant$Temperature
    )
    
  })
  
  # 4. Scatter Plot of Output vs Temperature
  output$scatterPlot <- renderPlot({
    
    plot(
      plant$Output,
      plant$Temperature,
      pch = 19,
      col = "blue",
      xlab = "Power Output (MW)",
      ylab = "Temperature (°C)",
      main = "Power Output vs Temperature"
    )
    
    text(
      plant$Output,
      plant$Temperature,
      labels = plant$PlantID,
      pos = 3
    )
    
  })
  
  # Dataset Table
  output$plantTable <- renderTable({
    
    plant
    
  })
  
}

shinyApp(ui, server)