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
hospital <- data.frame(
  PatientID = c(1, 2, 3, 4, 5),
  Age = c(25, 40, 35, 50, 29),
  WaitingTime = c(2, 5, 1, 7, 3),
  AppointmentStatus = c("Attended", "Missed", "Attended", "Missed", "Attended")
)

ui <- fluidPage(
  
  titlePanel("Hospital Appointment Analysis Dashboard"),
  
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
      h3("Hospital Appointment Data"),
      tableOutput("hospitalTable")
    )
    
  )
  
)

server <- function(input, output){
  
  # 1. Histogram of Patient Ages
  output$histPlot <- renderPlot({
    
    hist(
      hospital$Age,
      col = "skyblue",
      border = "black",
      main = "Distribution of Patient Ages",
      xlab = "Age",
      ylab = "Frequency"
    )
    
  })
  
  # 2. Pie Chart of Appointment Status
  output$pieChart <- renderPlot({
    
    status <- table(hospital$AppointmentStatus)
    
    pie(
      status,
      col = c("lightgreen", "tomato"),
      main = "Appointment Status Distribution"
    )
    
  })
  
  # 3. Bar Chart of Waiting Time
  output$barChart <- renderPlot({
    
    barplot(
      hospital$WaitingTime,
      names.arg = hospital$PatientID,
      col = "orange",
      main = "Waiting Time by Patient",
      xlab = "Patient ID",
      ylab = "Waiting Time (Days)"
    )
    
  })
  
  # 4. Scatter Plot
  output$scatterPlot <- renderPlot({
    
    plot(
      hospital$Age,
      hospital$WaitingTime,
      pch = 19,
      col = "blue",
      xlab = "Age",
      ylab = "Waiting Time (Days)",
      main = "Age vs Waiting Time"
    )
    
    text(
      hospital$Age,
      hospital$WaitingTime,
      labels = hospital$PatientID,
      pos = 3
    )
    
  })
  
  # Dataset Table
  output$hospitalTable <- renderTable({
    
    hospital
    
  })
  
}

shinyApp(ui, server)