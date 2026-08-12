

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
employee <- data.frame(
  EmployeeID = c(1, 2, 3, 4, 5),
  Department = c("Sales", "HR", "Marketing", "Sales", "HR"),
  YearsOfService = c(5, 3, 7, 4, 2),
  PerformanceScore = c(85, 92, 78, 90, 76)
)

ui <- fluidPage(
  
  titlePanel("Employee Performance Evaluation"),
  
  tabsetPanel(
    
    tabPanel("Dataset",
             tableOutput("table")),
    
    tabPanel("Line Chart",
             plotOutput("line")),
    
    tabPanel("Bar Chart",
             plotOutput("bar")),
    
    tabPanel("Scatter Plot",
             plotOutput("scatter")),
    
    tabPanel("Dashboard",
             
             fluidRow(
               
               column(6,
                      plotOutput("dashboardLine")),
               
               column(6,
                      plotOutput("dashboardBar"))
               
             )
    )
    
  )
)

server <- function(input, output){
  
  # Dataset
  output$table <- renderTable({
    employee
  })
  
  # 1. Line Chart
  output$line <- renderPlot({
    
    plot(
      employee$EmployeeID,
      employee$PerformanceScore,
      type = "o",
      col = "blue",
      pch = 16,
      lwd = 2,
      xlab = "Employee ID",
      ylab = "Performance Score",
      main = "Employee Performance Trend"
    )
    
    legend(
      "bottomright",
      legend = "Performance",
      col = "blue",
      lty = 1,
      pch = 16
    )
    
  })
  
  # 2. Bar Chart
  output$bar <- renderPlot({
    
    dept <- table(employee$Department)
    
    barplot(
      dept,
      col = c("orange", "skyblue", "green"),
      main = "Employees by Department",
      xlab = "Department",
      ylab = "Number of Employees"
    )
    
  })
  
  # 3. Scatter Plot
  output$scatter <- renderPlot({
    
    plot(
      employee$YearsOfService,
      employee$PerformanceScore,
      pch = 19,
      col = "red",
      xlab = "Years of Service",
      ylab = "Performance Score",
      main = "Years of Service vs Performance"
    )
    
    abline(
      lm(PerformanceScore ~ YearsOfService, data = employee),
      col = "blue",
      lwd = 2
    )
    
  })
  
  # Dashboard Charts
  output$dashboardLine <- renderPlot({
    
    plot(
      employee$EmployeeID,
      employee$PerformanceScore,
      type = "o",
      col = "blue",
      pch = 16,
      lwd = 2,
      xlab = "Employee ID",
      ylab = "Performance Score",
      main = "Performance Trend"
    )
    
  })
  
  output$dashboardBar <- renderPlot({
    
    dept <- table(employee$Department)
    
    barplot(
      dept,
      col = c("orange", "skyblue", "green"),
      main = "Department Distribution",
      xlab = "Department",
      ylab = "Employees"
    )
    
  })
  
}

shinyApp(ui, server)
