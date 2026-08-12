
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
patient <- data.frame(
  PatientID = c("P1","P2","P3","P4","P5"),
  Age = c(25,40,55,35,60),
  BMI = c(22,28,30,26,32),
  BP = c(120,135,145,130,150),
  Cholesterol = c(180,210,240,200,260)
)

ui <- fluidPage(
  
  titlePanel("Patient Health Risk Analysis Dashboard"),
  
  fluidRow(
    
    column(
      6,
      plotOutput("scatterMatrix", height = "350px")
    ),
    
    column(
      6,
      plotOutput("qqPlot", height = "350px")
    )
    
  ),
  
  br(),
  
  fluidRow(
    
    column(
      6,
      plotOutput("ecdfPlot", height = "350px")
    ),
    
    column(
      6,
      plotOutput("barChart", height = "350px")
    )
    
  ),
  
  br(),
  
  fluidRow(
    
    column(
      12,
      h3("Patient Health Data"),
      tableOutput("patientTable")
    )
    
  )
  
)

server <- function(input, output){
  
  # Scatterplot Matrix
  output$scatterMatrix <- renderPlot({
    
    pairs(
      patient[,2:5],
      main = "Scatterplot Matrix"
    )
    
  })
  
  # Q-Q Plot
  output$qqPlot <- renderPlot({
    
    qqnorm(
      patient$Cholesterol,
      main = "Q-Q Plot of Cholesterol"
    )
    
    qqline(
      patient$Cholesterol,
      col = "red",
      lwd = 2
    )
    
  })
  
  # ECDF Plot
  output$ecdfPlot <- renderPlot({
    
    plot(
      ecdf(patient$Cholesterol),
      main = "ECDF of Cholesterol",
      xlab = "Cholesterol",
      ylab = "ECDF",
      col = "blue",
      lwd = 2
    )
    
  })
  
  # Average Values Bar Chart
  output$barChart <- renderPlot({
    
    avg <- c(
      mean(patient$Age),
      mean(patient$BMI),
      mean(patient$BP),
      mean(patient$Cholesterol)
    )
    
    barplot(
      avg,
      names.arg = c("Age","BMI","BP","Cholesterol"),
      col = c("skyblue","orange","lightgreen","pink"),
      main = "Average Health Indicators",
      xlab = "Health Indicator",
      ylab = "Average Value"
    )
    
  })
  
  # Table
  output$patientTable <- renderTable({
    
    patient
    
  })
  
}

shinyApp(ui, server)