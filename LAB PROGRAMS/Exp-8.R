

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
learning <- data.frame(
  StudentID = c("L01","L02","L03","L04","L05","L06"),
  Gender = c("Male","Female","Male","Female","Male","Female"),
  Age = c(20,22,19,21,23,20),
  Course = c("R","R","SQL","R","R","SQL"),
  StudyTime = c(3.5,4.2,2.0,5.0,2.5,4.0),
  VideosWatched = c(12,15,8,18,9,14),
  QuizScore = c(78,85,65,92,70,88),
  LoginDate = as.Date(c(
    "2025-01-05",
    "2025-01-05",
    "2025-02-08",
    "2025-02-08",
    "2025-03-12",
    "2025-03-12"
  ))
)

ui <- fluidPage(
  
  titlePanel("Online Learning Activity Analysis Dashboard"),
  
  fluidRow(
    
    column(
      6,
      plotOutput("histPlot", height="350px")
    ),
    
    column(
      6,
      plotOutput("boxPlot", height="350px")
    )
    
  ),
  
  br(),
  
  fluidRow(
    
    column(
      6,
      plotOutput("scatterPlot", height="350px")
    ),
    
    column(
      6,
      plotOutput("linePlot", height="350px")
    )
    
  ),
  
  br(),
  
  fluidRow(
    
    column(
      12,
      h3("Online Learning Activity Dataset"),
      tableOutput("dataTable")
    )
    
  )
  
)

server <- function(input, output){
  
  # Histogram
  output$histPlot <- renderPlot({
    
    hist(
      learning$QuizScore,
      col="skyblue",
      border="black",
      main="Histogram of Quiz Scores",
      xlab="Quiz Score",
      ylab="Frequency"
    )
    
  })
  
  # Boxplot
  output$boxPlot <- renderPlot({
    
    boxplot(
      QuizScore~Course,
      data=learning,
      col=c("orange","lightgreen"),
      main="Quiz Score by Course",
      xlab="Course",
      ylab="Quiz Score"
    )
    
  })
  
  # Scatter Plot
  output$scatterPlot <- renderPlot({
    
    symbols(
      learning$StudyTime,
      learning$QuizScore,
      circles=learning$VideosWatched/5,
      inches=0.25,
      bg=rgb(0,0,1,0.5),
      fg="blue",
      xlab="Study Time (hrs)",
      ylab="Quiz Score",
      main="Study Time vs Quiz Score"
    )
    
  })
  
  # Line Chart with Moving Average
  output$linePlot <- renderPlot({
    
    monthlyAvg <- tapply(
      learning$QuizScore,
      format(learning$LoginDate,"%Y-%m"),
      mean
    )
    
    months <- names(monthlyAvg)
    scores <- as.numeric(monthlyAvg)
    
    plot(
      scores,
      type="o",
      pch=16,
      col="blue",
      xaxt="n",
      xlab="Month",
      ylab="Average Quiz Score",
      main="Average Quiz Score Per Month"
    )
    
    axis(1, at=1:length(months), labels=months)
    
    movingAvg <- filter(scores, rep(1/2,2), sides=1)
    
    lines(
      movingAvg,
      col="red",
      lwd=2
    )
    
    legend(
      "topleft",
      legend=c("Average Score","Moving Average"),
      col=c("blue","red"),
      lty=1,
      pch=16
    )
    
  })
  
  # Dataset Table
  output$dataTable <- renderTable({
    learning
  })
  
}

shinyApp(ui,server)
