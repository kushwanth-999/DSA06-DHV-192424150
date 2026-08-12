
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
student <- data.frame(
  StudentID = c("S01","S02","S03","S04","S05","S06"),
  Gender = c("Male","Female","Male","Female","Male","Female"),
  StudyHours = c(2.0,3.5,1.5,4.0,2.8,3.0),
  Attendance = c(78,90,70,95,85,92),
  MathScore = c(62,80,55,90,72,82),
  ScienceScore = c(65,85,58,92,74,86),
  ExamDate = as.Date(c(
    "2025-01-10",
    "2025-01-10",
    "2025-02-12",
    "2025-02-12",
    "2025-03-15",
    "2025-03-15"
  ))
)

ui <- fluidPage(
  
  titlePanel("Student Mini Data Analysis Dashboard"),
  
  fluidRow(
    
    column(
      6,
      plotOutput("histPlot", height = "350px")
    ),
    
    column(
      6,
      plotOutput("boxPlot", height = "350px")
    )
    
  ),
  
  br(),
  
  fluidRow(
    
    column(
      6,
      plotOutput("scatterPlot", height = "350px")
    ),
    
    column(
      6,
      plotOutput("linePlot", height = "350px")
    )
    
  ),
  
  br(),
  
  fluidRow(
    
    column(
      12,
      h3("Student Dataset"),
      tableOutput("studentTable")
    )
    
  )
  
)

server <- function(input, output){
  
  # Histogram
  output$histPlot <- renderPlot({
    
    hist(
      student$MathScore,
      col = "skyblue",
      border = "black",
      main = "Distribution of Math Scores",
      xlab = "Math Score",
      ylab = "Frequency"
    )
    
  })
  
  # Boxplot
  output$boxPlot <- renderPlot({
    
    boxplot(
      ScienceScore ~ Gender,
      data = student,
      col = c("lightgreen","orange"),
      main = "Science Score by Gender",
      xlab = "Gender",
      ylab = "Science Score"
    )
    
  })
  
  # Scatter Plot with Regression Line
  output$scatterPlot <- renderPlot({
    
    colors <- ifelse(student$Gender=="Male","blue","red")
    
    plot(
      student$StudyHours,
      student$MathScore,
      pch = 19,
      col = colors,
      xlab = "Study Hours",
      ylab = "Math Score",
      main = "Study Hours vs Math Score"
    )
    
    abline(
      lm(MathScore ~ StudyHours, data=student),
      col="darkgreen",
      lwd=2
    )
    
    legend(
      "topleft",
      legend=c("Male","Female"),
      col=c("blue","red"),
      pch=19
    )
    
  })
  
  # Line Chart with Moving Average
  output$linePlot <- renderPlot({
    
    avgMath <- tapply(
      student$MathScore,
      format(student$ExamDate,"%Y-%m"),
      mean
    )
    
    months <- names(avgMath)
    scores <- as.numeric(avgMath)
    
    plot(
      scores,
      type="o",
      pch=16,
      col="blue",
      xaxt="n",
      xlab="Month",
      ylab="Average Math Score",
      main="Monthly Average Math Score"
    )
    
    axis(
      1,
      at=1:length(months),
      labels=months
    )
    
    movingAvg <- filter(
      scores,
      rep(1/2,2),
      sides=1
    )
    
    lines(
      movingAvg,
      col="red",
      lwd=2
    )
    
    legend(
      "topleft",
      legend=c("Average","Moving Average"),
      col=c("blue","red"),
      lty=1,
      pch=16
    )
    
  })
  
  # Dataset Table
  output$studentTable <- renderTable({
    
    student
    
  })
  
}

shinyApp(ui, server)
