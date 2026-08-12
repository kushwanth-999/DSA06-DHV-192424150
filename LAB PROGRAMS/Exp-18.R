library(shiny)

# Hardcoded Dataset
student <- data.frame(
  StudentID = c("S1","S2","S3","S4","S5"),
  Age = c(19,21,20,22,23),
  StudyHours = c(12,8,15,10,7),
  Attendance = c(90,70,95,85,60),
  TestScore = c(85,70,92,80,65),
  ParticipationScore = c(8,7,9,8,6)
)

# Hardcoded Attendance Quartiles
student$Quartile <- factor(
  c("Q3","Q2","Q4","Q3","Q1"),
  levels = c("Q1","Q2","Q3","Q4")
)

ui <- fluidPage(
  
  titlePanel("Student Academic Performance Dashboard"),
  
  fluidRow(
    
    column(
      6,
      plotOutput("areaChart", height = "350px")
    ),
    
    column(
      6,
      plotOutput("boxPlot", height = "350px")
    )
    
  ),
  
  br(),
  
  fluidRow(
    
    column(
      12,
      plotOutput("densityPlot", height = "350px")
    )
    
  ),
  
  br(),
  
  fluidRow(
    
    column(
      12,
      h3("Student Academic Dataset"),
      tableOutput("studentTable")
    )
    
  )
  
)

server <- function(input, output){
  
  # ----------------------------
  # 1. Stacked Area Chart
  # ----------------------------
  output$areaChart <- renderPlot({
    
    x <- 1:nrow(student)
    
    test <- student$TestScore
    participation <- student$ParticipationScore
    
    total <- test + participation
    
    plot(
      x,
      total,
      type = "n",
      xaxt = "n",
      xlab = "Students",
      ylab = "Score",
      main = "Test Score and Participation Score"
    )
    
    axis(
      1,
      at = x,
      labels = student$StudentID
    )
    
    polygon(
      c(x, rev(x)),
      c(rep(0, length(x)), rev(test)),
      col = "skyblue",
      border = NA
    )
    
    polygon(
      c(x, rev(x)),
      c(test, rev(total)),
      col = "orange",
      border = NA
    )
    
    legend(
      "topleft",
      legend = c("Test Score", "Participation Score"),
      fill = c("skyblue", "orange")
    )
    
  })
  
  # ----------------------------
  # 2. Boxplot
  # ----------------------------
  output$boxPlot <- renderPlot({
    
    boxplot(
      StudyHours ~ Quartile,
      data = student,
      col = c("lightblue","lightgreen","orange","pink"),
      main = "Study Hours by Attendance Quartile",
      xlab = "Attendance Quartile",
      ylab = "Study Hours"
    )
    
  })
  
  # ----------------------------
  # 3. Density Plot
  # ----------------------------
  output$densityPlot <- renderPlot({
    
    d <- density(student$TestScore)
    
    plot(
      d,
      col = "blue",
      lwd = 2,
      main = "Density Plot of Test Scores",
      xlab = "Test Score",
      ylab = "Density"
    )
    
    polygon(
      d,
      col = rgb(0,0,1,0.3),
      border = "blue"
    )
    
  })
  
  # ----------------------------
  # 4. Table
  # ----------------------------
  output$studentTable <- renderTable({
    
    student[,1:6]
    
  })
  
}

shinyApp(ui, server)