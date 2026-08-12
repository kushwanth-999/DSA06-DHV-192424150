
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
survey <- data.frame(
  SurveyID = c(1,2,3),
  Question1 = c("A","B","C"),
  Question2 = c("B","A","A"),
  Question3 = c("C","D","B")
)

ui <- fluidPage(
  
  titlePanel("Survey Responses Analysis Dashboard"),
  
  fluidRow(
    
    column(
      6,
      plotOutput("groupBar", height = "350px")
    ),
    
    column(
      6,
      plotOutput("stackBar", height = "350px")
    )
    
  ),
  
  br(),
  
  fluidRow(
    
    column(
      12,
      h3("Survey Response Data"),
      tableOutput("surveyTable")
    )
    
  )
  
)

server <- function(input, output){
  
  # 1. Grouped Bar Chart
  output$groupBar <- renderPlot({
    
    q1 <- table(survey$Question1)
    
    barplot(
      q1,
      beside = TRUE,
      col = c("skyblue","orange","lightgreen"),
      main = "Distribution of Question 1 Responses",
      xlab = "Response",
      ylab = "Frequency"
    )
    
  })
  
  # 2. Stacked Bar Chart
  output$stackBar <- renderPlot({
    
    responses <- rbind(
      table(factor(survey$Question1,
                   levels=c("A","B","C","D"))),
      
      table(factor(survey$Question2,
                   levels=c("A","B","C","D"))),
      
      table(factor(survey$Question3,
                   levels=c("A","B","C","D")))
    )
    
    colnames(responses) <- c("A","B","C","D")
    rownames(responses) <- c("Question 1","Question 2","Question 3")
    
    barplot(
      t(responses),
      col = c("red","green","blue","gold"),
      main = "Responses for All Questions",
      xlab = "Questions",
      ylab = "Number of Responses",
      legend.text = c("A","B","C","D")
    )
    
  })
  
  # 3. Survey Table
  output$surveyTable <- renderTable({
    
    survey
    
  })
  
}

shinyApp(ui, server)
