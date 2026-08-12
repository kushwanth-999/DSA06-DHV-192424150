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
social <- data.frame(
  PostID = c(1, 2, 3, 4, 5),
  Likes = c(120, 200, 150, 300, 180),
  Comments = c(15, 30, 20, 40, 25),
  Shares = c(10, 20, 12, 35, 18)
)

ui <- fluidPage(
  
  titlePanel("Social Media Engagement Analysis Dashboard"),
  
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
      h3("Social Media Posts Dataset"),
      tableOutput("socialTable")
    )
    
  )
  
)

server <- function(input, output){
  
  # 1. Histogram of Likes
  output$histPlot <- renderPlot({
    
    hist(
      social$Likes,
      col = "skyblue",
      border = "black",
      main = "Distribution of Likes",
      xlab = "Likes",
      ylab = "Frequency"
    )
    
  })
  
  # 2. Pie Chart of Total Engagement Components
  output$pieChart <- renderPlot({
    
    engagement <- c(
      Likes = sum(social$Likes),
      Comments = sum(social$Comments),
      Shares = sum(social$Shares)
    )
    
    pie(
      engagement,
      col = c("lightgreen", "orange", "tomato"),
      main = "Total Engagement Components"
    )
    
  })
  
  # 3. Bar Chart of Comments by Post
  output$barChart <- renderPlot({
    
    bp <- barplot(
      social$Comments,
      names.arg = social$PostID,
      col = "gold",
      ylim = c(0, max(social$Comments) + 10),
      main = "Comments by Post",
      xlab = "Post ID",
      ylab = "Number of Comments"
    )
    
    text(
      bp,
      social$Comments + 2,
      labels = social$Comments
    )
    
  })
  
  # 4. Scatter Plot of Likes vs Shares
  output$scatterPlot <- renderPlot({
    
    plot(
      social$Likes,
      social$Shares,
      pch = 19,
      col = "blue",
      xlab = "Likes",
      ylab = "Shares",
      main = "Likes vs Shares"
    )
    
    text(
      social$Likes,
      social$Shares,
      labels = social$PostID,
      pos = 3
    )
    
  })
  
  # Dataset Table
  output$socialTable <- renderTable({
    
    social
    
  })
  
}

shinyApp(ui, server)