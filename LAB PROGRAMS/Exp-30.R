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
music <- data.frame(
  SongID = c(1, 2, 3, 4, 5),
  Duration = c(3.5, 4.2, 3.8, 5.0, 4.1),
  Streams = c(150, 200, 180, 250, 170),
  Genre = c("Pop", "Rock", "Pop", "Hip-Hop", "Rock")
)

ui <- fluidPage(
  
  titlePanel("Music Streaming Analysis Dashboard"),
  
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
      h3("Music Streaming Dataset"),
      tableOutput("musicTable")
    )
    
  )
  
)

server <- function(input, output){
  
  # 1. Histogram of Song Duration
  output$histPlot <- renderPlot({
    
    hist(
      music$Duration,
      col = "skyblue",
      border = "black",
      main = "Distribution of Song Durations",
      xlab = "Duration (Minutes)",
      ylab = "Frequency"
    )
    
  })
  
  # 2. Pie Chart of Genre Distribution
  output$pieChart <- renderPlot({
    
    genreCount <- table(music$Genre)
    
    pie(
      genreCount,
      col = c("lightgreen", "orange", "tomato"),
      main = "Genre Distribution"
    )
    
  })
  
  # 3. Bar Chart of Average Streams by Genre
  output$barChart <- renderPlot({
    
    avgStreams <- tapply(
      music$Streams,
      music$Genre,
      mean
    )
    
    bp <- barplot(
      avgStreams,
      col = c("gold", "lightblue", "lightgreen"),
      ylim = c(0, max(avgStreams) + 50),
      main = "Average Streams by Genre",
      xlab = "Genre",
      ylab = "Average Streams (000s)"
    )
    
    text(
      bp,
      avgStreams + 10,
      labels = round(avgStreams, 1)
    )
    
  })
  
  # 4. Scatter Plot of Duration vs Streams
  output$scatterPlot <- renderPlot({
    
    plot(
      music$Duration,
      music$Streams,
      pch = 19,
      col = "blue",
      xlab = "Duration (Minutes)",
      ylab = "Streams (000s)",
      main = "Duration vs Streams"
    )
    
    text(
      music$Duration,
      music$Streams,
      labels = music$SongID,
      pos = 3
    )
    
  })
  
  # Dataset Table
  output$musicTable <- renderTable({
    
    music
    
  })
  
}

shinyApp(ui, server)