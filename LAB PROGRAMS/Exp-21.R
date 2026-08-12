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
movies <- data.frame(
  MovieID = c(1, 2, 3, 4, 5),
  Genre = c("Action", "Comedy", "Drama", "Action", "Comedy"),
  Rating = c(4.5, 3.8, 4.2, 4.7, 3.5),
  Duration = c(120, 90, 140, 130, 95)
)

ui <- fluidPage(
  
  titlePanel("Movie Ratings Analysis Dashboard"),
  
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
      h3("Movie Ratings Dataset"),
      tableOutput("movieTable")
    )
    
  )
  
)

server <- function(input, output){
  
  # 1. Histogram of Ratings
  output$histPlot <- renderPlot({
    
    hist(
      movies$Rating,
      col = "skyblue",
      border = "black",
      main = "Distribution of Movie Ratings",
      xlab = "Rating",
      ylab = "Frequency"
    )
    
  })
  
  # 2. Pie Chart of Genre Distribution
  output$pieChart <- renderPlot({
    
    genreCount <- table(movies$Genre)
    
    pie(
      genreCount,
      col = c("tomato", "gold", "lightgreen"),
      main = "Genre Distribution"
    )
    
  })
  
  # 3. Average Rating by Genre
  output$barChart <- renderPlot({
    
    avgRating <- tapply(
      movies$Rating,
      movies$Genre,
      mean
    )
    
    bp <- barplot(
      avgRating,
      col = c("orange", "lightblue", "lightgreen"),
      ylim = c(0, 5),
      main = "Average Rating by Genre",
      xlab = "Genre",
      ylab = "Average Rating"
    )
    
    text(
      bp,
      avgRating + 0.1,
      labels = round(avgRating, 2)
    )
    
  })
  
  # 4. Scatter Plot
  output$scatterPlot <- renderPlot({
    
    plot(
      movies$Duration,
      movies$Rating,
      pch = 19,
      col = "blue",
      xlab = "Duration (Minutes)",
      ylab = "Rating",
      main = "Duration vs Rating"
    )
    
    text(
      movies$Duration,
      movies$Rating,
      labels = movies$MovieID,
      pos = 3
    )
    
  })
  
  # Dataset Table
  output$movieTable <- renderTable({
    
    movies
    
  })
  
}

shinyApp(ui, server)
