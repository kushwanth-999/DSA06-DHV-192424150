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
libraryData <- data.frame(
  UserID = c(1, 2, 3, 4, 5),
  BooksBorrowed = c(2, 5, 3, 6, 1),
  DaysKept = c(10, 25, 14, 30, 7),
  FineAmount = c(0, 15, 0, 20, 0)
)

ui <- fluidPage(
  
  titlePanel("Library Borrowing Records Dashboard"),
  
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
      h3("Library Borrowing Records"),
      tableOutput("libraryTable")
    )
    
  )
  
)

server <- function(input, output){
  
  # 1. Histogram of Books Borrowed
  output$histPlot <- renderPlot({
    
    hist(
      libraryData$BooksBorrowed,
      col = "skyblue",
      border = "black",
      main = "Distribution of Books Borrowed",
      xlab = "Books Borrowed",
      ylab = "Frequency"
    )
    
  })
  
  # 2. Pie Chart of Users With/Without Fines
  output$pieChart <- renderPlot({
    
    fineStatus <- ifelse(
      libraryData$FineAmount > 0,
      "With Fine",
      "No Fine"
    )
    
    pie(
      table(fineStatus),
      col = c("tomato", "lightgreen"),
      main = "Users With and Without Fines"
    )
    
  })
  
  # 3. Bar Chart of Fine Amount by User
  output$barChart <- renderPlot({
    
    bp <- barplot(
      libraryData$FineAmount,
      names.arg = libraryData$UserID,
      col = "orange",
      ylim = c(0, max(libraryData$FineAmount) + 5),
      main = "Fine Amount by User",
      xlab = "User ID",
      ylab = "Fine Amount"
    )
    
    text(
      bp,
      libraryData$FineAmount + 1,
      labels = libraryData$FineAmount
    )
    
  })
  
  # 4. Scatter Plot of Days Kept vs Fine Amount
  output$scatterPlot <- renderPlot({
    
    plot(
      libraryData$DaysKept,
      libraryData$FineAmount,
      pch = 19,
      col = "blue",
      xlab = "Days Kept",
      ylab = "Fine Amount",
      main = "Days Kept vs Fine Amount"
    )
    
    text(
      libraryData$DaysKept,
      libraryData$FineAmount,
      labels = libraryData$UserID,
      pos = 3
    )
    
  })
  
  # Dataset Table
  output$libraryTable <- renderTable({
    
    libraryData
    
  })
  
}

shinyApp(ui, server)