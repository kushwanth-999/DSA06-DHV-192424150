
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
stocks <- data.frame(
  Date = as.Date(c("2023-01-01",
                   "2023-01-02",
                   "2023-01-03")),
  StockA = c(100,105,110),
  StockB = c(150,152,148),
  StockC = c(120,118,122)
)

# Percentage Change for Stock A
pctChange <- c(
  0,
  ((105-100)/100)*100,
  ((110-105)/105)*100
)

ui <- fluidPage(
  
  titlePanel("Stock Prices Analysis Dashboard"),
  
  fluidRow(
    
    column(
      6,
      plotOutput("lineChart", height = "350px")
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
      h3("Stock Price Data"),
      tableOutput("stockTable")
    )
    
  )
  
)

server <- function(input, output){
  
  # Line Chart
  output$lineChart <- renderPlot({
    
    plot(
      stocks$Date,
      stocks$StockA,
      type="o",
      col="blue",
      ylim=c(90,160),
      pch=16,
      lwd=2,
      xlab="Date",
      ylab="Stock Price ($)",
      main="Stock Prices Over Time"
    )
    
    lines(
      stocks$Date,
      stocks$StockB,
      type="o",
      col="red",
      pch=17,
      lwd=2
    )
    
    lines(
      stocks$Date,
      stocks$StockC,
      type="o",
      col="darkgreen",
      pch=15,
      lwd=2
    )
    
    legend(
      "topright",
      legend=c("Stock A","Stock B","Stock C"),
      col=c("blue","red","darkgreen"),
      pch=c(16,17,15),
      lty=1
    )
    
  })
  
  # Bar Chart
  output$barChart <- renderPlot({
    
    barplot(
      pctChange,
      names.arg=stocks$Date,
      col="orange",
      main="Daily Percentage Change - Stock A",
      xlab="Date",
      ylab="Percentage Change (%)"
    )
    
  })
  
  # Table
  output$stockTable <- renderTable({
    
    stocks
    
  })
  
}

shinyApp(ui, server)
