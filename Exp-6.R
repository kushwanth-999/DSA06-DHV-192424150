

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
sales <- data.frame(
  ProductID = c(1, 2, 3),
  ProductName = c("Product A", "Product B", "Product C"),
  January = c(2000, 1500, 1200),
  February = c(2200, 1800, 1400),
  March = c(2400, 1600, 1100)
)

ui <- fluidPage(
  
  titlePanel("Product Sales Analysis Dashboard"),
  
  fluidRow(
    
    column(
      6,
      plotOutput("groupBar", height = "350px")
    ),
    
    column(
      6,
      plotOutput("areaChart", height = "350px")
    )
    
  ),
  
  br(),
  
  fluidRow(
    
    column(
      12,
      h4("Monthly Product Sales Data"),
      tableOutput("salesTable")
    )
    
  )
  
)

server <- function(input, output){
  
  # Grouped Bar Chart
  output$groupBar <- renderPlot({
    
    salesMatrix <- rbind(
      sales$January,
      sales$February,
      sales$March
    )
    
    barplot(
      salesMatrix,
      beside = TRUE,
      col = c("red", "green", "blue"),
      names.arg = sales$ProductName,
      main = "Quarter 1 Product Sales",
      xlab = "Products",
      ylab = "Sales",
      legend.text = c("January", "February", "March")
    )
    
  })
  
  # Stacked Area Chart
  output$areaChart <- renderPlot({
    
    x <- 1:3
    
    y <- rbind(
      sales$January,
      sales$February,
      sales$March
    )
    
    colors <- c("red", "green", "blue")
    
    plot(
      x,
      c(0,0,0),
      type = "n",
      xlim = c(1,3),
      ylim = c(0,6000),
      xaxt = "n",
      xlab = "Month",
      ylab = "Sales",
      main = "Overall Sales Trend"
    )
    
    axis(
      1,
      at = x,
      labels = c("January","February","March")
    )
    
    polygon(
      c(x, rev(x)),
      c(rep(0,3), rev(sales$January)),
      col = colors[1],
      border = NA
    )
    
    polygon(
      c(x, rev(x)),
      c(sales$January,
        rev(sales$January + sales$February)),
      col = colors[2],
      border = NA
    )
    
    polygon(
      c(x, rev(x)),
      c(sales$January + sales$February,
        rev(sales$January + sales$February + sales$March)),
      col = colors[3],
      border = NA
    )
    
    legend(
      "topleft",
      legend = sales$ProductName,
      fill = colors
    )
    
  })
  
  # Table
  output$salesTable <- renderTable({
    sales
  })
  
}

shinyApp(ui, server)
