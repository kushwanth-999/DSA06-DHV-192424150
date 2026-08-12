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
restaurant <- data.frame(
  OrderID = c(1, 2, 3, 4, 5),
  ItemsOrdered = c(2, 5, 3, 4, 2),
  BillAmount = c(25, 60, 35, 50, 20),
  DiningType = c("Dine-In", "Takeaway", "Dine-In", "Delivery", "Takeaway")
)

ui <- fluidPage(
  
  titlePanel("Restaurant Orders Analysis Dashboard"),
  
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
      h3("Restaurant Orders Dataset"),
      tableOutput("restaurantTable")
    )
    
  )
  
)

server <- function(input, output){
  
  # 1. Histogram of Bill Amounts
  output$histPlot <- renderPlot({
    
    hist(
      restaurant$BillAmount,
      col = "skyblue",
      border = "black",
      main = "Distribution of Bill Amounts",
      xlab = "Bill Amount",
      ylab = "Frequency"
    )
    
  })
  
  # 2. Pie Chart of Dining Types
  output$pieChart <- renderPlot({
    
    diningCount <- table(restaurant$DiningType)
    
    pie(
      diningCount,
      col = c("lightgreen", "orange", "tomato"),
      main = "Dining Type Distribution"
    )
    
  })
  
  # 3. Bar Chart of Items Ordered by Order
  output$barChart <- renderPlot({
    
    bp <- barplot(
      restaurant$ItemsOrdered,
      names.arg = restaurant$OrderID,
      col = "gold",
      ylim = c(0, max(restaurant$ItemsOrdered) + 2),
      main = "Items Ordered by Order",
      xlab = "Order ID",
      ylab = "Number of Items"
    )
    
    text(
      bp,
      restaurant$ItemsOrdered + 0.2,
      labels = restaurant$ItemsOrdered
    )
    
  })
  
  # 4. Scatter Plot of Items Ordered vs Bill Amount
  output$scatterPlot <- renderPlot({
    
    plot(
      restaurant$ItemsOrdered,
      restaurant$BillAmount,
      pch = 19,
      col = "blue",
      xlab = "Items Ordered",
      ylab = "Bill Amount",
      main = "Items Ordered vs Bill Amount"
    )
    
    text(
      restaurant$ItemsOrdered,
      restaurant$BillAmount,
      labels = restaurant$OrderID,
      pos = 3
    )
    
  })
  
  # Dataset Table
  output$restaurantTable <- renderTable({
    
    restaurant
    
  })
  
}

shinyApp(ui, server)