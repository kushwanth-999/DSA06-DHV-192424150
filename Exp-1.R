library(shiny)
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
library(ggplot2)
library(plotly)

#-----------------------------
# Data
#-----------------------------

sales_data <- data.frame(
  Month = c("January", "February", "March", "April", "May"),
  Sales = c(15000, 18000, 22000, 20000, 23000)
)

product_data <- data.frame(
  Product = c("Product A","Product B","Product C","Product D","Product E"),
  Sales = c(50000,42000,38000,30000,27000)
)

scatter_data <- data.frame(
  Advertising = c(3000,3500,4200,3900,4500),
  Sales = c(15000,18000,22000,20000,23000)
)

#-----------------------------
# User Interface
#-----------------------------

ui <- fluidPage(
  
  titlePanel("Sales Analysis Dashboard"),
  
  fluidRow(
    
    column(
      6,
      plotlyOutput("linePlot", height = "350px")
    ),
    
    column(
      6,
      plotlyOutput("barPlot", height = "350px")
    )
    
  ),
  
  br(),
  
  fluidRow(
    
    column(
      12,
      plotlyOutput("scatterPlot", height = "400px")
    )
    
  )
  
)

#-----------------------------
# Server
#-----------------------------

server <- function(input, output) {
  
  output$linePlot <- renderPlotly({
    
    p <- ggplot(sales_data,
                aes(x = Month, y = Sales, group = 1)) +
      geom_line(color = "blue", linewidth = 1.5) +
      geom_point(size = 4, color = "red") +
      labs(
        title = "Monthly Sales",
        x = "Month",
        y = "Sales ($)"
      ) +
      theme_minimal()
    
    ggplotly(p)
    
  })
  
  output$barPlot <- renderPlotly({
    
    p <- ggplot(product_data,
                aes(x = Product,
                    y = Sales,
                    fill = Product)) +
      geom_bar(stat = "identity") +
      labs(
        title = "Top Selling Products",
        x = "Products",
        y = "Annual Sales ($)"
      ) +
      theme_minimal()
    
    ggplotly(p)
    
  })
  
  output$scatterPlot <- renderPlotly({
    
    p <- ggplot(scatter_data,
                aes(x = Advertising,
                    y = Sales)) +
      geom_point(size = 4, color = "darkgreen") +
      geom_smooth(method = "lm",
                  se = FALSE,
                  color = "red") +
      labs(
        title = "Advertising Budget vs Monthly Sales",
        x = "Advertising Budget ($)",
        y = "Monthly Sales ($)"
      ) +
      theme_minimal()
    
    ggplotly(p)
    
  })
  
}

#-----------------------------
# Run App
#-----------------------------

shinyApp(ui = ui, server = server)
