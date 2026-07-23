
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
product <- data.frame(
  Category = c("Electronics", "Clothing", "Appliances"),
  Sales = c(50000, 35000, 40000)
)

ui <- fluidPage(
  
  titlePanel("Product Category Analysis Dashboard"),
  
  fluidRow(
    
    column(
      6,
      plotOutput("funnelChart", height = "350px")
    ),
    
    column(
      6,
      plotOutput("pieChart", height = "350px")
    )
    
  ),
  
  br(),
  
  fluidRow(
    
    column(
      12,
      h3("Product Category Sales"),
      tableOutput("salesTable")
    )
    
  )
  
)

server <- function(input, output){
  
  # Funnel Chart
  output$funnelChart <- renderPlot({
    
    plot(
      c(0,10),
      c(0,12),
      type="n",
      axes=FALSE,
      xlab="",
      ylab="",
      main="Sales Funnel by Product Category"
    )
    
    # Electronics
    polygon(
      c(2,8,7,3),
      c(10,10,8,8),
      col="skyblue"
    )
    text(5,9,"Electronics\n50000")
    
    # Appliances
    polygon(
      c(3,7,6,4),
      c(7,7,5,5),
      col="orange"
    )
    text(5,6,"Appliances\n40000")
    
    # Clothing
    polygon(
      c(4,6,5.5,4.5),
      c(4,4,2,2),
      col="lightgreen"
    )
    text(5,3,"Clothing\n35000")
    
  })
  
  # Pie Chart
  output$pieChart <- renderPlot({
    
    pie(
      product$Sales,
      labels = paste(product$Category, "\n$", product$Sales),
      col = c("skyblue","orange","lightgreen"),
      main = "Sales Distribution by Product Category"
    )
    
  })
  
  # Table
  output$salesTable <- renderTable({
    
    product
    
  })
  
}

shinyApp(ui, server)
