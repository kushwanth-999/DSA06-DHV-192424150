

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
customer <- data.frame(
  CustomerID = c(1, 2, 3),
  Age = c(28, 35, 42),
  Gender = c("Female", "Male", "Female"),
  Income = c(50000, 60000, 75000)
)

ui <- fluidPage(
  
  titlePanel("Customer Demographics Analysis Dashboard"),
  
  fluidRow(
    
    column(
      6,
      plotOutput("barChart", height = "350px")
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
      h3("Customer Demographic Information"),
      tableOutput("customerTable")
    )
    
  )
  
)

server <- function(input, output){
  
  # Bar Chart
  output$barChart <- renderPlot({
    
    barplot(
      customer$Age,
      names.arg = paste("Customer", customer$CustomerID),
      col = c("skyblue", "orange", "lightgreen"),
      main = "Distribution of Customer Ages",
      xlab = "Customers",
      ylab = "Age (Years)"
    )
    
  })
  
  # Pie Chart
  output$pieChart <- renderPlot({
    
    genderCount <- table(customer$Gender)
    
    pie(
      genderCount,
      labels = names(genderCount),
      col = c("pink", "lightblue"),
      main = "Customer Distribution by Gender"
    )
    
  })
  
  # Customer Table
  output$customerTable <- renderTable({
    customer
  })
  
}

shinyApp(ui, server)
