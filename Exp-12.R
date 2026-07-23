

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
inventory <- data.frame(
  ProductID = c(1, 2, 3),
  ProductName = c("Product A", "Product B", "Product C"),
  Quantity = c(250, 175, 300),
  Price = c(20, 15, 18),
  Category = c("Electronics", "Furniture", "Electronics")
)

ui <- fluidPage(
  
  titlePanel("Product Inventory Management Dashboard"),
  
  fluidRow(
    
    column(
      6,
      plotOutput("barChart", height = "350px")
    ),
    
    column(
      6,
      plotOutput("stackedBar", height = "350px")
    )
    
  ),
  
  br(),
  
  fluidRow(
    
    column(
      12,
      h3("Product Inventory Data"),
      tableOutput("inventoryTable")
    )
    
  )
  
)

server <- function(input, output){
  
  # Bar Chart
  output$barChart <- renderPlot({
    
    barplot(
      inventory$Quantity,
      names.arg = inventory$ProductName,
      col = c("skyblue", "orange", "lightgreen"),
      main = "Quantity Available for Each Product",
      xlab = "Product Name",
      ylab = "Quantity Available"
    )
    
  })
  
  # Stacked Bar Chart
  output$stackedBar <- renderPlot({
    
    categories <- unique(inventory$Category)
    products <- inventory$ProductName
    
    quantityMatrix <- matrix(
      0,
      nrow = length(categories),
      ncol = length(products)
    )
    
    rownames(quantityMatrix) <- categories
    colnames(quantityMatrix) <- products
    
    for(i in 1:nrow(inventory)){
      quantityMatrix[
        inventory$Category[i],
        inventory$ProductName[i]
      ] <- inventory$Quantity[i]
    }
    
    barplot(
      quantityMatrix,
      col = c("steelblue", "tomato"),
      main = "Quantity by Product Category",
      xlab = "Products",
      ylab = "Quantity",
      legend.text = rownames(quantityMatrix)
    )
    
  })
  
  # Inventory Table
  output$inventoryTable <- renderTable({
    
    inventory[,1:4]
    
  })
  
}

shinyApp(ui, server)
