
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
  ProductID = c(1, 2, 3, 4, 5),
  ProductName = c("Product A", "Product B", "Product C", "Product D", "Product E"),
  Quantity = c(250, 175, 300, 200, 220)
)

# Hardcoded Product Category
inventory$Category <- c("Electronics", "Electronics", "Furniture", "Furniture", "Electronics")

# Hardcoded Product Price
inventory$Price <- c(1200, 800, 1500, 1000, 1300)

ui <- fluidPage(
  
  titlePanel("Product Inventory Management"),
  
  tabsetPanel(
    
    tabPanel("Dataset",
             tableOutput("table")),
    
    tabPanel("Bar Chart",
             plotOutput("bar")),
    
    tabPanel("Stacked Bar Chart",
             plotOutput("stacked")),
    
    tabPanel("Scatter Plot",
             plotOutput("scatter")),
    
    tabPanel("Dashboard",
             
             fluidRow(
               
               column(6,
                      plotOutput("dashboardBar")),
               
               column(6,
                      plotOutput("dashboardStack"))
               
             )
             
    )
    
  )
  
)

server <- function(input, output){
  
  # Dataset
  output$table <- renderTable({
    inventory
  })
  
  # 1. Bar Chart
  output$bar <- renderPlot({
    
    barplot(
      inventory$Quantity,
      names.arg = inventory$ProductName,
      col = "skyblue",
      main = "Quantity of Products in Inventory",
      xlab = "Product Name",
      ylab = "Quantity Available"
    )
    
  })
  
  # 2. Stacked Bar Chart
  output$stacked <- renderPlot({
    
    data <- table(inventory$Category, inventory$ProductName)
    
    quantityMatrix <- data
    
    for(i in 1:ncol(quantityMatrix)){
      quantityMatrix[,i] <- 0
    }
    
    for(i in 1:nrow(inventory)){
      quantityMatrix[inventory$Category[i], inventory$ProductName[i]] <- inventory$Quantity[i]
    }
    
    barplot(
      quantityMatrix,
      col = c("orange", "green"),
      main = "Quantity by Product Category",
      xlab = "Products",
      ylab = "Quantity",
      legend = rownames(quantityMatrix)
    )
    
  })
  
  # 3. Scatter Plot
  output$scatter <- renderPlot({
    
    plot(
      inventory$Price,
      inventory$Quantity,
      pch = 19,
      col = "red",
      xlab = "Product Price",
      ylab = "Quantity Available",
      main = "Price vs Quantity Available"
    )
    
    abline(
      lm(Quantity ~ Price, data = inventory),
      col = "blue",
      lwd = 2
    )
    
  })
  
  # Dashboard Bar Chart
  output$dashboardBar <- renderPlot({
    
    barplot(
      inventory$Quantity,
      names.arg = inventory$ProductName,
      col = "skyblue",
      main = "Inventory Quantity",
      xlab = "Product",
      ylab = "Quantity"
    )
    
  })
  
  # Dashboard Stacked Bar Chart
  output$dashboardStack <- renderPlot({
    
    data <- table(inventory$Category, inventory$ProductName)
    
    quantityMatrix <- data
    
    for(i in 1:ncol(quantityMatrix)){
      quantityMatrix[,i] <- 0
    }
    
    for(i in 1:nrow(inventory)){
      quantityMatrix[inventory$Category[i], inventory$ProductName[i]] <- inventory$Quantity[i]
    }
    
    barplot(
      quantityMatrix,
      col = c("orange", "green"),
      main = "Category-wise Inventory",
      legend = rownames(quantityMatrix)
    )
    
  })
  
}

shinyApp(ui, server)
