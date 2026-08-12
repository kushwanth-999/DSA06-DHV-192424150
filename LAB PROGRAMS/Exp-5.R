
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
traffic <- data.frame(
  Date = as.Date(c(
    "2023-01-01",
    "2023-01-02",
    "2023-01-03",
    "2023-01-04",
    "2023-01-05"
  )),
  PageViews = c(1500, 1600, 1400, 1650, 1800),
  CTR = c(2.3, 2.7, 2.0, 2.4, 2.6)
)

# Hardcoded User Interactions
traffic$Likes <- c(120, 150, 100, 170, 180)
traffic$Shares <- c(60, 80, 55, 90, 95)
traffic$Comments <- c(25, 35, 20, 40, 45)

ui <- fluidPage(
  
  titlePanel("Website Analytics"),
  
  tabsetPanel(
    
    tabPanel("Dataset",
             tableOutput("table")),
    
    tabPanel("Line Chart",
             plotOutput("line")),
    
    tabPanel("Bar Chart",
             plotOutput("bar")),
    
    tabPanel("Stacked Area Chart",
             plotOutput("area")),
    
    tabPanel("Dashboard",
             
             fluidRow(
               
               column(6,
                      plotOutput("dashboardLine")),
               
               column(6,
                      plotOutput("dashboardBar"))
               
             )
             
    )
    
  )
  
)

server <- function(input, output){
  
  # Dataset
  output$table <- renderTable({
    traffic
  })
  
  # 1. Line Chart
  output$line <- renderPlot({
    
    plot(
      traffic$Date,
      traffic$PageViews,
      type = "o",
      col = "blue",
      pch = 16,
      lwd = 2,
      xlab = "Date",
      ylab = "Page Views",
      main = "Daily Page Views Over Time"
    )
    
  })
  
  # 2. Bar Chart (Top N CTR)
  output$bar <- renderPlot({
    
    top <- traffic[order(-traffic$CTR), ]
    
    barplot(
      top$CTR,
      names.arg = top$Date,
      col = "orange",
      main = "Top Days by Click-through Rate",
      xlab = "Date",
      ylab = "CTR (%)"
    )
    
  })
  
  # 3. Stacked Area Chart
  output$area <- renderPlot({
    
    x <- 1:nrow(traffic)
    
    y <- rbind(
      traffic$Likes,
      traffic$Shares,
      traffic$Comments
    )
    
    colors <- c("skyblue", "orange", "green")
    
    plot(
      x,
      traffic$Likes,
      type = "n",
      xlab = "Date",
      ylab = "User Interactions",
      xaxt = "n",
      main = "User Interaction Distribution"
    )
    
    axis(1,
         at = x,
         labels = traffic$Date)
    
    polygon(c(x, rev(x)),
            c(rep(0, length(x)), rev(traffic$Likes)),
            col = colors[1],
            border = NA)
    
    polygon(c(x, rev(x)),
            c(traffic$Likes,
              rev(traffic$Likes + traffic$Shares)),
            col = colors[2],
            border = NA)
    
    polygon(c(x, rev(x)),
            c(traffic$Likes + traffic$Shares,
              rev(traffic$Likes + traffic$Shares + traffic$Comments)),
            col = colors[3],
            border = NA)
    
    legend(
      "topleft",
      legend = c("Likes","Shares","Comments"),
      fill = colors
    )
    
  })
  
  # Dashboard Line Chart
  output$dashboardLine <- renderPlot({
    
    plot(
      traffic$Date,
      traffic$PageViews,
      type = "o",
      col = "blue",
      pch = 16,
      lwd = 2,
      main = "Page Views",
      xlab = "Date",
      ylab = "Views"
    )
    
  })
  
  # Dashboard Bar Chart
  output$dashboardBar <- renderPlot({
    
    top <- traffic[order(-traffic$CTR), ]
    
    barplot(
      top$CTR,
      names.arg = top$Date,
      col = "orange",
      main = "CTR",
      xlab = "Date",
      ylab = "%"
    )
    
  })
  
}

shinyApp(ui, server)
