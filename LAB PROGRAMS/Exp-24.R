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
hotel <- data.frame(
  BookingID = c(1, 2, 3, 4, 5),
  StayNights = c(2, 5, 3, 7, 4),
  Guests = c(2, 4, 1, 3, 2),
  RoomType = c("Standard", "Deluxe", "Standard", "Suite", "Deluxe")
)

ui <- fluidPage(
  
  titlePanel("Hotel Booking Analysis Dashboard"),
  
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
      h3("Hotel Booking Dataset"),
      tableOutput("hotelTable")
    )
    
  )
  
)

server <- function(input, output){
  
  # 1. Histogram of Stay Nights
  output$histPlot <- renderPlot({
    
    hist(
      hotel$StayNights,
      col = "skyblue",
      border = "black",
      main = "Distribution of Stay Nights",
      xlab = "Stay Nights",
      ylab = "Frequency"
    )
    
  })
  
  # 2. Pie Chart of Room Types
  output$pieChart <- renderPlot({
    
    roomCount <- table(hotel$RoomType)
    
    pie(
      roomCount,
      col = c("gold", "lightgreen", "tomato"),
      main = "Room Type Distribution"
    )
    
  })
  
  # 3. Bar Chart of Guests per Booking
  output$barChart <- renderPlot({
    
    bp <- barplot(
      hotel$Guests,
      names.arg = hotel$BookingID,
      col = "orange",
      ylim = c(0, max(hotel$Guests) + 1),
      main = "Guests per Booking",
      xlab = "Booking ID",
      ylab = "Number of Guests"
    )
    
    text(
      bp,
      hotel$Guests + 0.2,
      labels = hotel$Guests
    )
    
  })
  
  # 4. Scatter Plot of Guests vs Stay Nights
  output$scatterPlot <- renderPlot({
    
    plot(
      hotel$Guests,
      hotel$StayNights,
      pch = 19,
      col = "blue",
      xlab = "Number of Guests",
      ylab = "Stay Nights",
      main = "Guests vs Stay Nights"
    )
    
    text(
      hotel$Guests,
      hotel$StayNights,
      labels = hotel$BookingID,
      pos = 3
    )
    
  })
  
  # Dataset Table
  output$hotelTable <- renderTable({
    
    hotel
    
  })
  
}

shinyApp(ui, server)
