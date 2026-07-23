

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
library(tm)
library(wordcloud)
library(RColorBrewer)

# Hardcoded Dataset
customerID <- c(1, 2, 3, 4, 5)
age <- c(25, 30, 35, 28, 40)
satisfaction <- c(4, 5, 3, 4, 5)

customer <- data.frame(customerID, age, satisfaction)

# Hardcoded Feedback
feedback <- c(
  "Excellent service",
  "Very satisfied",
  "Good support",
  "Amazing experience",
  "Excellent quality"
)

# Create Age Groups
customer$AgeGroup <- cut(
  customer$age,
  breaks = c(20, 30, 40, 50),
  labels = c("21-30", "31-40", "41-50"),
  include.lowest = TRUE
)

ui <- fluidPage(
  
  titlePanel("Customer Feedback Analysis"),
  
  tabsetPanel(
    
    tabPanel("Dataset",
             tableOutput("table")),
    
    tabPanel("Histogram",
             plotOutput("hist")),
    
    tabPanel("Pie Chart",
             plotOutput("pie")),
    
    tabPanel("Stacked Bar",
             plotOutput("bar")),
    
    tabPanel("Word Cloud",
             plotOutput("cloud"))
    
  )
  
)

server <- function(input, output){
  
  output$table <- renderTable({
    customer
  })
  
  output$hist <- renderPlot({
    
    hist(customer$age,
         col = "skyblue",
         border = "black",
         main = "Distribution of Customer Ages",
         xlab = "Age",
         ylab = "Frequency")
    
  })
  
  output$pie <- renderPlot({
    
    counts <- table(customer$satisfaction)
    
    pie(
      counts,
      labels = paste("Score", names(counts)),
      col = rainbow(length(counts)),
      main = "Customer Satisfaction Scores"
    )
    
  })
  
  output$bar <- renderPlot({
    
    data <- table(customer$AgeGroup, customer$satisfaction)
    
    barplot(
      data,
      col = rainbow(nrow(data)),
      legend = rownames(data),
      main = "Customer Satisfaction by Age Group",
      xlab = "Satisfaction Score",
      ylab = "Count"
    )
    
  })
  
  output$cloud <- renderPlot({
    
    corpus <- Corpus(VectorSource(feedback))
    
    corpus <- tm_map(corpus, content_transformer(tolower))
    corpus <- tm_map(corpus, removePunctuation)
    corpus <- tm_map(corpus, removeWords, stopwords("english"))
    
    wordcloud(
      corpus,
      max.words = 50,
      random.order = FALSE,
      colors = brewer.pal(8, "Dark2")
    )
    
  })
  
}

shinyApp(ui, server)
