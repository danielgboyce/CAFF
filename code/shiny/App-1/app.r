##install.packages('shiny')
library(shiny)
##runExample("01_hello")


# Define UI for app that draws a histogram ----
ui <- fluidPage(

  # App title ----
  titlePanel("Hello Dan!"),

  # Sidebar layout with input and output definitions ----
  sidebarLayout(

    # Sidebar panel for inputs ----
    sidebarPanel(

      selectInput("stockpicker",label="Stock")
    ),

    # Main panel for displaying outputs ----
    mainPanel(

      # Output: Histogram ----
      plotOutput(outputId = "distPlot")

    )
  )
)

# Define server logic required to draw a histogram ----
server <- function(input, output) {

df<-reactive({
  df<-subset(data,stock %in% input$stock)
})
# Histogram of the Old Faithful Geyser Data ----
  # with requested number of bins
  # This expression that generates a histogram is wrapped in a call
  # to renderPlot to indicate that:
  #
  # 1. It is "reactive" and therefore should be automatically
  #    re-executed when inputs (input$bins) change
  # 2. Its output type is a plot
  output$distPlot <- renderPlot({

  df<-subset(data,stock %in% input$stock)
  
  
##    x    <- faithful$waiting
##    bins <- seq(min(x), max(x), length.out = input$bins + 1)

##    hist(x, breaks = bins, col = "#75AADB", border = "orange",
##         xlab = "Waiting time to next eruption (in mins)",
##         main = "Histogram of waiting times")

    })

}

# Create Shiny app ----
shinyApp(ui = ui, server = server)