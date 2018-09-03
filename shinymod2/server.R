

library(shiny)
library(quantmod)


# Define server logic required to draw a histogram
shinyServer(function(input, output) {
   
  output$distPlot <- renderPlot({
    
    price <- getSymbols('input$stockInput',from='2017-01-01')
    plot(price)
    
  })
  

  
})
