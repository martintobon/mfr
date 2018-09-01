

library(shiny)
library(DMwR2)
library(ggplot2)
library(xts)
library(quantmod)


# Define server logic required to draw a histogram
shinyServer(function(input, output) {
   
  output$distPlot <- renderPlot({
    
    #only run when button is clicked
    if (input$goButton == 0)
      return()
    
    # generate bins based on input$bins from ui.R
    #x    <- faithful[, 2] 
    #bins <- seq(min(x), max(x), length.out = input$bins + 1)
    
    # draw the histogram with the specified number of bins
    #hist(x, breaks = bins, col = 'darkgray', border = 'white')
    
    #get stock prices
    price <- getSymbols('input$stockInput',from='2017-01-01')
    #close <- price$input$stockInput.Close
    plot(price)
    
  })
  
  
  
})
