#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

<<<<<<< HEAD
#add library
=======
#required libraries
>>>>>>> 49b18764b8cd22aa97c8c12b90c8af1643a1070d
library(shiny)
library(DMwR2)
library(ggplot2)
library(xts)
library(quantmod)

<<<<<<< HEAD
#add a new line

=======
#Hello Team!!!! 
>>>>>>> 49b18764b8cd22aa97c8c12b90c8af1643a1070d

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
   
  output$distPlot <- renderPlot({
    
    # generate bins based on input$bins from ui.R
    x    <- faithful[, 2] 
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    
    # draw the histogram with the specified number of bins
    hist(x, breaks = bins, col = 'darkgray', border = 'white')
    
  })
  
})
