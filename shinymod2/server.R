


library(quantmod)


# Define server logic required to draw a histogram
shinyServer(function(input, output) {
   
  output$plot <- renderPlot({
    
    dataInput <- reactive({
      getSymbols(input$stockInput, src = "yahoo",
                 from='2017-01-01', 
                 to='2017-04-30',
                 auto.assign = FALSE)
    })
    
    output$plot <- renderPlot({    
      chartSeries(dataInput(), theme = chartTheme("white"),
                  type = "line")
    })
    
  })
  

  
})
