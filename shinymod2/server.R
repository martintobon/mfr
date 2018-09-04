library(shiny)
library(DMwR2)
library(ggplot2)
library(xts)
library(quantmod)
library(forecast)


# Define server logic required to draw a histogram
shinyServer(function(input, output) {
   
  output$plot <- renderPlot({
    
    dataInput <- reactive({
      getSymbols(input$stockInput, src = "yahoo",
                 from='2018-01-01', 
                 auto.assign = FALSE)
    })
    
    output$plot <- renderPlot({    
      chartSeries(dataInput(), theme = chartTheme("white"),
                  type = "line")
    })
    
    output$autoarima <- renderPlot({
      
      
      
      # retrieve stock data and produce ARIMA non-seasonal model estimate
      stock<-as.character(input$stockInput)
      stock_df<-as.data.frame(getSymbols((Symbols=stock), auto.assign=FALSE, from='2018-01-01'))
      stock_df$Open=stock_df[,1]
      stock_df$High=stock_df[,2]
      stock_df$Low=stock_df[,3]
      stock_df$Close=stock_df[,4]
      stock_df$Volume=stock_df[,5]
      stock_df$Adj=stock_df[,6]
      stock_df<-stock_df[,c(7,8,9,10,11,12)]
      
      model1<-auto.arima(stock_df$Close, ic="bic")
      forecast1<-forecast(model1, h=5)
      
      if (input$modelInput=="ARIMA Non-seasonal"){
        return(plot(forecast1))
      } else {
        return(NULL)
      }
      
      
      
    })
    
    #ARIMA Seasonal
    output$arimaseasonal<-renderPlot({
      
      # retrieve stock data
      stock<-as.character(input$stockInput)
      stock_df<-as.data.frame(getSymbols((Symbols=stock), auto.assign=FALSE, from='2018-01-01'))
      stock_df$Open=stock_df[,1]
      stock_df$High=stock_df[,2]
      stock_df$Low=stock_df[,3]
      stock_df$Close=stock_df[,4]
      stock_df$Volume=stock_df[,5]
      stock_df$Adj=stock_df[,6]
      stock_df<-stock_df[,c(7,8,9,10,11,12)]
      
      #Prepare data for ARIMA seasonal model
      stock_df$v7_MA<-ma(stock_df$Close, order=7)
      station<-ts(na.omit(stock_df$v7_MA), frequency=30)
      decomp<-stl(station, s.window="periodic")
      adjusted<-seasadj(decomp)
      
      #Fit ARIMA seasonal model
      model2<-auto.arima(adjusted, seasonal=TRUE)
      forecast2<-forecast(model2, h=5)
      
      if (input$modelInput=="ARIMA Seasonal"){
        return(plot(forecast2))
      } else {
        return(NULL)
      }
    })
    
    #Exponential Smoothing
    output$ets<-renderPlot({
      
      # retrieve stock data
      stock<-as.character(input$stockInput)
      stock_df<-as.data.frame(getSymbols((Symbols=stock), auto.assign=FALSE, from='2018-01-01'))
      stock_df$Open=stock_df[,1]
      stock_df$High=stock_df[,2]
      stock_df$Low=stock_df[,3]
      stock_df$Close=stock_df[,4]
      stock_df$Volume=stock_df[,5]
      stock_df$Adj=stock_df[,6]
      stock_df<-stock_df[,c(7,8,9,10,11,12)]
      
      #Fit Exponential Smoothing model
      model3<-ets(stock_df$Close)
      forecast3<-forecast(model3, h=5)
      
      if (input$modelInput=="Exponential Smoothing (ETS)"){
        return(plot(forecast3))
      } else {
        return(NULL)
      }
      
      
    })
    
      #seasonal naive
      output$seasonalnaive<-renderPlot({
       
      # retrieve stock data
      stock<-as.character(input$stockInput)
      stock_df<-as.data.frame(getSymbols((Symbols=stock), auto.assign=FALSE, from='2018-01-01'))
      stock_df$Open=stock_df[,1]
      stock_df$High=stock_df[,2]
      stock_df$Low=stock_df[,3]
      stock_df$Close=stock_df[,4]
      stock_df$Volume=stock_df[,5]
      stock_df$Adj=stock_df[,6]
      stock_df<-stock_df[,c(7,8,9,10,11,12)]
      
      #Random Walk model
      model4<-snaive(stock_df$Close, h=5)
      forecast4<-forecast(model4, h=5)
      
      if (input$modelInput=="Seasonal Naive"){
        return(plot(forecast4))
      } else {
        return(NULL)
      }
      
      
    })
    
  })
  
  
})
