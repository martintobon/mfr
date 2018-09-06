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
                 from='2010-01-01', 
                 auto.assign = FALSE)
    })
    
    output$plot <- renderPlot({    
      chartSeries(dataInput(), theme = chartTheme("white"),
                  type = "line")
    })
    
    output$autoarima <- renderPlot({
      
    
      # retrieve stock data and produce ARIMA non-seasonal model estimate
      stock<-as.character(input$stockInput)
      stock_df<-as.data.frame(getSymbols((Symbols=stock), auto.assign=FALSE, from='2010-01-01'))
      stock_df$Open=stock_df[,1]
      stock_df$High=stock_df[,2]
      stock_df$Low=stock_df[,3]
      stock_df$Close=stock_df[,4]
      stock_df$Volume=stock_df[,5]
      stock_df$Adj=stock_df[,6]
      stock_df<-stock_df[,c(7,8,9,10,11,12)]
      
      stockxts <- as.xts(stock_df)
      
      train_stock <- stockxts["/2018-05-31"]
      test_stock <- stockxts["2018-06-01/"]
      
      model1<- auto.arima(train_stock$Close, ic="bic")
      model1b <- Arima(test_stock$Close, model=model1)
      forecast1<-forecast(model1b, h=5)
      
      if (input$modelInput=="ARIMA Non-seasonal"){
        return(plot(forecast1, main = "ARIMA Non-seasonal (next 5 days)",  xlab = "Date", ylab = "Price"))
      } else {
        return(NULL)
      }
      
      # Arima table
      m1 <- accuracy(model1)
      m1b <- accuracy(model1b)
      m1 <- as.data.frame(m1)
      m1b <- as.data.frame(m1b)
      m1 <- rbind(m1, m1b)
      
      output$autoarima <- renderTable({
      
        if (input$forecast == "ARIMA Non-seasonal") {
          return(t(m1))
        } else {
          return(NULL)
        }
      })  
    })
    
    #ARIMA Seasonal
    output$arimaseasonal<-renderPlot({
      
      # retrieve stock data
      stock<-as.character(input$stockInput)
      stock_df<-as.data.frame(getSymbols((Symbols=stock), auto.assign=FALSE, from='2010-01-01'))
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
      
      stockxts <- as.xts(stock_df)
      train_stock <- stockxts["/2018-05-31"]
      test_stock <- stockxts["2018-06-01/"]
      
      #Fit ARIMA seasonal model

      model2<- auto.arima(train_stock$Close, ic="bic", seasonal=TRUE)
      model2b <- Arima(test_stock$Close, model=model2, seasonal=TRUE)
      forecast2<-forecast(model2b, h=5)
      
      if (input$modelInput=="ARIMA Seasonal"){
        return(plot(forecast2, main = "ARIMA Seasonal (next 5 days)",  xlab = "Date", ylab = "Price"))
      } else {
        return(NULL)
      }
    })

    # Arima non-seasonal table
    m2 <- accuracy(model2)
    m2b <- accuracy(model2b)
    m2 <- as.data.frame(m2)
    m2b <- as.data.frame(m2b)
    m2 <- rbind(m2, m2b)
    
    output$autoarima <- renderTable({
      
      if (input$forecast == "ARIMA Seasonal") {
        return(t(m1))
      } else {
        return(NULL)
      }
    })  
  })
  
    
    #Exponential Smoothing
    output$ets<-renderPlot({
      
      # retrieve stock data
      stock<-as.character(input$stockInput)
      stock_df<-as.data.frame(getSymbols((Symbols=stock), auto.assign=FALSE, from='2010-01-01'))
      stock_df$Open=stock_df[,1]
      stock_df$High=stock_df[,2]
      stock_df$Low=stock_df[,3]
      stock_df$Close=stock_df[,4]
      stock_df$Volume=stock_df[,5]
      stock_df$Adj=stock_df[,6]
      stock_df<-stock_df[,c(7,8,9,10,11,12)]

      stockxts <- as.xts(stock_df)
      
      train_stock <- as.data.frame(stockxts["/2018-05-31"])
      test_stock <- as.data.frame(stockxts["2018-06-01/"])
            
      #Fit Exponential Smoothing model
      model3<- ets(train_stock$Close)
      model3b <- ets(test_stock$Close, model=model3)
      forecast3<-forecast(model3b, h=5)
      
      if (input$modelInput=="Exponential Smoothing (ETS)"){
        return(plot(forecast3, main = "Exponential Smoothing Forecast (Next 5 Days)",  xlab = "Date", ylab = "Price"))
      } else {
        return(NULL)
      }
  
      
      # ETS table
      m3 <- accuracy(model3)
      m3b <- accuracy(model3b)
      m3 <- as.data.frame(m3)
      m3b <- as.data.frame(m3b)
      m3 <- rbind(m3, m3b)
      
      output$autoarima <- renderTable({
        
        if (input$forecast == "Exponential Smoothing (ETS)") {
          return(t(m1))
        } else {
          return(NULL)
        }
      })  
    })    
      
    })
    
