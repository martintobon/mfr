library(shiny)
library(shinydashboard)
library(DT)
library(DMwR2)
library(ggplot2)
library(xts)
library(quantmod)
library(forecast)

### Create global data store for tables
GlobalData<-NULL 

### Model Functions ##########


# NB.The functions are separated from the Shiny Code to make it cleaner

### ARIMA Function ##########

arima_function <- function(x) {
  stock<-as.character(x)
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
  
  m1 <- accuracy(model1)
  m1b <- accuracy(model1b)
  m1 <- as.data.frame(m1)
  m1b <- as.data.frame(m1b)
  m1 <- rbind(m1, m1b)
  rownames(m1)=c("Training set", "Test set")
  Data_Set=c("Training set", "Test set")
  m1=cbind(Data_Set,m1)
  
  GlobalData<<-m1 #storing m1 before returning
  return(plot(forecast1, main = paste("ARIMA Non-seasonal (next 5 days)",x), xlab = "Date", ylab = "Price"))
}

### ARIMA Seasonal Function ##########

arima_seasonal_function <- function(x) {
  stock<-as.character(x)
  stock_df<-as.data.frame(getSymbols((Symbols=stock), auto.assign=FALSE, from='2010-01-01'))
  stock_df$Open=stock_df[,1]
  stock_df$High=stock_df[,2]
  stock_df$Low=stock_df[,3]
  stock_df$Close=stock_df[,4]
  stock_df$Volume=stock_df[,5]
  stock_df$Adj=stock_df[,6]
  stock_df<-stock_df[,c(7,8,9,10,11,12)]
  
  
  train_stock <- stock_df[1:2117,]
  test_stock <- stock_df[2118:2186,]
  
  #Prepare data for ARIMA seasonal model
  train_stock$v7_MA<-ma(train_stock$Close, order=7)
  station<-ts(na.omit(train_stock$v7_MA), frequency=30)
  decomp<-stl(station, s.window="periodic")
  adjusted<-seasadj(decomp)
  
  test_stock$v7_MA<-ma(test_stock$Close, order=7)
  station1<-ts(na.omit(test_stock$v7_MA), frequency=30)
  decomp1<-stl(station1, s.window="periodic")
  adjusted1<-seasadj(decomp1)
  
  
  #Fit ARIMA seasonal model
  model2<- auto.arima(adjusted, ic="bic", seasonal=TRUE)
  model2b <- Arima(adjusted1, model=model2, seasonal=TRUE)
  forecast2<-forecast(model2b, h=5)
  
  m2 <- accuracy(model2)
  m2b <- accuracy(model2b)
  m2 <- as.data.frame(m2)
  m2b <- as.data.frame(m2b)
  m2 <- rbind(m2, m2b) 
  rownames(m2)=c("Training set", "Test set")
  Data_Set=c("Training set", "Test set")
  m2=cbind(Data_Set,m2)
  
  GlobalData<<-m2
  return(plot(forecast2, main = paste("ARIMA Seasonal (next 5 days)",x), xlab = "Date", ylab = "Price"))
  
}

### ETS Function ##########
ETS_function <- function(x) {
  stock<-as.character(x)
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
  
  train_stock <- as.data.frame(train_stock)
  test_stock <- as.data.frame(test_stock)
  
  model3<- ets(train_stock$Close, ic="bic")
  model3b <- ets(test_stock$Close, model=model3)
  forecast3<-forecast(model3b, h=5)
  
  m3 <- accuracy(model3)
  m3b <- accuracy(model3b)
  m3 <- as.data.frame(m3)
  m3b <- as.data.frame(m3b)
  m3 <- colnames(c("Train", "Test"))
  m3 <- rbind(m3, m3b)
  Data_Set=c("Training set", "Test set")
  m3=cbind(Data_Set,m3)
  GlobalData<<-m3
  return(plot(forecast3, main = paste("ETS estimate for next 5 days",x), xlab = "Date", ylab = "Price"))
  
}

### app.r #########################
ui <- dashboardPage(
  dashboardHeader(title = "Stock Guru", titleWidth = 450),
  dashboardSidebar(disable = TRUE),
  dashboardBody(
    tags$head(tags$style(HTML('/* navbar (rest of the header) */
                              .skin-blue .main-header .navbar {
                              background-color: 00868B;
                              }'))),
fluidRow(
  column(width = 12),
  selectInput("forecast", "Pick your prediction model", c("ARIMA Non-seasonal","ARIMA Seasonal","Exponential Smoothing (ETS)"),"ARIMA Non-seasonal"),
  selectInput("stockInput", "Pick your stock", c("AMZN","GE","GOOG","NVDA","AAPL"),"GOOG"),
  plotOutput("forecast_plots"),
  tableOutput("tablet")
    )
,column(width = 4) 
    )
    )
### Server #######################	

server <- function(input, output) { 
  
  ### Two reactive objects to keep track of changes in the menus ###########
  whichModel<-reactive({
    input$forecast
  })
  whichStock<-reactive({
    input$stockInput
  })
  
  
  output$forecast_plots <- renderPlot({
    
    ## Run the above functions for the correct model
    if (input$forecast == "ARIMA Non-seasonal") {
      (arima_function(input$stockInput))
    }
    else if (input$forecast == "ARIMA Seasonal") {
      (arima_seasonal_function(input$stockInput))
    }
    else if (input$forecast == "Exponential Smoothing (ETS)") {
      (ETS_function(input$stockInput))
    }
    
    observeEvent(whichModel(),
                 
                 #edit here using input$text.....................................................
                 {
                   
                   
                   if (input$forecast == "ARIMA Non-seasonal") {
                     (arima_function(input$stockInput))
                   }
                   else if (input$forecast == "ARIMA Seasonal") {
                     (arima_seasonal_function(input$stockInput))
                   }
                   else if (input$forecast == "Exponential Smoothing (ETS)") {
                     (ETS_function(input$stockInput))
                   }
                   output$tablet <- renderTable({
                     GlobalData
                   })
                   
                 }
    )
    observeEvent(whichStock(),
                 
                 #edit here using input$text.....................................................
                 {
                   
                   
                   if (input$forecast == "ARIMA Non-seasonal") {
                     (arima_function(input$stockInput))
                   }
                   else if (input$forecast == "ARIMA Seasonal") {
                     (arima_seasonal_function(input$stockInput))
                   }
                   else if (input$forecast == "Exponential Smoothing (ETS)") {
                     (ETS_function(input$stockInput))
                   }
                   output$tablet <- renderTable({
                     GlobalData
                   })
                   
                   
                 }
    )
    
    
  })
}

shinyApp(ui, server) 
### end ###########