library(shiny)

shinyUI(fluidPage(
  
  # Application title
  titlePanel("Stock Guru - Scroll down if you don't see model"),
  
  sidebarLayout(
    sidebarPanel(
      
      
      #This is a dropdown to select the stock
      
      selectInput("stockInput", 
                  "Pick your stock:", 
                  c("AMZN","FB","GOOG","NVDA","AAPL"),"AMZN"),
      
      #This is a dropdown to select the model
      
      selectInput("modelInput", 
                  "Pick your prediction model:", 
                  c("ARIMA Non-seasonal","ARIMA Seasonal","Exponential Smoothing (ETS)", "Seasonal Naive"),"ARIMA Non-seasonal")
      
    ),
    mainPanel(
       plotOutput("plot"),
       plotOutput("autoarima"),
       plotOutput("arimaseasonal"),
       plotOutput("ets"),
       plotOutput("seasonalnaive")
    )
  )
))
