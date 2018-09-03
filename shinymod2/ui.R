library(shiny)

shinyUI(fluidPage(
  
  # Application title
  titlePanel("Stock Guru"),
  
  sidebarLayout(
    sidebarPanel(
       
       #This is a dropdown to select the stock
       selectInput("stockInput", 
                   "Pick your stock:", 
                   c("AMZN","FB","GOOG","NVDA","AAPL"),
                   "AMZN"),selected = "GOOG"),
    

    mainPanel(
       plotOutput("plot")
    )
  )
))
