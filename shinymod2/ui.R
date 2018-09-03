library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Stock Guru"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
       
       #This is a dropdown to select the stock
       selectInput("stockInput", 
                   "Pick your stock:", 
                   c("AMZN","FB","GOOG","NVDA","AAPL"),
                   "AMZN"),selected = "GOOG"),
    
    # Show a plot of the generated distribution
    mainPanel(
       plotOutput("distPlot")
    )
  )
))
