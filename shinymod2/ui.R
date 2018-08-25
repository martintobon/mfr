#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

# change
library(shiny)

# Define UI for application
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Stock Predictor Guru"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      #This is a slider from the original template, we will delete this //MT
       sliderInput("bins",
                   "Number of bins:",
                   min = 1,
                   max = 50,
                   value = 30),
       
       #This is a dropdown to select the stock //MT
       selectInput("stock", 
                   "Pick your stock:", 
                   c("AMZN","FB","GOOG","NVDA","AAPL"),
                   "AMZN"),
       
       #This is a dropdown to select the model //MT
       selectInput("model", 
                   "Pick your prediction model:", 
                   c("Linear Regression","Random Forest","Naive Bayes"),
                   "Linear Regression")
       
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
       plotOutput("distPlot")
    )
  )
))
