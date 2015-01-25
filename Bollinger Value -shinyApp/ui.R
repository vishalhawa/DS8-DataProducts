library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(

  # Application title
  titlePanel("Shiny App: Correration and Scatter Plot between Bollinger Values"),
  
  # Sidebar with a slider input for the number of bins
  sidebarLayout(
    sidebarPanel(
      sliderInput("lookback",   "Lookback Days:",   min = 1, max = 50, value = 30),
#       dateRangeInput(inputId = "calendar",label = "Date Ranges (to fetch) for Bollinger Values"),
#       numericInput(inputId = "lookback",label = "Lookback Days:",value=20),
      dateInput(inputId = "bollingerdate",label = "Date of Bollinger Value",max=Sys.Date()),
         uiOutput("assets"),
      actionButton("goButton", "Go!")
      ),
#      actionButton("goButton", "Go!"),
    
    # Show a plot of the generated distribution
    mainPanel(
      h4(textOutput("bollingerstatement"),style = "color:blue"),
      plotOutput("distPlot"),
   
      em(textOutput("datestatement")),
      strong(textOutput("stockselect")),
      strong(textOutput("bvalue"))
    )
)
))