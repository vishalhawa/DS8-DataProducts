library(shiny)

source("util.R")
# This is to call Bollinger Value function
shinyServer(function(input, output) {
  
  # Expression that generates a histogram. The expression is
  # wrapped in a call to renderPlot to indicate that:
  #
  #  1) It is "reactive" and therefore should re-execute automatically
  #     when inputs change
  #  2) Its output type is a plot
   endDate = Sys.Date()
  lookback = 10

  r <- reactive(if (input$goButton > 0){  
    endDate = input$bollingerdate
    lookback = input$lookback
    startDate = endDate - 2*lookback
#     getBollingerValue(asset = c("SPY",input$assets) ,startDate = startDate,endDate=endDate,lookback)

  getMultipleBollingerValues(asset = c("SPY",input$assets) ,endDate=endDate,lookback,idxrange=50)

    })
  

  output$distPlot <- renderPlot({
 
 
    if(input$goButton==0) {
     df = getBollingerValue(asset = c("SPY","AAPL") ,startDate = Sys.Date(),endDate=Sys.Date(),lookback=20)
     barplot(height=as.matrix(df),  col = 'skyblue', border = 'white')
    }
    else{
      df = t(as.data.frame(r()))
#       print(df)
      plot(df)
#     # draw the barplot with the specified number of assets
#     barplot(height=as.matrix(r()),  col = c("blue","purple"), border = 'white')
#     
#     # set up the plot 
#     plot(xrange, yrange, type="n", xlab="Age (days)",
#          ylab="Circumference (mm)" ) 
#     colors <- rainbow(10) 
#    lines(tree$age, tree$circumference, type="b", lwd=1.5,
#              col=colors[1], pch=plotchar[1]) 
#      
    
    }
  })

  output$bollingerstatement <- renderText({ 
    paste("Scatter plot of Bollinger Values at Date of: ",input$bollingerdate," of SPY with Stock/Asset: ",input$assets)
 
    
  })
  
  output$bvalue <- renderPrint({
    if(input$goButton>0) {
      paste("Correlation between SPY and ",input$assets,"is: ", cor(t(as.data.frame(r())))[1,2] )
    }
   })
  
  output$assets <- renderUI({
      asset.vec = read.csv("sp5002012.csv")
       
      selectInput("assets", "Choose Assets",  choices = (asset.vec$stock))
  })
  
  })