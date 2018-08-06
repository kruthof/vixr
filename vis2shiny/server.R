#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  predicted <- reactive({
    hw <- cbind("vix" = as.ts(vix2shiny$VIX), "ma" = ma(as.ts(vix2shiny$VIX),input$MovingAvg))
    
  })
  
  output$dygraph <- renderDygraph({
    dygraph(predicted(), main = "VIX Intraday Prices by Minutes") %>%
      dySeries("vix", label = "VIX")%>%
      dySeries("ma", label = "Moving Average",col="red")%>%
      dyRangeSelector()%>%
      
      dyOptions(drawGrid = input$showgrid)
  })
  
})