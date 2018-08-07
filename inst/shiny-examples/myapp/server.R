#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(forecast)
library(dygraphs)
shinyServer(function(input, output) {
  
  predicted <- reactive({
    if(input$addsmoother) {
      if(input$smoother=="ma") {
        vix_smoother = ma(as.ts(vix2shiny$VIX),input$MovingAvg) 
      } else if(input$smoother=="smooth.spline") {
        fit = fitVIX(vix2shiny,fit_type = "smooth.spline") 
        vix_smoother = predict(fit$model,data.frame(x=seq(1:length(vix2shiny$VIX))))
        vix_smoother = vix_smoother$y$x
      }else{  
        fit = fitVIX(vix2shiny,fit_type = input$smoother) 
        vix_smoother = predict(fit$model,data.frame(x=seq(1:length(vix2shiny$VIX))))
      }
    }
    else{vix_smoother= as.ts(vix2shiny$VIX)}
    hw <- cbind("vix" = as.ts(vix2shiny$VIX), "smooth" = vix_smoother)
    
  })
  
  output$dygraph <- renderDygraph({
    
    dygraph(predicted(), main = "VIX Intraday Prices by Minutes") %>%
      dySeries("vix", label = "VIX")%>% 
      dySeries("smooth", label = "Smoother",col="blue")%>% 
      dyRangeSelector()%>%
      dyOptions(drawGrid = input$showgrid)
    
    
    
  })
  
  
  
})



