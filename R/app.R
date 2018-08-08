#______________________________________________________________________________________
#______________________________________________________________________________________
#                           VIX Replication
#                         author: Garvin Kruthof
#
#                          -   Shiny app File  -
#______________________________________________________________________________________
#______________________________________________________________________________________


ui <- shinyUI(fluidPage(

  titlePanel(""),

  sidebarLayout(
    sidebarPanel(
      checkboxInput(inputId = "showgrid", label = "Show Grid", value = TRUE),
      checkboxInput(inputId = "addsmoother", label = "Add Smoother", value = FALSE),

      conditionalPanel(condition = ("input.addsmoother==true"),
      selectInput("smoother",
                  "Smoother type",
                  choices = list("Linear regression" = "lm",
                                 "Loess smooth" = "loess",
                                 "Spline smooth" = "smooth.spline",
                                 "Moving Average" = "ma"),
                  selected = "loess")),       
      conditionalPanel(condition = ("input.addsmoother==true && input.smoother=='ma'"), sliderInput("MovingAvg", label = "Simple Moving Average Smoother",
                                                                                                                                           value = 60, min = 2, max = 129, step = 1))
      

    ),
    mainPanel(
      dygraphOutput("dygraph")
    )
  )
))




server <- function(input, output, session) {
  

    predicted <- reactive({
    if(input$addsmoother) {
      if(input$smoother=="ma") {
        vix_smoother = ma(as.ts(vix2shiny_$VIX),input$MovingAvg) 
        } else if(input$smoother=="smooth.spline") {
          fit = fitVIX(vix2shiny_,fit_type = "smooth.spline") 
          vix_smoother = predict(fit$model,data.frame(x=seq(1:length(vix2shiny_$VIX))))
          vix_smoother = vix_smoother$y$x
          }else{  
      fit = fitVIX(vix2shiny_,fit_type = input$smoother) 
      vix_smoother = predict(fit$model,data.frame(x=seq(1:length(vix2shiny_$VIX))))
      }
      }
    else{vix_smoother= as.ts(vix2shiny_$VIX)}
    hw <- cbind("vix" = as.ts(vix2shiny_$VIX), "smooth" = vix_smoother)
    
  })

  output$dygraph <- renderDygraph({

    dygraph(predicted(), main = "VIX Intraday Prices by Minutes") %>%
      dySeries("vix", label = "VIX")%>% 
      dySeries("smooth", label = "Smoother",col="blue")%>% 
      dyRangeSelector()%>%
      dyOptions(drawGrid = input$showgrid)

    

  })
  }



