#______________________________________________________________________________________
#______________________________________________________________________________________
#                           VIX Replication
#                         author: Garvin Kruthof
#
#                          -   Shiny app File  -
#______________________________________________________________________________________
#______________________________________________________________________________________

require(shiny)
require(dygraphs)

ui <- shinyUI(fluidPage(

  titlePanel(""),

  sidebarLayout(
    sidebarPanel(
      sliderInput("MovingAvg", label = "Simple Moving Average Smoother",
                  value = 60, min = 2, max = 129, step = 1),

      checkboxInput("showgrid", label = "Show Grid", value = TRUE)
    ),
    mainPanel(
      dygraphOutput("dygraph")
    )
  )
))




server <- function(input, output, session) {
  predicted <- reactive({
    hw <- cbind("vix" = as.ts(vix), "ma" = ma(as.ts(vix),input$MovingAvg))

  })

  output$dygraph <- renderDygraph({
    dygraph(predicted(), main = "VIX Intraday Prices by Minutes") %>%
      dySeries("vix", label = "VIX")%>%
      dySeries("ma", label = "Moving Average",col="red")%>%
      dyRangeSelector()%>%

      dyOptions(drawGrid = input$showgrid)
  })

}

