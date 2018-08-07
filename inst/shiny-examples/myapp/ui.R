#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#
library(dygraphs)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
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

