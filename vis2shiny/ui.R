#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(dygraphs)
library(forecast)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
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