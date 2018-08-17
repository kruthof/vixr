## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ------------------------------------------------------------------------
library(vixr)

#Package includes a sample dataset
data("VIX_SampleData")

#calculating the VIX 
vix = vix_calc(VIX_SampleData,nearT=28,nextT=35, rf_near = 0.002, rf_next = 0.002)

## ------------------------------------------------------------------------
#Ploting VIX quotes
plotvix(vix, smoother="loess", type='l')

## ------------------------------------------------------------------------
#Fitting a smoothing spline model
fitVIX(vix,fit_type = "smooth.spline")

## ------------------------------------------------------------------------
#Starting Shiny app
#letVixShiny(vix)

