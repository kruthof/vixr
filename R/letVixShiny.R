#' Create a shiny app for the VIX data
#'
#' @param vix An object of data type VIX, containing vix qotes (i.e. output of vic_calc)
#' @import dygraphs
#' @import shiny
#' @importFrom forecast "ma"
#' @importFrom stats "as.ts"
#' @return Siny App
#' @export
#' @seealso \code{\link{vix_calc}, \link{fitVIX},\link{plotvix}}
#' @examples 
#' \dontrun{
#' vix <- data.frame('VIX' = runif(450, 9, 9.7))
#' letVixShiny(vix)
#'  }
letVixShiny = function(vix) {
  vix2shiny <<- vix

  shinyApp(ui = ui, server = server)

}
# 
# letVixShiny  <- function(vix) {
#   vix2shiny <<- vix
#   appDir <- system.file("shiny-examples","myapp", package = "vixr")
#   if (appDir == "") {
#     stop("Could not find example directory. Try re-installing `vixr`.", call. = FALSE)
#   }
#   
#   shiny::runApp(appDir, display.mode = "normal")
#   return(NULL)
# }