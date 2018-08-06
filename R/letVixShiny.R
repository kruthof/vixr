#' Create a shiny app for the VIX data
#'
#' @param vix An object of data type VIX, containing vix qotes (i.e. output of vic_calc)
#' @import dygraphs
#' @import shiny
#' @importFrom forecast "ma"
#' @return Siny App
#' @export
#'@seealso \code{\link{vic_calc}}, \code{\link{convert_date}}
#' @examples letVixShiny(vix)
letVixShiny = function(vix) {
  vix2shiny <<- vix
  shinyApp(ui = ui, server = server)

}
