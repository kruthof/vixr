#' Create a shiny app for the VIX data
#'
#' @param vix Vector containing vix qotes (i.e. output of vic_calc)
#' @import dygraphs
#' @import shiny
#' @return Siny App
#' @export
#'@seealso \code{\link{vic_calc}},\code{\link{convert_date}}
#' @examples letVixShiny(vix)
letVixShiny = function(vix) {

  shinyApp(ui = ui, server = server)

}
