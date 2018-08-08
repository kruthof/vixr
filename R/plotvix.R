#' Plot vix_calc output
#'
#' @param vix Output from \code{\link{vix_calc}}
#' @param smoother eg. "lm", "glm", "gam", "loess". FALSE for no smoother (default)
#' @param type "l" for line, "p" for points
#'
#' @return Nothing: just a nice plot
#' @seealso \code{\link{vix_calc}, \link{fitVIX}, \link{letVixShiny}}
#' @export
#' @import ggplot2
#'
#' @examples
#' vix <- data.frame('VIX' = runif(450, 9, 9.7))
#' plotvix(vix, smoother="loess",type="p")

plotvix = function(vix,smoother=FALSE, type="l") {

  x = seq(1:length(vix$VIX))
  tmp = data.frame(x=x,VIX = vix$VIX)
  

  # Finally create the plot
 tt <-  ggplot(tmp, aes(x = x, y = VIX)) +
    theme_bw() +
    xlab('Minutes') +
    ylab('VIX') +
    ggtitle('Volatility Index (VIX)' )
 
 if(type=="p"){
   
   tt <- tt + geom_point(position = 'jitter')
 } else {tt <- tt +geom_line() }
 
 if (smoother != FALSE){  
   tt+   stat_smooth(method = smoother)
 }else {tt}
 

}
