#' fitVIX - Fit a basic statistical model to VIX data
#'
#' @param Data A VIX object (returned by vix_calc())
#' @param fit_type The type of model required, either linear regression (\code{lm}), loess (\code{loess}), or smoothing spline (\code{smooth.spline})
#'
#' @return Returns a list of class fitVIX which includes the model details as well as fit type used
#' @export
#' @importFrom stats "lm" "loess" "smooth.spline" "na.omit" "predict"
#' @seealso \code{\link{letVixShiny}, \link{vix_calc}}
#' @examples SampleData <- data.frame('VIX' = runif(450, 9, 9.7))
#' fitVIX(SampleData, fit_type = 'smooth.spline')
#' 
fitVIX  = function(Data,  fit_type = c('lm', 'loess', 'smooth.spline')) {
  x = seq(1:length(Data$VIX))
  # Find what type of fitting method
  #fit_arg = match.arg(fit_type)
  fit_arg=fit_type
  # Fit some models
  if(fit_arg == 'lm') {
    mod = lm(Data$VIX ~ x)
  } else if(fit_arg == 'loess') {
    mod = loess(Data$VIX ~ x)
  } else if(fit_arg == 'smooth.spline') {
    mod = smooth.spline(x, Data$VIX)
  }
  print(mod)
  out = list(model = mod,
             Data = Data,
             fit_type = fit_arg)

  
}

