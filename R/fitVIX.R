#' fitVIX - Fit a basic statistical model to VIX data
#'
#' @param data A VIX object (returned by vix_calc())
#' @param fit_type The type of model required, either linear regression (\code{lm}), loess (\code{loess}), or smoothing spline (\code{smooth.spline})
#'
#' @return Returns a list of class \code{fitVIX} which includes the model details as well as fit type used
#' @export
#' @importFrom stats "lm" "loess" "smooth.spline" "na.omit" "predict"
#' @examples fitVIX(vix, fit_type = 'smooth.spline')
#' 
fitVIX  = function(data,  fit_type = c('lm', 'loess', 'smooth.spline')) 
  
  {
  x = seq(1:length(data$VIX))
  # Find what type of fitting method
  fit_arg = match.arg(fit_type)
  # Fit some models
  if(fit_arg == 'lm') {
    mod = data %$% lm(data$VIX ~ x)
  } else if(fit_arg == 'loess') {
    mod = data %$% loess(data$VIX ~ x)
  } else if(fit_arg == 'smooth.spline') {
    mod = data %$% smooth.spline(x, data$VIX)
  }
  print(mod)
  out = list(model = mod,
             data = data,
             fit_type = fit_arg)
  class(out) = 'fitVix'
  
  invisible(out)
  
}

