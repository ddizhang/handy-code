
#' De-seasonality a ts object
#'
#' call R internal function 'stl' to deseasonize the series.
#' When type = "a", 'stl' is applied normally (additively).
#' When type = "m", natral logarism is applied to original series to get multiplicative seasonal indexs
#' when series has exacctly 2 periods, one additional obs will be generated to ensure computation then deleted.
#'
#' @param y a ts object
#' @param freq frequency
#' @param s.window	in calling stl(): either the character string "periodic" or the span (in lags) of the loess window for seasonal extraction, which should be odd and at least 7, according to Cleveland et al. This has no default.
#' @param type "multiplicative" or "additive"
#' @return y.desea deseasonalized series
#' @return sea.factor seasonal factors
#' @export
#'
deseason = function(y,  swindow = "periodic", type = "a", ...)
{
  if (!"ts" %in% class(y)) stop("In deseason: Input should be a ts object!")
  if (!type %in% c("a", "m"))
    stop("wrong 'type' parameter!")

  freq = frequency(y)
  start = start(y)

  if (length(y) < 2*freq)
  {
    warning("Deseasonalization not done: Length of time series should be no less than 2 periods!")
    return(y)
  }
  makeup = 0
  if (length(y) == 2*freq)
  {
    y = ts(c(y, mean(y[1], y[1+freq], y[2*freq])),frequency=freq)
    makeup = 1
  }

  if (type == "a")
  {
    sea = stl(y, s.window = swindow, ...)
    y.desea = sea$time.series[,2]+sea$time.series[,3]
    sea.factor = sea$time.series[,1]
  }
  else if (type == "m")
  {
    sea = stl(log(y), s.window = swindow, ...)
    y.desea = exp(sea$time.series[,2]+sea$time.series[,3])
    sea.factor = exp(sea$time.series[,1])
  }

  if (makeup == 1)
  {
    y.desea = ts(y.desea[1:(2*freq)], frequency=freq, start = start)
    sea.factor = ts(sea.factor[1:(2*freq)], frequency=freq, start = start)
    
  }


  return(list(y.desea = y.desea, sea.factor = sea.factor))
}
