
#' Adjust outliers, and seasonality effects for a time series
#'
#' @param y a vector of numbers
#' @param otype outlier type, one or more of "TC"(temporary change), "LS" (level shift), or "AO" (additive outliers)
#' @param freq frequency
#' @param s.window	in calling stl(): either the character string "periodic" or the span (in lags) of the loess window for seasonal extraction, which should be odd and at least 7, according to Cleveland et al. This has no default.
#' @param stype in calling deseason(): "multiplicative" or "additive", specify type of seasonal factors in de-seasonality step
#' @param log in calling rmOlier(): print log or not
#' @return yts the original series
#' @return yadj the adjusted series
#' @return outliers outliers found by the process
#' @return yadj.seasonal seasonal effect
#' @export

seriesAdjuster0 = function(y, otype, freq, swindow = "periodic", stype = "a", log = "FALSE") {
  yts = ts(y, frequency=freq)
  yadj = rmOlier(yts, types = otype, log = log)
  yadj.desea = deseason(yadj$yadj, freq, swindow = swindow, type = stype)
  result = list(yts = yts, yadj = yadj$yadj, outliers = yadj$outliers,
                yadj.seasonal = yadj.desea)
  return(result)
}

