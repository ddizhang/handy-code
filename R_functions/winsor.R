#' remove top quantile outliers
#' @param x series
#' @param qMin top quantile to be winsorized
#' @param qMax bottom quantile to be winsorized


winsor = function(x, qMin = 0, qMax = 0.95)
{
  x[x > quantile(x, qMax, na.rm = TRUE)] = quantile(x, qMax, na.rm = TRUE)
  x[x < quantile(x, qMin, na.rm = TRUE)] = quantile(x, qMin, na.rm = TRUE)
  return(x)
}
