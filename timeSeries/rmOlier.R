#' remove Outlier
#'
#' remove outliers for a given time series
#' @param y time series
#' @param types type of outliers, "TC" for temporary change, "LS" for level shift, "AO" for additive outliers
#' @param log print log or not
#' @export

rmOlier = function(y, types = "TC", log = FALSE, ...)
{
  tryC = tryCatch({
    tsoutliers::tso(y, types = types); #a$outliers
  },error=function(e) NULL)

  if ( length(tryC) == 0 )
  {
    if (log) cat("Error from tso()./n")
    return(list(yadj = y, outliers = NULL))
  }
  else
  {
    return(list(yadj = tryC$yadj, outliers = tryC$outliers))
  }
}
