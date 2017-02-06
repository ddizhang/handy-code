
#' initialtive for seriesAdjuster
#'
#' Install package "tso"
#'
init.sAdj = function()
{
  install.packages(tsoutliers)
} #first time user


#' prepare ts for seriesAdjuster
#'
prepare = function(names, dat, freq, olierType, colnames)
{
  library(tsoutliers)
  library(stats)
  if (length(olierType) == 0)
    olierType = rep("TC", ncol(dat))
  if (length(colnames) != 0 & sum(names %in% colnames) == 0)
    stop("Column names not found in original table!")
  if (nrow(dat) < freq*2)
    stop("Length of time series should be greater than 2 circles!")
  if (sum(sapply(dat, function(t) !is.numeric(t))))
    stop("Non-numeric column exists!")
  if (sum(sapply(dat, function(t) sum(is.na(t)))))
    stop("Missing value exists!")
  return(olierType)
}




#'seriesAdjuster0, and return a dataframe
#'
seriesAdjuster0.df = function(y, otype, freq) {
  yts = ts(y, frequency=freq)
  yadj = rmOlier(yts, types = otype, log = TRUE)
  yadj.desea = deseason(yadj$yadj, freq)
  result = data.frame(yts, yadj$yadj,
                      yadj.desea = yadj.desea[,2]+yadj.desea[,3])
  return(result)
}


