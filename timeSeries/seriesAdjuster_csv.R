

#' Adjust outliers, and seasonality effects for a time series.
#'
#' This tool is used to automatically detect outliers and seasonality
#' of a time series which has multiple observations in a period(i.e. a year).
#' Typically, a weekly/monthly/quarterly TC, TA series.
#'
#' The input series should be longer than two circles.
#' Also, there should not be any missing values.
#'
#' The series should be placed in a column of a .csv file, in the same folder as this file.
#' Multiple series can be put in one file, and will be taken care separately by the tool,
#' as long as they have the same length and same frequency (i.e. # of observations in one circle).
#' Only the columns specified in the input will be taken care of.
#
#'
#' @param filename: name of the .csv file
#' @param colnames: name of the columns which you want to de-seasonalize
#' @param frequency: observations of a single circle.
#' @param outlierType: "TC"(Temporary Change), "LS"(Level Shift), "AO"(Additive Outlier). default to be "TC"
#'
#' @return yts the original series
#' @return yadj the adjusted series
#' @return outliers outliers found by the process
#' @return yadj.seasonal seasonal effect
#' @export
#'
#' @examples
#' seriesAdjuster.csv(filename = "example.csv",
#'                    colnames = c("col1", "col2", "col3"),
#'                    frequency = 35,
#'                    olierType = list(c("TC", "LS"), c("TC", "AO"), "TC"))
#'
#'



seriesAdjuster.csv = function(filename, colnames, frequency, olierType = NULL)
{
  dat0 = read.csv(filename, header = T)
  dat = dat0[,names(dat0) %in% colnames]
  freq = frequency
  #check if data is clean
  olierType = prepare(names(dat0), dat, frequency, olierType, colnames)

  modat0 = mapply(seriesAdjuster0.df, dat, olierType, rep(freq, length(olierType)))
  modat = data.frame(do.call('cbind',modat0[1:length(modat0)]))
  names = outer(names(dat), c("_original", "_rmOutlier","_deseason"), paste0)
  names(modat) = as.vector(t(names))
  write.csv(modat, file = "seriesAdjustedResult.csv")
}

