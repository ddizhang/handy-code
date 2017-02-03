
#' Break a continuous variable into several bins
#' 
#' @param var the continuous variable
#' @param type either nBins(Number of Bins based on fixed quantiles), or binVec(a custom vector of quantiles), or breaks(directly set break values)
#' @param nBins if type = nBins, specify the number of bins here
#' @param binVec if type = binVec, specify the quantiles here
#' @param breaks if type = breaks, specify the break values here
#' @example 
#' a = binBreak(data$annual_inc, type = "nBins", nBins = 5)
#' a = binBreak(data$annual_inc, type = "binVec", binVec = c(0, 0.5, 1))
#' a = binBreak(data$annual_inc, type = "breaks", breaks = c(0,1000,20000,30000, 1000000))

binBreak = function(var, type = c("nBins", "binVec", "breaks"), nBins = 10, binVec = c(0, 0.3, 0.7, 1), breaks, labs = c("midPoint", "numeric", "interval"))
{
  if (type == "nBins")
    breaks = quantile(var, probs = seq(0, 1, length.out = nBins + 1), na.rm = TRUE)
  else if (type == "binVec")
    breaks = quantile(var, probs = binVec, na.rm = TRUE)
  else if (type != "breaks") stop("invalid type")
  
  breaks = unique(breaks)
  if (labs == "midPoint") lab = RcppRoll::roll_mean(breaks, n = 2)  #label: mid point of a group interval
  if (labs == "numeric") lab = 1:(length(breaks)-1)
  if (labs == "interval") lab = NULL
  groups = cut(var, breaks = breaks, include.lowest = TRUE, labels = lab)
  return(groups)
}
