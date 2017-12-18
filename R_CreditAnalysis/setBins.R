#’ Take in a dataframe, binning variable with specified method, either by quantiles, breaks, or find optimal break given a response
#'
#' @param dataset the original dataset 
#' @param varNames the CONTINUOUS variable names that you'd like to bin, defaulting to all variables
#' @param method "optim":find the optimized cuts that results in best separation for binevent(which should be specified as argument); "quantile": cut the variable based on quantiles
#' @param binevent specify if method = "optim". 
#' @param binType specify if method = "quantile". either "nBins" or "binVec". Please see function binBreak()
#' @param nBins specify if method = "quantile". Please see function binBreak()
#' @param binVec specify if method = "quantile". Please see function binBreak()
#' @example 
#' test = setBins(data, varNames = c("fico_range_high", "inq_last_6mths"), method = "optim", binevent = "intBinary")
#' test = setBins(data, varNames = c("fico_range_high", "inq_last_6mths"), method = "quantile", binType = "nBins", nBins = 4, newVarNameAppx = "yoohoo")
#' test = setBins(data[, c("fico_range_high", "inq_last_6mths", "annual_inc", "monthly_dti_pct")], method = "quantile", binType = "nBins", nBins = 4, newVarNameAppx = "yoohoo")

setBins = function(dataset, varNames = colnames(dataset), method = c("optim", "quantile"), 
                  binevent, binType = c("nBins", "binVec"), nBins = 10, binVec = c(0, 0.3, 0.7, 1), newVarNameAppx = "bin")
{
  if (is.null(newVarNameAppx))
    newVarNames = varNames
  else newVarNames = paste0(varNames, "_", newVarNameAppx)
  
  if (method == "quantile")
  {
    data = data.frame(dataset[, varNames])
    res = data.frame(sapply(data, function(t) binBreak(t, type = binType, nBins = nBins, binVec = binVec)))
  }
  
  else if (method == "optim")
  {
    library(smbinning)
    varNames = varNames[varNames != binevent] 
    smb = lapply(varNames, function(t) unique(smbinning(df = dataset, y = binevent, x = t)$bands))
    res = data.frame(mapply(cut, dataset[,varNames], smb, include.lowest = TRUE))
  }
  else (stop("Invalid method!"))
  colnames(res) = newVarNames
  res
}

