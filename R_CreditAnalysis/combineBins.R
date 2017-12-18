#'cut a continuous variable into bins and combine those with similar survival curves
#'
#'use logrank test to find cutpoints of a continuous variable that significantly seperate the survival curves
#'
#'@param var the continuous variable to bin
#'@param survObj survival object
#'@param type the method to determine bins to start from. Either nBins(Number of Bins based on fixed quantiles), or binVec(a custom vector of quantiles)
#'@param nBins if type = nBins, specify the number of bins here
#'@param binVec if type = binVec, specify the quantiles here
#'@param alpha significant
#'@value groups a vector indicating what group each observations are in
#'@example
#var = data$notional
#survObj = data$survObj
#g.no = combineBins(data$notional, data$survObj, alpha = 0.001)
#source("../functions/ggsurv.R")
#ggsurv(survfit(survObj ~ g.no))



combineBins = function(var, survObj, data, type = "nBins", nBins = 10,  binVec, alpha = 0.01)
{
  library(survival)
  library(multcomp)
  
  #break the continuous variable into several bins
  groups = binBreak(var, type = type, nBins = nBins, binVec = binVec)
  
  while (length(levels(groups)) > 1)
  {
    fit <- coxph(survObj ~ groups)
    
    #pairwise logrank test
    pw = glht(fit, linfct = mcp(groups = "Tukey"))
    pws = summary(pw)
    
    #get the pairwise logrank test p-value and store in a lower triangular matrix
    pval = matrix(0, ncol = length(levels(groups)), nrow = length(levels(groups)))
    pval[lower.tri(pval)] = pws$test$pvalues
    
    #sort the p-values and return their corresponding coordinates in the matrix
    odv = order(pval, decreasing = TRUE)
    od = data.frame(row = odv%%nrow(pval), col = (odv-1)%/%nrow(pval)+1)
    od$row[od$row == 0] = nrow(pval)   #nBins should be less than 20!!!
    
    #find the closest 2 groups and combine them
    #If those two groups are already significantly different from each other, break.
    rm.ind = od[min(which(od$row - od$col == 1)),]
    if(pval[rm.ind$row, rm.ind$col] < alpha) break
    groups[groups == levels(groups)[rm.ind$col]] = levels(groups)[rm.ind$row]
    groups = as.factor(as.integer(groups))
  }
  
  groups = as.factor(as.integer(groups))
  levels(groups) = 
    paste0("[", tapply(var, groups, min),",",tapply(var, groups, max)+1, ")")
  return(groups)
}

