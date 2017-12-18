#'
#'subset data and fit coefficients in each subset, for survival models
#'
#'@param formul formula
#'@param subset a FACTOR vector with same length as data
#'@param data data frame
#'@param return.pval whether to return the actual p-value or just the significant level
#'
#'@return coefficients
#'@return significance
#'@return pvalues
#'
#'@example
#'formul = as.formula(survObj ~ strata(grade) + fico_range_low+inq_last_6mths+borrowerrate)
#'coefStability.surv(formul, data$grade, data)



coefStability.surv = function(formul, subset, data, return.pval = FALSE)
{
  library(survival)
  
  m.allpop = coxph(formul, data = data)
  
  s.m = summary(m.allpop)
  varnum = nrow(s.m$coef)
  
  #memory allocation for coefficients, p-values and significance levels
  coef.matx = matrix(nrow = length(levels(subset)) + 1, ncol = varnum + 1)
  pval.matx = matrix(nrow = length(levels(subset)) + 1, ncol = varnum + 1)
  sig.matx = matrix(" ", nrow = length(levels(subset)) + 1, ncol = varnum + 1)
  
  #coefficients for model with whole population
  coef.matx[1, 1:varnum] = s.m$coef[,1] #coefficients
  pval.matx[1, 1:varnum] = s.m$coef[,5] #p values
  
  #coefficients for model in each subpopulation
  for (i in 1:length(levels(subset)))
  {
    m = coxph(formul, data = data[subset == levels(subset)[i],])
    coef.matx[i+1, 1:varnum] = summary(m)$coef[,1] #coefficients
    pval.matx[i+1, 1:varnum] = summary(m)$coef[,5] #p values
  }
  #last column shows the percentage of each sub-population
  coef.matx[, varnum + 1] = c(1, round(table(subset)/nrow(data), digit = 3))
  sig.matx[, varnum + 1] = c(1, round(table(subset)/nrow(data), digit = 3))
  
  #define significance levels
  sig.matx[pval.matx < 0.1] = "."
  sig.matx[pval.matx < 0.05] = "*"
  sig.matx[pval.matx < 0.01] = "**"
  sig.matx[pval.matx < 0.001] = "***"
  
  #naming matrices
  invisible(colnames(coef.matx, do.NULL = TRUE)); colnames(coef.matx) = c(rownames(s.m$coef), "obs.pct")
  invisible(rownames(coef.matx, do.NULL = TRUE)); rownames(coef.matx) = c("allPopulation", levels(subset))
  invisible(colnames(sig.matx, do.NULL = TRUE)); colnames(sig.matx) = c(rownames(s.m$coef), "obs.pct")
  invisible(rownames(sig.matx, do.NULL = TRUE)); rownames(sig.matx) = c("allPopulation", levels(subset))
  
  #return values
  if (return.pval == FALSE)
    return(list(coefficients = coef.matx, significance = sig.matx))
  if (return.pval == TRUE)
  {
    pval.matx[, varnum + 1] = c(1, table(subset)/nrow(data))
    invisible(colnames(pval.matx, do.NULL = TRUE)); colnames(pval.matx) = c(rownames(s.m$coef), "obs.pct")
    invisible(rownames(pval.matx, do.NULL = TRUE)); rownames(pval.matx) = c("allPopulation", levels(subset))
    return(list(coefficients = coef.matx, significance = sig.matx, pvalue = pval.matx))
  }
}


