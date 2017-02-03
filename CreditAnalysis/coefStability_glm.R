#'
#'subset data and fit coefficients in each subset, for glm models
#'
#'@param formul formula
#'@param subset a FACTOR vector with same length as data. Note: there can't be any empty level or level with too few observations to build model on.
#'@param data data frame
#'@param bineven the response
#'@param return.pval whether to return the actual p-value or just the significant level
#'
#'@return coefficients
#'@return significance
#'@return pvalues
#'
#'@example 
#'formul = as.formula(eventBinary ~ fico_range_low+inq_last_6mths+borrowerrate) #2
#'coefStability.glm(formul, data$grade, data, binevent = "eventBinary")
#'
#'

coefStability.glm = function(formul, subset, data, binevent, plotROC = TRUE, return.pval = FALSE, ...)
{
  m.allpop = glm(formul, data = data, family = "binomial")
  
  s.m = summary(m.allpop)
  varnum = nrow(s.m$coef)
  
  #plot whole population ROC
  if (plotROC)
  {
    library(pROC)
    plot(0, 0, xlim = c(1,0), col = "white", ylim = c(0,1),
         xlab = "Specificity", ylab = "Sensitivity")
    abline(a = 1, b = -1, lwd = 3, col = "grey")
    roc = roc(data[, binevent], predict(m.allpop, data, type = "response"), algorithm = 2)
    lines(roc, col = "grey", lty = 2)
  }
 
  #memory allocation for coefficients, p-values and significance levels
  coef.matx = matrix(nrow = length(levels(subset)) + 1, ncol = varnum + 1)
  pval.matx = matrix(nrow = length(levels(subset)) + 1, ncol = varnum + 1)
  sig.matx = matrix(" ", nrow = length(levels(subset)) + 1, ncol = varnum + 1)
  
  #coefficients for model with whole population
  coef.matx[1, 1:varnum] = s.m$coef[,1] #coefficients
  pval.matx[1, 1:varnum] = s.m$coef[,4] #p values
  
  #coefficients for model in each subpopulation
  for (i in 1:length(levels(subset)))
  {
    #m = coxph(formul, data = data[subset == levels(subset)[i],])
    m = glm(formul, data = data[subset == levels(subset)[i],], family = "binomial")
    coef.matx[i+1, 1:varnum] = summary(m)$coef[,1] #coefficients
    pval.matx[i+1, 1:varnum] = summary(m)$coef[,4] #p values
    
    #add ROC curve of each subset model
    if(plotROC)
    {
      color = rainbow(length(levels(subset)))
      actual = data[subset == levels(subset)[i], binevent]
      pred = predict(m, data[subset == levels(subset)[i],], ...)
      roc = pROC::roc(actual, pred, algorithm = 2)
      lines(roc, col = color[i])
    }
  }
  if(plotROC) legend("bottomright", levels(subset), lty = rep(1, length(levels(subset))), col = color)
  
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


