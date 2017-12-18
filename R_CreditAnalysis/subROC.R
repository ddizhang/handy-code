
#'plot ROC curve on subsets of a dataset given a model object and a subseting variable
#'
#'@param model model to be tested, survival or glm
#'@param subsetVar subset variable 
#'@param binevent response
#'@param dat dataset
#'@param ... more arguments to be passed into predict(). Remember to load the libraries needed in processing models before calling this function!
#'@example subROC(model = m3, subsetVar = "ficoCut", binevent = "bindq", dat, type = "response")
#note: if intended to test a survival mode, binevent is the binary live/death event(NOT survival object!) also, library(survival) before calling thie function.

subROC = function(model, subsetVar, binevent, dat, ...)
{
  library(pROC)
  library(InformationValue)
  
  dat[, subsetVar] = as.factor(dat[, subsetVar])
  l = length(levels(dat[,subsetVar]))
  l_nonemp = which(table(dat[, subsetVar]) != 0)
  ln = length(l_nonemp)
  if (length(l_nonemp) > 15) stop("Too many subsets!!")
  
  plot(0, 0, xlim = c(1,0), col = "white", ylim = c(0,1), main = paste("subset by", subsetVar),
       xlab = "Specificity", ylab = "Sensitivity")
  abline(a = 1, b = -1, lwd = 3, col = "grey")
  
  auc = rep(NULL, l)
  ks = rep(NULL, l)
  for(i in 1:ln)
  {
    color = rainbow(ln)
    testSet = dat[dat[, subsetVar] == levels(dat[, subsetVar])[l_nonemp[i]],]
    actual = testSet[, binevent]
    pred = predict(model, testSet, ...)
    roc = pROC::roc(actual, pred, algorithm = 2)
    lines(roc, col = color[i])
    auc[l_nonemp[i]] = roc$auc
    ks[l_nonemp[i]] = InformationValue::ks_stat(actual, pred)
  }
  legend("bottomright", levels(dat[,subsetVar])[l_nonemp], lty = rep(1, ln), col = color)
  
  stats = rbind(auc, ks)
  colnames(stats) = levels(dat[,subsetVar])
  return(list(stats = stats))
}