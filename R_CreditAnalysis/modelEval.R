#' return evaluation statistics of one or more model(s)
#' 
#' ROC curve, AUC, KS, cohen kappa, pseudo R squares for glm, and decileIV which is defined as the Information Value of predicted score on the response
#' @param modelList a LIST, each element is a model object
#' @param testSet if specified, the criterions will be run on a test set. Default null, and criterions will be run on training set
#' @param respondName if testSet is specified, indicate the name of respond variable
#' @param threshold the threshold of probability for classification in logistic regression
#' @param plot.roc whether to plot the roc curves (all models in one plot)
#' @param legend.off whether to add legend automatically
#' @param rsq whether to provide Pseudo R square (TRUE only when it's GLM)
#' @example
#' inTest = sample(1:nrow(data), 5000)
#' train = data[-inTest, ]
#' test = data[inTest,]
#' m1 = glm(binreject ~ amount_requested + dti + dtisq + I(emp_length<1), data = train, family = binomial)
#' m2 = glm(binreject ~ amount_requested, data = train, family = binomial)
#' modelEval(list(m1), threshold = 0.3)
#' modelEval(list(m1, m2), testSet = test, respondName = "binreject", threshold = 0.3)


modelEval = function(modelList, testSet = NULL, respondName, threshold = 0.08, plot.roc = TRUE, 
                     legend.off = FALSE, rsq = TRUE, ...)
{
  #library(InformationValue)
  #library(pROC)
  #library(psych)
  #library(BaylorEdPsych)
  
  l = length(modelList)
  
  if (rsq)
  {
    McFaddenRsq = sapply(modelList, function(m) PseudoR2(m)[1])
    CoxSnellRsq = sapply(modelList, function(m) PseudoR2(m)[3])
  }
  
  if (!is.null(testSet))
  {
    actual = testSet[, respondName]
    roc = lapply(modelList, function(m) 
      pROC::roc(actual, predict(m, testSet, ...), algorithm = 2))
    auc = sapply(roc, function(t) t$auc[1])
    ks = sapply(modelList, function(m) 
      InformationValue::ks_stat(actual, predict(m, testSet, ...)))
    cohenKappa = lapply(modelList, function(m) 
      psych::cohen.kappa(table(predict(m, testSet, ...) > threshold, as.logical(actual)))$kappa)
    decileIV = lapply(modelList, function(m, actualv = actual) 
      Information::create_infotables(data = data.frame(predict(m, testSet, ...), actual = actualv), y = "actual")$Summary$IV)
  }
  if (is.null(testSet))
  {
    roc = lapply(modelList, function(m) pROC::roc(m$y, m$fitted.values, algorithm = 2))
    auc = sapply(roc, function(t) t$auc[1])
    ks = sapply(modelList, function(m) InformationValue::ks_stat(m$y, m$fitted.values))
    cohenKappa = lapply(modelList, function(m) 
      psych::cohen.kappa(table(m$fitted.values > median(m$fitted.values), as.logical(m$y)))$kappa)
    decileIV = lapply(modelList, function(v) 
      Information::create_infotables(data = data.frame(v$fitted.values, actual = v$y), y = "actual")$Summary$IV)
    
  }
  
  if (plot.roc)
  {
    color = rainbow(l)
    plot(roc[[1]], col = color[1])
    if(l > 1)
      for(j in 2:l) lines(roc[[j]], col = color[j])
    if (!legend.off) legend("bottomright", paste("model",1:l), lty = rep(1, l), col = color)
  }
  if(rsq) stats = rbind(auc, ks, cohenKappa, decileIV, McFaddenRsq, CoxSnellRsq)
  else stats = rbind(auc, ks, cohenKappa, decileIV)
  
  colnames(stats) = paste("model", 1:l)
  stats
}

