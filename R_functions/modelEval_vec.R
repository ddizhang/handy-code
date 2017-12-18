#' return evaluation statistics of one or more prediction vectors
#' 
#' ROC curve, AUC, KS, cohen kappa, pseudo R squares for glm, and decileIV which is defined as the Information Value of predicted score on the response
#' The difference between it and modelEval() is merely the input
#' 
#' @param predVecList a LIST, each element is a vector of predictions
#' @param actualVec vector of actual values
#' @param threshold the threshold of probability for classification in logistic regression
#' @param plot.roc whether to plot the roc curves (all models in one plot)
#' @param legend.off whether to add legend automatically
#' @param addOn if true, add the ROC curve on current plot 

modelEval.vec = function(predVecList, actualVec, threshold = 0.08, plot.roc = TRUE, legend.off = FALSE, 
                         addOn = FALSE)
{
  roc = lapply(predVecList, function(v) 
    pROC::roc(actualVec, v, algorithm = 2))
  auc = sapply(roc, function(t) t$auc[1])
  ks = sapply(predVecList, function(v) 
    InformationValue::ks_stat(actualVec, v))
  cohenKappa = lapply(predVecList, function(v) 
    psych::cohen.kappa(table(v > threshold, as.logical(actualVec)))$kappa)
  decileIV = lapply(predVecList, function(v, actual = actualVec) 
    Information::create_infotables(data = data.frame(v = v, actual = actual), y = "actual")$Summary$IV)
  
  l = length(predVecList)
  
  if (plot.roc)
  {
    color = rainbow(l)
    if (!addOn)
    {
      plot(roc[[1]], col = color[1])
      if(l > 1)
        for(j in 2:l) lines(roc[[j]], col = color[j])
      if (!legend.off) legend("bottomright", paste("model",1:l), lty = rep(1, l), col = color)
    }
    else 
    {
      for(j in 1:l) lines(roc[[j]], col = color[j], lty = 2)
      if (!legend.off) legend("topleft", paste("model",1:l), lty = rep(2, l), col = color)
    }
  }
  stats = rbind(auc, ks, cohenKappa, decileIV)
  colnames(stats) = paste("model", 1:l)
  stats
}

