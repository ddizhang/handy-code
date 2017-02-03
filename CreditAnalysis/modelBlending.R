#' Find optimal blend over two models by optimizing AUC/ks
#' 
#' @param data data
#' @param m1 model object 1
#' @param m2 model object 2
#' @param respondName variable name of the response variable in data
#' @example 
#' m1 = glm(eventBinary ~ cat_fico+cat_Inq+monthly_dti_pct, data, family = "binomial")
#' m2 = glm(eventBinary ~ fico_range_low+inq_last_6mths+borrowerrate, data, family = "binomial")
#' optBlend(data, m1, m2, "eventBinary", criteria = "KS")

optBlend = function(data, m1, m2, respondName, criteria = c("AUC", "KS"))
{
  #library(pROC)
  p1 = predict(m1, data)
  p2 = predict(m2, data)
  a = sapply(seq(0, 1, 0.05), function(alpha) {
    bld = alpha * p1 + (1-alpha) * p2
    if (criteria == "AUC")
    {
      roc.bld = pROC::roc(data[,respondName], bld, algorithm = 2)
      as.numeric(auc(roc.bld))
    }
    else if (criteria == "KS")
    {
      ks = InformationValue::ks_stat(data[,respondName], bld)
      as.numeric(ks)
    }
    else return(0)
  })
  a.best = which(a == max(a))
  #c(alpha = seq(0,1,0.05)[a.best], auc = a[a.best])
  seq(0,1,0.05)[a.best]
}



#' Return the blend score on a dataset of two models given their proportion
#' 
#' @param m1 model object 1
#' @param m2 model object 2
#' @param te test set
#' @param alpha the proportion of model 1
#' 
#' @example 
#' pred = blendPred(m1, m2, te, 0.3)
#' 
blendPred = function(m1, m2, te, alpha)
  alpha*predict(m1, te) + (1-alpha)*predict(m2, te)

