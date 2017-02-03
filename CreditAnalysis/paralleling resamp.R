#' Resample in a dataset, fitting survival model and conduct out-of-sample prediction on each replication
#' 
#' Only on survival models.
#' Take in a dataset first. In each replication, randomly seperate the dataset into training and test set given a proportion. 
#' Fit model on training sets and predict on test set. 
#' Return the coefficient estimate and p value, also mean event-percentage and time-to-event at different predicting score levels
#' @param formul model formula
#' @param data dataset
#' @param rep number of replications
#' @param quantil the prediction score levels where the mean event-percentage and time-to-event of testset is calculated on
#' @param te.pc proportion of original records used as test set
#' @return coef coefficient estimates in each replication
#' @return pval p value of each predictor in each replication
#' @return co.pct mean event percentage at each score quantile
#' @return mob.co time-to-event at each score quantile


resamp.surv = function(formul, data, binevent, timeToEvent, rep, quantil = c(0, 0.2, 0.4, 0.6, 0.8, 1), te.pc = 0.2)
{
  library(survival)
  library(parallel)
  cl = makeCluster(4, "FORK") 
  
  samp = replicate(rep, sample(1:nrow(data), round(te.pc*nrow(data))))
  #rep.list = lapply(1:rep, resamp0.surv,  data = data, samp = samp, formul = formul, quantil = quantil)
  rep.list = clusterApply(cl, 1:rep, resamp0.surv, data = data, 
                          samp = samp, formul = formul, quantil = quantil, binevent = binevent, timeToEvent = timeToEvent)
  
  coef.matx = t(sapply(rep.list, function(t) t$coef))
  pval.matx = t(sapply(rep.list, function(t) t$pval))
  co.pct = t(sapply(rep.list, function(t) t$co.pct))
  mob.co = t(sapply(rep.list, function(t) t$mob.co))
  
  res = list(formula = formul, coef = coef.matx, pval = pval.matx, 
             co.pct = co.pct, mob.co = mob.co)
  class(res) = c("resamp", class(res))
  return(res)
}


#' helper function for resamp.surv()
resamp0.surv = function(i, data, samp, formul, quantil, binevent, timeToEvent)
{
  #training set and test set
  te.n = samp[,i]
  train = data[-te.n,]
  test = data[te.n,]
  
  #modeling
  m = coxph(formul, data = train, model = TRUE)
  
  #save coefficients and p-values
  coef = summary(m)$coef[,1]
  pval = summary(m)$coef[,5] 
  
  ### hazard estimate for each observation
  test$monjaRisk = exp(predict(m, test))
  test$monjaRank = cut(test$monjaRisk, 
                       breaks = quantile(test$monjaRisk, probs = quantil, na.rm = TRUE),
                       labels = quantil[-1])
  
  ### is charged off
  co.pct = tapply(test[,binevent], test$monjaRank, mean)
  
  ### number of payments before charging off
  mob.co = tapply(test[test[,binevent] == 1, timeToEvent], test$monjaRank[test[,binevent] == 1], mean)
  
  return(list(coef = coef, pval = pval, co.pct = co.pct, mob.co = mob.co))
}



#' Summary of a "resamp" object, typically an outcome of resamp.surv()
#' 
#' Return the mean, standard deviation, 5%, 50%, 95% quantiles of each statistic in the output of resamp.surv()
#' @param resamp a resamp object, which is the outcome of resamp.surv()

summary.resamp = function(resamp)
{
  coef.summary = rbind(apply(resamp$coef, 2, mean), apply(resamp$coef, 2, sd), 
                       apply(resamp$coef, 2, quantile, c(0.05,0.5, 0.95)),
                       apply(resamp$pval, 2, function(t) mean(t > 0.1)), 
                       apply(resamp$pval, 2, function(t) mean(t > 0.05)), 
                       apply(resamp$pval, 2, function(t) mean(t > 0.01)))
  rownames(coef.summary) = c("mean", "se", "5% QT", "50% QT", "95% QT", "% p>0.1", "% p>0.05", "%p>0.01")
  co.pct.summary = rbind(apply(resamp$co.pct, 2, mean), apply(resamp$co.pct, 2, sd),
                         apply(resamp$co.pct, 2, quantile, c(0.05, 0.5, 0.95)))
  rownames(co.pct.summary) = c("mean", "se", "5% QT", "50% QT", "95% QT")
  mob.co.summary = rbind(apply(resamp$mob.co, 2, mean), apply(resamp$mob.co, 2, sd),
                         apply(resamp$mob.co, 2, quantile, c(0.05, 0.5, 0.95)))
  rownames(mob.co.summary) = c("mean", "se", "5% QT", "50% QT", "95% QT")
  
  return(list(formula = resamp$formula, coef = coef.summary, co.pct = co.pct.summary, mob.co = mob.co.summary))
}
