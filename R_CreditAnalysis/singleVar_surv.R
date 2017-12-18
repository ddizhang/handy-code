#'run single variable survival model based on the original/deciles/tricect/optimized-cut variable correspondingly.
#'
#'Evaluate and compare the model by ROC curve, ks, auc and cohenKappa
#'@param binevent binary event
#'@param varname variable interested
#'@param survobj survival object for survival model
#'@param dataset
#'@threshold threshold used for classification
#'@example singleVar.surv(binevent = "binary_bpp", varname = "pti", survobj = "survObj", data)


singleVar.surv = function(binevent, varname, survobj, mobevent, dataset, threshold = 0.06)
{
  library(survival)
  newdata = cbind(
    dataset[, binevent],
    dataset[, survobj],
    dataset[, mobevent],
    dataset[, varname],
    setBins(dataset, varNames = c(varname), method = "optim", binevent = binevent, newVarNameAppx = "opt"),
    setBins(dataset, varNames = c(varname), method = "quantile", binType = "nBins", nBins = 10, newVarNameAppx = "decil"),
    setBins(dataset, varNames = c(varname), method = "quantile", binType = "binVec", binVec = c(0, 0.33, 0.66, 1), newVarNameAppx = "random"))
  colnames(newdata) = c("binevent", "survobj", "mobevent", "origin", "optim", "deciles", "random")
  
  m1 = coxph(survobj ~ origin, data = newdata, model = TRUE)
  m2 = coxph(survobj ~ optim, data = newdata, model = TRUE)
  m3 = coxph(survobj ~ deciles, data = newdata, model = TRUE)
  m4 = coxph(survobj ~ random, data = newdata, model = TRUE)
  
  stats = modelEval(list(m1, m2, m3, m4), testSet = newdata, respondName = "binevent",
                    threshold = threshold, legend.off = TRUE, rsq = FALSE)
  legend("bottomright", colnames(newdata)[4:7], lty = rep(1, 4), col = rainbow(4))
  
  newdata = cbind(newdata,
                  q1 = binBreak(exp(predict(m1, newdata)), type = "nBins"),
                  #q2 = binBreak(exp(predict(m2, newdata)), type = "nBins"),
                  q3 = binBreak(exp(predict(m3, newdata)), type = "nBins"))
                  #q4 = binBreak(exp(predict(m4, newdata)), type = "nBins"))
  l = ncol(newdata)
  par(mfrow = c(2,1))
  
  bintb = lapply(newdata[, c("q1", "q3")], function(t) tapply(newdata$binevent, t, mean))
  plot(bintb[[1]], type = "l"); lines(bintb[[2]], type = "l", col = "red")
  
  mobtb = lapply(newdata[, c("q1", "q3")], function(t) tapply(newdata$mobevent, t, mean))
  plot(mobtb[[1]], type = "l"); lines(mobtb[[2]], type = "l", col = "red")
  
  par(mfrow = c(1,1))
  stats
}


