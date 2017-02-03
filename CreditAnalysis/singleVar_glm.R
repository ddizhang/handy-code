#'run single variable glm model based on the original/deciles/tricect/optimized-cut variable correspondingly.
#'
#'Evaluate and compare the model by ROC curve, ks, auc and cohenKappa
#'@param binevent binary event
#'@param varname variable interested
#'@param dataset
#'@threshold threshold used for classification
#'calls setBins(), modelEval()

singleVar.glm = function(binevent, varname, dataset, threshold = threshold)
{
  newdata = cbind(
    dataset[, binevent],
    dataset[, varname],
    setBins(dataset, varNames = c(varname), method = "optim", binevent = binevent, newVarNameAppx = "opt"),
    setBins(dataset, varNames = c(varname), method = "quantile", binType = "nBins", nBins = 10, newVarNameAppx = "decil"),
    setBins(dataset, varNames = c(varname), method = "quantile", binType = "binVec", binVec = c(0, 0.33, 0.66, 1), newVarNameAppx = "random"))
  colnames(newdata) = c("binevent", "origin", "optim", "deciles", "random")
  
  m1 = glm(binevent ~ origin, data = newdata, family = "binomial")
  m2 = glm(binevent ~ optim, data = newdata, family = "binomial")
  m3 = glm(binevent ~ deciles, data = newdata, family = "binomial")
  m4 = glm(binevent ~ random, data = newdata, family = "binomial")
  
  stats = modelEval(list(m1, m2, m3, m4), respondName = "binevent", legend.off = TRUE, threshold = threshold)
  legend("bottomright", colnames(newdata)[2:5], lty = rep(1, 4), col = rainbow(4))
  stats
}

