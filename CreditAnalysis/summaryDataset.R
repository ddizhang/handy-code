# return quantiles for continuous variables in a dataset
summary.dataset = function(data, conVarName = "ALLVARIABLES", strataName, 
                           nBins = 10, quantil = round(seq(0, 1, length.out = nBins + 1), digit = 4))
{
  #if "ALLVARIABLES" get all variable names
  if (length(conVarName) == 1)
  {
    if(conVarName == "ALLVARIABLES")
      conVarName = colnames(data)[which(sapply(data, class) %in% c("numeric", "integer"))]
  }
  
  #loop over each variable and do summary quantiles
  res.list = lapply(data[, conVarName], 
                    function(t) {
                      summary.conVar.qt(t, strata = data[, strataName], quantil = quantil)})
  len = nrow(res.list[[1]])
  res = do.call(rbind, res.list)
  res = data.frame(variable = unlist(lapply(conVarName, rep, len)),
                   res)
  rownames(res) = NULL
  return(res)
}


#return quantiles for ONE continuous variable
summary.conVar.qt = function(var, strata, quantil = c(0, 0.2, 0.4, 0.6, 0.8, 1))
{
  #loop over each strata and return the quantiles
  res0 = tapply(var, strata, 
              function(t) { res = c(quantile(t, probs = quantil, na.rm = TRUE), 
                                    length(t), mean(is.na(t)))
                            names(res) = c(paste0("q", quantil*100), "# obs", "% NA\'s")
                            return(res)})
  
  #melt res0 from wide format into long format
  res1 = cbind(expand.grid(lapply(strata, levels)), do.call(rbind, res0))
  res2 = melt(res1)
  colnames(res2)[(ncol(res2)-1)] = "Statistics"
  colnames(res2)[1:(ncol(res2)-2)] = paste0("Strata:", colnames(res2)[1:(ncol(res2)-2)])
  return(res2)
}





#prototype of summary.conVar.qt that take in only one single strata
summary.conVar.qt0 = function(var, strata, quantil = c(0, 0.2, 0.4, 0.6, 0.8, 1))
{
  res.list = lapply(levels(as.factor(strata)), 
                    function(t)
                      data.frame(strata = rep(t, length(quantil) + 2),
                                 statistics = c(paste0("q", quantil*100), "# obs", "% NA\'s"),
                                 value = c(quantile(var[strata == t], probs = quantil, na.rm = TRUE), 
                                           length(var[strata == t]), mean(is.na(var)))))          
  return(do.call(rbind, res.list))
}

