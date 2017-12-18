#' a combination of modeling_descr and modeling_ggsurv

modeling = function(formul, data, predOn = c("train", "test", "all"), method = c("descr", "ggsurv", "both"), 
                    na.rm = TRUE, te.pc = 0.2, seed = 12345, 
                    quantil = c(0, 0.3, 0.7, 1),
                    plotGroupname = "listing_term")
{
  library(survival)
  
  set.seed(seed)
  te.n = sample(1:nrow(data), round(te.pc*nrow(data)))
  train = data[-te.n,]
  test = data[te.n,]
  
  m = coxph(formul, data = train, model = TRUE)
  
  if (predOn == "test") scoreDF = data.frame(test, monjaRisk = exp(predict(m, test)))
  if (predOn == "train")  scoreDF = data.frame(train, monjaRisk = exp(predict(m, train)))
  if (predOn == "all")  scoreDF = data.frame(data, monjaRisk = exp(predict(m, data)))
  
  if (method %in% c("descr", "both"))
  {
    descr(varname = "monjaRisk", groupname = plotGroupname, 
          binEvent = "binary_bpp",  mobEvent = "tau_pp", dat = test,
          type ="continuous",  na.rm = na.rm, 
          n.bins = 15, sep.bins = sepbins)
    
    if (method = "descr") return(summary(m))
  }
  
  if (method %in% c("ggsurv", "both"))
  {
    labels = paste0("(", quantil[1:(length(quantil)-1)], ",", quantil[2:length(quantil)], ")")
    scoreDF$quantile = cut(scoreDF$monjaRisk, 
                           breaks = quantile(scoreDF$monjaRisk, quantil, na.rm = TRUE), 
                           labels = labels, include.lowest = TRUE)
    
    if(na.rm == TRUE)  scoreDF = scoreDF[!is.na(scoreDF$quantile),]
    if(na.rm == FALSE)
    {
      scoreDF$quantile = as.character(scoreDF$quantile)
      scoreDF$quantile[is.na(scoreDF$quantile)] = 
        paste0("NA ", round(sum(is.na(scoreDF$quantile))/nrow(scoreDF)*100, 3), "%")
      scoreDF$quantile = as.factor(scoreDF$quantile)
    }
    
    return(list(ggsurv = ggsurv(survfit(survObj ~ quantile, data = scoreDF)),
                summary = summary(m)))
  }
  
}
