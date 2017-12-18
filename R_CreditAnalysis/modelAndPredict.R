#' fitting survival model, and plot the score using descr()
#' 
#' @param formul survival function
#' @param data dataset
#' @param seed seed in randomly drawing test set
#' @param te.pc percentage of data in test set
#' @param sepbins argument in descr(), whether bins are defined by fixed length "fixed", or by quantile "quantile"
#' @param na.rm argument in descr(), whether to remove the observations with missing values in modeling variables
#' @param plotGroupName argument in descr(), grouping variable


modeling_descr = function(formul, data, seed = 12345, te.pc = 0.2, 
                          binEvent, mobEvent, 
                          sepbins = "quantile", na.rm = TRUE, plotGroupname = "listing_term")
{
  library(survival)
  
  set.seed(seed)
  te.n = sample(1:nrow(data), round(te.pc*nrow(data)))
  train = data[-te.n,]
  test = data[te.n,]
  
  m = coxph(formul, data = train, model = TRUE)
  test$monjaRisk = exp(predict(m, test))
  
  descr(varname = "monjaRisk", groupname = plotGroupname, 
        binEvent = binEvent,  mobEvent = mobEvent, dat = test,
        type ="continuous",  na.rm = na.rm, 
        n.bins = 15, sep.bins = sepbins)
  
  return(summary(m))
}


#' fitting survival model, and seperate the test set into low/mid/high score groups, plotting their survival curve correspondingly
#' 
#' @param formul survival function
#' @param data dataset
#' @param predOn whether prediction based on train set, test set or all data (while model parameters are estimated upon train set)
#' @param quantil quantiles to categorize predicted Risk Scores and to show as separated curves on plot
#' @param na.rm whether to remove the observations with missing values in modeling variables
#' @param te.pc percentage of data in test set
#' @param seed seed in randomly drawing test set

modeling_ggsurv = function(formul, data, predOn = c("train", "test", "all"), quantil = c(0, 0.3, 0.7, 1),
                           na.rm = TRUE, te.pc = 0.2, seed = 12345)
{
  library(survival)
  
  #randomly seperate train and test sets
  set.seed(seed)
  te.n = sample(1:nrow(data), round(te.pc*nrow(data)))
  train = data[-te.n,]
  test = data[te.n,]
  
  #modeling
  m = coxph(formul, data = train, model = TRUE)
  
  #predicting on selected dataset
  if (predOn == "test") scoreDF = data.frame(survObj = test$survObj, monjaRisk = exp(predict(m, test)))
  if (predOn == "train")  scoreDF = data.frame(survObj = train$survObj, monjaRisk = exp(predict(m, train)))
  if (predOn == "all")  scoreDF = data.frame(survObj = data$survObj, monjaRisk = exp(predict(m, data)))
  
  #and set quantiles
  labels = paste0("(", quantil[1:(length(quantil)-1)], ",", quantil[2:length(quantil)], ")")
  scoreDF$quantile = cut(scoreDF$monjaRisk, 
                         breaks = quantile(scoreDF$monjaRisk, quantil, na.rm = TRUE), 
                         labels = labels, include.lowest = TRUE)
  
  #if not removing N/A's, calculate the percentage of N/A's
  if(na.rm == TRUE)  scoreDF = scoreDF[!is.na(scoreDF$quantile),]
  if(na.rm == FALSE)
  {
    scoreDF$quantile = as.character(scoreDF$quantile)
    scoreDF$quantile[is.na(scoreDF$quantile)] = 
      paste0("NA ", round(sum(is.na(scoreDF$quantile))/nrow(scoreDF)*100, 3), "%")
    scoreDF$quantile = as.factor(scoreDF$quantile)
  }
  
  #return ggsurv
  return(list(ggsurv = ggsurv(survfit(survObj ~ quantile, data = scoreDF)),
              summary = summary(m)))
}

