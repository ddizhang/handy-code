deseason = function(y,  swindow = "periodic", type = "a", ...)
{
  if (!"ts" %in% class(y)) stop("In deseason: Input should be a ts object!")
  if (!type %in% c("a", "m"))
    stop("wrong 'type' parameter!")
  
  freq = frequency(y)
  start = start(y)
  
  if (length(y) < 2*freq)
  {
    warning("Deseasonalization not done: Length of time series should be no less than 2 periods!")
    return(y)
  }
  makeup = 0
  if (length(y) == 2*freq)
  {
    y = ts(c(y, mean(y[1], y[1+freq], y[2*freq])),frequency=freq)
    makeup = 1
  }
  
  if (type == "a")
  {
    sea = stl(y, s.window = swindow, ...)
    y.desea = sea$time.series[,2]+sea$time.series[,3]
    sea.factor = sea$time.series[,1]
  }
  else if (type == "m")
  {
    sea = stl(log(y), s.window = swindow)
    y.desea = exp(sea$time.series[,2]+sea$time.series[,3])
    sea.factor = exp(sea$time.series[,1])
    
    # if multiplicative: relevel the seasonal factors so that the deseasoned series and original series have the same mean value
    adj = sum(y)/sum(y.desea)
    sea.factor = sea.factor/adj
    y.desea = y/sea.factor
    
    
    
  }
  
  if (makeup == 1)
  {
    y.desea = ts(y.desea[1:(2*freq)], frequency=freq, start = start)
    sea.factor = ts(sea.factor[1:(2*freq)], frequency=freq, start = start)
    
  }
  

  return(list(y.desea = y.desea, sea.factor = sea.factor))
}
