#Example Code: 
LC.dta<-subset(calcDta,platform=='LC')
F4<-as.formula(event.bin~inq_cnt+open_cnt+ I(as.factor(home))+time_to_default+I(time_to_default^2) +rate)
model.LC<-glm(F4,data=subset(calcDta,platform=='LC'  ),family=binomial())
summary(model.LC) 
varInfl.one(model.LC, "inq_cnt", LC.dta)
varInfl.mod(model.LC, LC.dat)


#' Looping over every variable in a model, plotting each against the predicting score
#' This function will automatically detect non-numeric variables and skip them
#' 
#' @param model
#' @param data

varInfl.mod = function(model, data, ...)
{
  plot.new()
  par(mfrow = c(2, 2))

  for(v in attr(terms(model), "term.labels"))
  {
    tryCatch({
      varInfl.one(model, v, data, ...)
    }, error = function(e) {})
  }
  par(mfrow = c(1,1))
}




#'plot model prediction score against a continuous variable, to see how much contribution this variable has to the model
#'
#'@param model 
#'@param varname
#'@param data
#'@param type, nBins, binVec all to be passed into to decide how prediction score is cutted and grouped

varInfl.one = function(model, varname, data, type = "nBins", nBins = 10, binVec = c(0, 0.3, 0.7, 1))
{
  pred = predict(model, data)
  predBin = binBreak(pred, type = type, nBins, binVec)
  plotForm = as.formula(paste0(varname, " ~ predBin"))
  if(class(data[, varname]) == "factor") 
  {
    data[,varname] = as.numeric(as.character(data[,varname]))
    if (sum(is.na(data[,varname])) == nrow(data)) stop("Not a numeric variable!")
  }
  if(class(data[, varname]) == "logical") data[,varname] = as.numeric(data[,varname])
  plot(aggregate(plotForm, data = data, FUN = mean), main = varname, ylab = "")
}




