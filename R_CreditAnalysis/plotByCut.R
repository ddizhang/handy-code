#' Plot the mean value of a variable by a sequential grouping variable 
#' 
#' @param data data
#' @param cutVar the sequential grouping varialbe
#' @param plotVar the variable whose mean is plotted
#' 
#' @example 
#' lc$numericGrade = as.numeric(lc$grade)
#' plotByCut(lc, cutVar = "numericGrade", plotVar = "event_bad")


plotByCut = function(data, cutVar, plotVar)
{
  tb = tapply(data[,plotVar], data[,cutVar], mean)
  tb.df = data.frame(cutVar = as.numeric(names(tb)), plotVar = tb)
  complete = merge(tb.df, data.frame(cutVar = 1:max(data[,cutVar], na.rm = TRUE)), all.y = TRUE)
  plot(complete$cutVar, complete$plotVar, type = "l", main = paste(plotVar, "by", cutVar), 
       xlab = cutVar, ylab = plotVar)
}

