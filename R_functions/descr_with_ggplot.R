#' Descriptive plots of a variable with histogram and its correlation with event binary and time-to-event
#' 
#' @param varname intended variable
#' @param groupname grouping variable
#' @param binEvent a variable indicating whether the event interested (such as defaulting) happens
#' @param mobEvent a variable showing the mob pervormance of the observation
#' @param dat dataframe where varname, groupname and mobEvent comes frome
#' @param type whether varname is discrete or continuous variable
#' @param qWinsor quantiles for winsorization
#' @param range.min the minimun x coordinate value (out of use)
#' @param range.max the maximun x coordinate value (out of use)
#' @param na.rm whether NA's should be removed or be considered as a seperate group
#' @param n.bins number of bins
#' @param sep.bins whether bins are defined by fixed length "fixed", or by quantile "quantile"
#' 
#' @example 
#'descr(varname = "fico_range_high", groupname = "grade", 
#'      binEvent = "eventBinary", mobEvent = "event_time_all", dat = data, qWinsor = c(0, 0.95),
#'      type ="continuous", sep.bins = "quantile", 
#'      n.bins = 10, na.rm = FALSE, binYlab = "test1", timeYlab = "test2")

#' ## one can also choose not to display the plot but saves it by the following approach:
#'   png(filename=file.path(paste("./", varname,'.png')))
#'   descr(varname = "all801", groupname = "listing_term", mobEvent = "mob_co", dat = data, type ="continuous")
#'   dev.off()
#'   


descr = function(varname, groupname, binEvent, mobEvent, dat,
                 type = c("discrete", "continuous"), 
                 qWinsor = c(0, 0.95), na.rm = FALSE, 
                 range.min = min(newvar0, na.rm = TRUE), range.max = max(newvar0, na.rm = TRUE), 
                 n.bins = 15, sep.bins = c("quantile", "fixed"),
                 binYlab = "is.event", timeYlab = "time.before.event")
{
  library(ggplot2)
  library(reshape2)
  library(grid)
  library(gridExtra)
  
  #=======================================
  #dat$binEvent = !is.na(dat[,mobEvent])
  dat[, mobEvent] = ifelse(dat[, binEvent], dat[, mobEvent], NA)
  
  #if na.rm = TRUE, simply remove the rows with NAs
  if (na.rm == TRUE)  dat = dat[!is.na(dat[,varname]),] 
  
  #defaulted observations
  co.rows = which(!is.na(dat[,mobEvent]))
  newvar0 = dat[,varname]
  
  if (type == "discrete")
  {
    newvar = newvar0
    newvar[is.na(newvar)] = "NA"  #NA's
    newvar = as.factor(newvar)
  }
  
  if (type == "continuous")
  {
    newvar0 = winsor(newvar0, qMin = qWinsor[1], qMax = qWinsor[2]) #remove top say 5% 
    
    if(sep.bins == "fixed")
      cutpoint = seq(range.min-0.00001, range.max+0.00001, length.out = n.bins)
    if(sep.bins == "quantile")
      cutpoint = quantile(newvar0, 1:n.bins/n.bins, na.rm = TRUE)
    
    #if no NA's in data
    if (sum(is.na(newvar0)) == 0)
    {
      newvar = cut(newvar0, breaks = cutpoint, labels = cutpoint[1:(n.bins-1)])
      levels(newvar) = round(as.numeric(levels(newvar)), digits = 3)
      
      if(sep.bins == "quantile")  levels(newvar) = 1:n.bins/n.bins
    }
    
    #if exists NA's in data
    if (sum(is.na(newvar0)) > 0)
    {
      newvar0[is.na(newvar0)] = -999 #NA's
      newvar = cut(newvar0, breaks = c(-1000, cutpoint), 
                   labels = c(cutpoint[1] - 3*(cutpoint[2] - cutpoint[1]), 
                              cutpoint[1:(n.bins-1)]))
      levels(newvar) = round(as.numeric(levels(newvar)), digits = 3)
      
      if(sep.bins == "quantile")  levels(newvar) = c(-0.5, 1:n.bins/n.bins)
      
      newvar0[newvar0 == -999] = cutpoint[1] - 3*(cutpoint[2] - cutpoint[1])
    }
  }
  
  plotData = data.frame(newvar0, newvar, groupvar = as.factor(dat[,groupname]),
                        isCO = dat[,binEvent], mobEvent = dat[,mobEvent])
  
  #--------plot the histogram of variable
  if (type == "continuous")
    p1 <- qplot(newvar0, colour = groupvar, data = plotData, geom = "histogram") +
    xlab("")
  if (type == "discrete")
    p1 <- qplot(as.character(newvar), colour = groupvar, data = plotData, geom = "bar") +
    xlab("")
  
  #--------plot variable against isCO
  tb.isco = tapply(plotData$isCO, INDEX = list(plotData$newvar, plotData$groupvar), mean)  #grouped data
  df.isco = melt(tb.isco)
  df.isco$Var2 = as.factor(df.isco$Var2)  #groupvar
  
  p2 <- ggplot(df.isco, aes(Var1, value, colour = Var2)) + 
    geom_line(aes(group = Var2)) + 
    geom_smooth(aes(group = 1)) + 
    xlab("") + ylab(binYlab) + 
    scale_colour_discrete(name = groupname)
  
  #--------plot variable against mob_co
  plotData.co = plotData[co.rows, ]
  tb.mobco = tapply(plotData.co$mobEvent, 
                    INDEX = list(plotData.co$newvar, plotData.co$groupvar), mean)
  df.mobco = melt(tb.mobco)
  df.mobco$Var2 = as.factor(df.mobco$Var2)  #groupvar
  
  p3 <- ggplot(df.mobco, aes(as.factor(Var1), value, colour = Var2)) + 
    geom_line(aes(group = Var2)) +
    geom_smooth(aes(group = 1)) +
    xlab("") + ylab(timeYlab) +
    scale_colour_discrete(name = groupname)
  
  #-----------------------------------
  grid.arrange(p1, p2, p3, ncol = 1, top = varname)
}
