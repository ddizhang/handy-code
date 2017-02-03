
#' Plot the quantiles of a variable against quarterly date 
#' 
#' @param data data
#' @param ContinuousVar the continuous variable of which quantiles to be plotted
#' @param segVar if only inspecting a subset of the original data, the subsetting variable (has to be a factor object). If not, "ALL"
#' @param segLev the level of subsetting variable we're inspecting
#' @param dateVar the date variable (has to be a date object)
#' @param quantiles the quantiles of ContinuousVar we wish to plot
#' @param startDate start date
#' @param endDate end date
#' @param fullTitle whether to add full title to the plot, or just indicating the subset level name

scoreTrend = function(data, ContinuousVar, segVar = "ALL", segLev = NULL, dateVar = "issue_d",
                      quantiles = c(0.05, 0.25, 0.5, 0.75, 0.95), startDate = "2014-01-01", endDate = "2016-04-01", fullTitle = TRUE)
{  
  library(ggplot2)
  library(dplyr)
  library(lubridate)
  library(reshape2)
  
  test = 
    data %>% mutate_(date = dateVar, segvar = segVar, value = ContinuousVar) %>%
    transmute(date = date, quarter = paste0(year(date), quarters(date)), 
              id = id, segvar = segvar, value = value) %>%
    filter(date >= startDate & date < endDate)
  
  if (segLev == "ALL")
    ag = aggregate(value ~ quarter, data = test, quantile, probs = quantiles)
  else 
    ag = aggregate(value ~ quarter, data = test[test[,"segvar"] == segLev,], quantile, probs = quantiles)
  
  ag = as.matrix(ag)
  colnames(ag) = c("quarter", paste0("Q", quantiles*100))
  ag = melt(data.frame(ag), id = "quarter")
  colnames(ag) = c("quarter", "quantile", "value")
  ag$value = as.numeric(as.character(ag$value))
  
  p = ggplot(ag, aes(quarter, value, colour = quantile)) +
    geom_line(aes(group = quantile)) + 
    xlab("") + ylab("Score") + scale_colour_discrete(name = "Quantile")  
 
  if (fullTitle)
    p = p + ggtitle(paste("Trend of", ContinuousVar, "by Quantile,", segVar, segLev))
  else 
    p = p + ggtitle(segLev)
  
  return(p)
}


