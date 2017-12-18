#' a series of plots of the quantiles of a variable against quarterly date, segmented by a segmenting variable
#' 
#' Compare with scoreTrend(): scoreTrend() only plots one level of a segVar, scoreTrend.seg() plots all level of it
#' @param data data
#' @param ContinuousVar the continuous variable of which quantiles to be plotted
#' @param segVar segmenting variable
#' @param dateVar the date variable (has to be a DATE object)
#' @param startDate start date
#' @param endDate end date
#' @param yLim: yLim of the plot: if yLim = "fixed", then all sub-plots use the same yLim; if yLim = NULL, each of them use their best adjusted yLims;  or you can assign yLims manually by setting yLim = c(lower, upper)
#' 
scoreTrend.seg = function(data, ContinuousVar, segVar = "grade", dateVar = "issue_d", 
                          startDate = "2014-01-01", endDate = "2016-04-01", yLim = NULL, ...)
{  
  library(gridExtra)
  data = 
    data %>% mutate_(issue_d = dateVar, segvar = segVar, value = ContinuousVar) %>%
    transmute(issue_d = issue_d, quarter = paste0(year(issue_d), quarters(issue_d)), 
              id = id, segvar = segvar, value = value) %>%
    filter(issue_d >= startDate & issue_d < endDate)
  
  if (!is.null(yLim))
  {
    if (yLim == "fixed")
    {
      ag.all = aggregate(value ~ quarter + segvar, data = data, quantile, probs = c(0.05, 0.25, 0.5, 0.75, 0.95))
      yLim = c(min(unlist(ag.all$value)), max(unlist(ag.all$value)))
    }
  }
  
  if(is.null(yLim))
    ps = lapply(c("ALL", levels(data$segvar)), function(l) 
      scoreTrend(data, ContinuousVar = "value", segVar = "segvar", segLev = l, dateVar = "issue_d", startDate = startDate, endDate = endDate, fullTitle = FALSE))
  else if(class(yLim) == "numeric" & length(yLim) == 2)
    ps = lapply(c("ALL", levels(data$segvar)), function(l) 
      scoreTrend(data, ContinuousVar = "value", segVar = "segvar", segLev = l, dateVar = "issue_d", startDate = startDate, endDate = endDate, fullTitle = FALSE) + coord_cartesian(ylim = yLim))

  do.call(grid.arrange, c(ps, top = paste("Trend of", ContinuousVar, "by Quantile, Seg by", segVar), ...))
}


