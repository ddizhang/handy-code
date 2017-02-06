#' make up missing values
#'
#' the core function of "missing" series that actually does the labor
#' @param start day_id of start date (date diff between startdate and 2000-1-1)
#' @param end day_id of end date
#' @param df data frames with columns day_id and measurements, df should not include any missing values
#'
#' @return ms a data frame of made-up values which is of the same form as df
#' @export

missingMakeUp = function(start, end, df)
{
  ms = df[1,]; ms = ms[-1,]
  isMissingDay = !seq(start, end) %in% unique(df$day_id) #whether a day is a missing day
  isEmptyWeek = findEmptyWeeks(isMissingDay)

  coef = rep(0, (ncol(df)-1))
  mkup = rep(0, (ncol(df)-1))
  measurements = colnames(df)[-1]

  for(i in 1:(end - start + 1))
  {
    if (isEmptyWeek[i]) next

    if(isMissingDay[i])
    {
      tryCatch({
        if(i <= which(cumsum(!isEmptyWeek) == 8)) next
        #start working from the 2nd non-empty week
        #if first 4 weeks are empty, then we only take care of obs after the 5th week

        j = 1   #try i-7, then i+7. if both missing, next
        if (isMissingDay[i-7])
        {
          if (isMissingDay[i+7]) next
          else j = -1
        }

        k = 1
        while(i+(-1)^k*ceiling(k/2)-7*j > 0)   #try i-1&i-8, then i+1&i-6, then i-2&i-9, ...
        {
          new = i+ (-1)^k*ceiling(k/2)   #e.g. i-1
          old = i+(-1)^k*ceiling(k/2)-7*j    #e.g. i-8

          if ( new > end-start+1 | isMissingDay[new]| isMissingDay[old])  #last day | missing value
            k = k + 1
          else break
        }
        if (i+(-1)^k*ceiling(k/2)-7*j <= 0 | new > end - start + 1) next
        #can't find suitable values to make up the missing value

        for(m in 1:length(measurements))
        {
          coef[m] = df[df$day_id == new+start-1,measurements[m]] / df[df$day_id == old+start-1,measurements[m]]
          mkup[m] = df[df$day_id == i+start-1-7*j,measurements[m]] * coef[m]
        }
        temp = data.frame(matrix(c(i+start-1, mkup), ncol = ncol(df)))
        colnames(temp) = colnames(df)
        ms = rbind(ms, temp)

      },error=function(e) NULL)
    }
  }
  return (ms)
}



