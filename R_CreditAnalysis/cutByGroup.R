#'Cut a continuous variable into quantiles within each given subsets
#'returns the original data frame with an extra column named "cuts" indicating which quantile this record is in
#'
#'@param data data
#'@param variable the variable to cut into quantiles
#'@param groupBy the subsetting variable
#'@param nCut number of quantiles to cut the variable into
#'
#'@example
#'a = cutByGroup(lc, variable = "DTI", groupBy = "grade", nCut = 10)
#'table(a$cuts, a$grade)

cutByGroup = function(data, variable, groupBy, nCut)
{
  library(dplyr)
  #library(lazyeval)
  data.gr = group_by_(data, groupBy)
  call = substitute(group_by(data.gr, cuts = ntile(x = v, n = nCut)), list(v = as.name(variable), nCut = nCut))
  res = eval(call)
  return(data.frame(res))
}
