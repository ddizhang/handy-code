#' smbinning on one sample 
#' 
#' returning the cutting bands and woe
#' @param data data
#' @param sampID sample information dataframe, typically a return from sampleSelect()
#' @param sampName column name of sample in sampID we wish to use
#' @param weightName column name of weight, actually not used in this function
#' @param y response variable
#' @param x the variable whose optimal cut we wish to find
#' 
getSmbin = function(data, sampID, sampName, weightName, y, x)
{
  library(smbinning)
  smdata = getModelData(data, sampID, sampName = sampName, weightName = weightName)
  smbin = smbinning(df = smdata, y = y, x = x)
  if(typeof(smbin) == "character") 
  {
    bands = c(min(smdata[,x]), max(smdata[,x]))
    woe = 0
    #cuts = NULL
  }
  else 
  {
    bands = smbin$bands
    woe = smbin$ivtable$WoE
    #cuts = smbin$cuts
  }
  return(list(bands = bands, woe = woe))
}


#' helper function in sampSMBwrapper
#' 
smbin.stats = function(smbin.obj)
{
  return(c(cut.count = as.integer(length(smbin.obj$bands)-2), 
           bands.sd = sd(smbin.obj$bands), 
           woe.sd = sd(smbin.obj$woe, na.rm = TRUE)))
}



#' smbinning, replicating n times
#' 
#' plots smbinning cuts in each replication, also return the mean and sd of following stats:
#' number of cuts found, the standard deviation of bands and the sd of WOE in each replication
#' 
#' @param data data
#' @param vars variable used to form cells in stratified sampling
#' @param cellSize size of each cell, either a constant or a vector of the same length of the category
#' @param nSamps number of replication
#' @param y response variable
#' @param x the variable whose optimal cut we wish to find
#' @param col color in plotting the smb cuts
#' 
sampSMBwrapper = function(data, vars, cellSize, nSamps, y, x, col = "red")
{
  smbin.all = smbinning(df = data, y = y, x = x)
  
  catDic = getCat(data, vars)
  catDic = setParams(catDic, cellSize = cellSize, weight = 1)
  sampID = sampSelect(data, catDic, nSamps = nSamps)
  smbins = lapply(paste0("sample", 1:nSamps), 
                  function(t) getSmbin(data, sampID, t, "weight", y = y, x = x))
  
  plot(0, 0, xlim = c(0,length(smbins)), ylim = c(min(smbins[[1]]$bands), max(smbins[[1]]$bands)), col = "white")
  for ( i in 1: length(smbins))
    points(rep(i, length(smbins[[i]]$bands)), smbins[[i]]$bands, pch = 20, col = col)
  if(class(smbin.all) != 'character') abline(h = smbin.all$bands, col = "grey")
  
  stats = sapply(smbins, smbin.stats)
  rbind(mean = apply(stats,1, mean, na.rm = TRUE), sd = apply(stats, 1, sd, na.rm = TRUE)) 
}


#' smbinning, and test on testset, replicating n times
#'
#' Apply the optimal cuts found in training set on a testset, and return the iv table, badrate sd, odds sd of it
#' 
#' @param tr training set where samples are drawn from
#' @param te test set
#' @param vars variable used to form cells in stratified sampling
#' @param cellSize size of each cell, either a constant or a vector of the same length of the category
#' @param nSamps number of replication
#' @param y response variable
#' @param x the variable whose optimal cut we wish to find
#' 
sampSMBontest = function(tr, te, vars = c("binevent", "supragrade"), 
                         cellSize = getCellSize(c(1, 64, 3, 68, 1.7, 16), 20000), 
                         nSamps = 3, y = "binevent", x = "dti")
{
  catDic = getCat(tr, vars)
  catDic = setParams(catDic, cellSize = cellSize, weight = 1)
  sampID = sampSelect(tr, catDic, nSamps = nSamps)
  smbins = lapply(paste0("sample", 1:nSamps), 
                  function(t) getSmbin(tr, sampID, t, "weight", y, x))
  
  test = lapply(smbins, function(t) { 
    #tryCatch({
    smb.cus = smbinning.custom(df = te, y = y, x = x, cuts = t$bands)
    if (class(smb.cus) != "character")
    return(c(badrate.sd = sd(smb.cus$ivtable$BadRate, na.rm = TRUE), 
             odds.sd = sd(smb.cus$ivtable$Odds, na.rm = TRUE), 
             iv = smb.cus$iv))
    else return("no cut")
    #}, error = function(e) NULL )
  })
  test
}
