
#' stratified sampling: defining cells
#' 
#' @param data dataset
#' @param vars variable names to form stratas

getCat = function(data, vars)
{
  library(reshape2)
  catDic = melt(tapply(data[,1], data[,vars], function(x) return(1)))
  catDic$value = 1:nrow(catDic)
  names(catDic)[which(names(catDic) == "value")] = "category"
  if(length(vars) == 1) names(catDic)[1] = vars 
  return(catDic)
}


#' stratified sampling: calculate cell size given raito between each cell and the sample size
#' 
#' @param ratio ratio between each cell
#' @param total sample size
getCellSize = function(ratio, total)
  round(ratio * total/sum(ratio))



#' stratified sampling: set size and weight for each cell
#' 
#' @param catDic object returned from getCat()
#' @param cellSize size of each cell, either a constant or a vector of the same length of the category
#' @param weight weight of each cell, eigher a constant or a vector

setParams = function(catDic, cellSize = 1000, weight = 1)
{
  l = nrow(catDic)
  if(!length(cellSize) %in% c(1, l)) stop("Wrong cellSize length!")
  if(!length(weight) %in% c(1, l)) stop("Wrong weight length!")
  
  catDic$cellSize = cellSize
  catDic$weight = weight
  catDic
}



#' stratified sampling: draw samples
#' 
#' return a dataframe with all rows from the original data frame, with columns "sample1", "sample2",... indicating which record is in sample
#' @param data dataset
#' @param catDic dataframe containing the size and weight of each cell, typically the return from getCat() and setParam()
#' @param nSamps number of trails/replications
sampSelect = function(data, catDic, nSamps)
{
  data$rowid = 1:nrow(data)
  dfsampID = merge(data[, c(names(catDic)[!names(catDic) %in% c("cellSize", "weight", "category")], "rowid")], catDic)
  
  sampSelect0 = function(catDic,dfsampID)
  {
    sampList = lapply(1:nrow(catDic), function(i, data, catDic) {
      sample(which(data$category == i), size = catDic$cellSize[i])
    }, dfsampID, catDic)
    
    samp = unlist(sampList)
    insamp = rep(0, nrow(dfsampID))
    insamp[samp] = 1
    insamp
  }

  samps = data.frame(replicate(nSamps, sampSelect0(catDic, dfsampID)))
  names(samps) = paste0("sample", 1:ncol(samps))
  dfsampID = cbind(dfsampID, samps)

  return(dfsampID)
}


  
#' stratified sampling: change/add weight vectors
#' 
#' adding another column indicating the weight to the sample dataframe (typically a return from sampSelect())
#' @param newWt a vector of the same length as Category, indicating the new weight of each cell
#' @param dfsampID dataframe of all records and their category
chgWt = function(newWt, dfsampID, catDic, updateDic = FALSE, add = TRUE)
{
  catDic$weight = newWt
  if(add == FALSE) 
    dfsampID = dfsampID[, names(dfsampID) != "weight"]
  
  names = intersect(names(dfsampID), names(catDic))
  dfsampID = merge(dfsampID, catDic, by = names[!grepl("weight", names)])
  wts = grep("weight", names(dfsampID))
  names(dfsampID)[wts] = paste0("weight", 1:length(wts))
  
  if(updateDic == TRUE)
    return(list(catDic = catDic, dfsampID = dfsampID))
  else return(dfsampID)
}


#' stratified sampling: extract a sample from the whole data set
#' 
#' @param data original dataset
#' @param dfsampID sample information data frame, typically a return from sampSelect()
#' @param sampName name of sample column in dfsampID
#' @param weightName name of weight column in dfsampID

getModelData = function(data, dfsampID, sampName, weightName)
{
  modeldf = data[dfsampID$rowid[dfsampID[,sampName] == 1],] 
  modeldf$weight = dfsampID[dfsampID[,sampName] == 1, weightName]
  return(modeldf)
}



#' build model on a sample of original dataset
#' 
#' take in a sample from original dataset along with the weight of each observation, and build a weighted model
#' @param data original dataset
#' @param sampID sample information dataframe, typically a return from sampleSelect()
#' @param sampName column name of sample in sampID we wish to use
#' @param weightname column name of weight in sampID we wish to use
#' @param formul a model formula
#' 
model = function(sampName, data, sampID, formul, weightName) 
{
  modeldata = getModelData(data, sampID, sampName = sampName, weightName = weightName)
  m = glm(formul, data = modeldata, family = "binomial", weight = weight)
  m
}


#' build model on multiple samples and return the mean and sd of each coefficient estimate
#' 
#' @param data original dataset
#' @param sampID sample information dataframe, typically a return from sampleSelect()
#' @param formul a model formula
#' @param weightName column name of weight in sampID we wish to use
#' @param nSamps number of replication

model.mc = function(data, sampID, formul, weightName, nSamps)
{
  test = t(sapply(paste0("sample", 1:nSamps), 
                  function(t) model(sampName = t, data, sampID, formul, weightName = weightName)$coef
  ))
  rbind(mean = apply(test, 2, mean), sd = apply(test,2, sd))
}






