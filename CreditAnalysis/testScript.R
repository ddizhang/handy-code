data = read.csv("testData.csv")
names(data)
library(survival)
data$survObj = Surv(data$event_time_all, data$eventBinary)

formul = as.formula(survObj ~ strata(grade) + cat_fico+cat_Inq+monthly_dti_pct)
formul = as.formula(survObj ~ strata(grade) + fico_range_low+inq_last_6mths+borrowerrate)
formul = as.formula(survObj ~ strata(grade) + fico_range_low+ I(inq_last_6mths>3) + I(monthly_dti_pct>.15))
formul = as.formula(survObj ~ strata(grade) + fico_range_low+monthly_lti_pct+monthly_dti_pct)
formul = as.formula(survObj ~ strata(grade) + fico_range_low+I(monthly_lti_pct+monthly_dti_pct) + I(monthly_dti_pct^2))

formul = as.formula(eventBinary ~ cat_fico+cat_Inq+monthly_dti_pct) #1
formul = as.formula(eventBinary ~ fico_range_low+inq_last_6mths+borrowerrate) #2
formul = as.formula(eventBinary ~ fico_range_low+ I(inq_last_6mths>3) + I(monthly_dti_pct>.15)) #3
formul = as.formula(eventBinary ~ fico_range_low+monthly_lti_pct+monthly_dti_pct) #4
formul = as.formula(eventBinary ~ fico_range_low+I(monthly_lti_pct+monthly_dti_pct) + I(monthly_dti_pct^2)) #5

#-----------------
#binBreak
a = binBreak(data$annual_inc, type = "nBins", nBins = 5)
a = binBreak(data$annual_inc, type = "binVec", binVec = c(0, 0.5, 1))
a = binBreak(data$annual_inc, type = "breaks", breaks = c(0,1000,20000,30000, 1000000))
summary(a)

#-----------------
#setBins
test = setBins(data, varNames = c("fico_range_high", "inq_last_6mths"), method = "optim", binevent = "intBinary")
test = setBins(data, varNames = c("fico_range_high", "inq_last_6mths"), method = "quantile", binType = "nBins", nBins = 4, newVarNameAppx = "yoohoo")
test = setBins(data[, c("fico_range_high", "inq_last_6mths", "annual_inc", "monthly_dti_pct")], method = "quantile", binType = "nBins", nBins = 4, newVarNameAppx = "yoohoo")

#-----------------
#coefStability

coefStability.glm(formul, data$grade, data, binevent = "eventBinary")
#code failed on formul1 because in this dataset there's no obs in grade G that has fico higher than 740
coefStability.surv(formul, data$grade, data)


#-----------------
#combineBins
g.no = combineBins(data$fico_range_high, data$survObj, alpha = 0.001)
ggsurv(survfit(data$survObj ~ g.no))


#-----------------
#cutByGroup
a = cutByGroup(data, variable = "fico_range_high", groupBy = "grade", nCut = 10)
table(a$cuts, a$grade)


#-----------------
#plotByCut
data$numericGrade = as.numeric(data$grade)
plotByCut(data, cutVar = "numericGrade", plotVar = "eventBinary")


#-----------------
#descr
descr(varname = "fico_range_high", groupname = "grade", 
      binEvent = "eventBinary", mobEvent = "event_time_all", dat = data, qWinsor = c(0, 0.95),
      type ="continuous", sep.bins = "quantile", 
      n.bins = 10, na.rm = FALSE, binYlab = "test1", timeYlab = "test2")

data$all = 1
descr(varname = "grade", groupname = "all", 
      binEvent = "eventBinary", mobEvent = "event_time_all", dat = data, qWinsor = c(0, 0.95),
      type ="discrete", sep.bins = "quantile", 
      n.bins = 10, na.rm = FALSE)


#-----------------
#modelBlending

tr = data[1:7000, ]

m1 = glm(eventBinary ~ cat_fico+cat_Inq+monthly_dti_pct, data, family = "binomial")
m2 = glm(eventBinary ~ fico_range_low+inq_last_6mths+borrowerrate, data, family = "binomial")
m3 = glm(eventBinary ~ fico_range_low+ I(inq_last_6mths>3) + I(monthly_dti_pct>.15), data, family = "binomial")

m1 = coxph(survObj ~ strata(grade) + cat_fico+cat_Inq+monthly_dti_pct, data = data, model = TRUE)
m2 = coxph(survObj ~ strata(grade) + fico_range_low+inq_last_6mths+borrowerrate, data = data, model = TRUE)

pred = blendPred(m1, m2, te, 0.3)
optBlend(data, m1, m2, "eventBinary", criteria = "KS")


#-----------------
#modelEval
te = data[3000:5000, ]
modelEval(list(m1, m2), testSet = te, respondName = "eventBinary", rsq = FALSE)
modelEval(list(m1, m2),  respondName = "eventBinary", rsq = FALSE)
#will break on coxph models when test set is not indicated

modelEval.vec(list(blendPred(m1, m2, te, 0.3), predict(m1, te), predict(m2, te)), actualVec = te$eventBinary)


#-----------------
#resamp
formul = as.formula(survObj ~ strata(grade) + fico_range_low+inq_last_6mths+borrowerrate)
#only on survival models
resamp1 = resamp.surv(formul, data, binevent = "eventBinary", timeToEvent = "event_time_all", 5, te.pc = 0.5)
summary(resamp1)


#-----------------
#stratified sampling modeling

#define cells
catDic = getCat(data, c("eventBinary", "flag_priordq"))
catDic

#set size and weight for each cell
#first argument: ratio between each cell, second argument: sample size
csize = getCellSize(c(1, 2, 1, 2), 900)

#cellSize can be a constant, or a vector
catDic = setParams(catDic, cellSize = csize, weight = 1)
catDic = setParams(catDic, cellSize = 300, weight = c(1, 2, 1, 3))

#sampling, nSamps is the number of trials, samples will be named as "sample1", "sample2", ...
sampID = sampSelect(data, catDic, nSamps = 10)

#change weight, when add = TRUE the new weight vector will be appended instead of replacing the old one
#when there's one weight, it's named "weight". When there're more than one, they're named "weight1", "weight2", ...
sampID = chgWt(c(1,3,1,3), sampID, catDic, updateDic = FALSE, add = TRUE)

#get the sample
sample = getModelData(data, sampID, sampName = "sample2", weightName = "weight")

#a wrapper for getting the sample and model on it
model(sampName = "sample2", data, sampID, formul, weightName = "weight1") 
#only on glm models

#on multiple samples and return the mean and sd of coefficients
model.mc(data, sampID, formul, weightName = "weight2", nSamps = 10)

#-----------------
#stratified sampling smbinning

#smbinning on one sample (weight is actually not used)
smb = getSmbin(data, sampID, "sample9", "weight1", y = "eventBinary", x = "fico_range_low")

#smbinning, repeating nSamps times
smb2 = sampSMBwrapper(data, vars = c("eventBinary", "flag_priordq"), 
               cellSize = 1200, nSamps = 7, y = "binevent", x = "annual_inc", col = "blue")

te = data[3000:5000, ]
smb3 = sampSMBontest(data, te, vars = c("eventBinary", "flag_priordq"), 
                       cellSize = 900, 
                       nSamps = 7, y = "binevent", x = "annual_inc")

#-----------------
#scoreTrend
data$issue_d = as.Date(data$issue_d)

scoreTrend(data, ContinuousVar = "fico_range_low", segVar = "grade", 
           segLev = "A", dateVar = "issue_d")

scoreTrend.seg(data, ContinuousVar = "fico_range_low", segVar = "grade", dateVar = "issue_d", 
               startDate = "2014-01-01", endDate = "2016-04-01", yLim = "fixed")




#------------------
#singleVar
singleVar.glm(binevent = "eventBinary", varname = "fico_range_low", data, threshold = 0.3)

singleVar.surv(binevent = "eventBinary", varname = "fico_range_low", 
               survobj = "survObj", mobevent = "event_time_all", data, threshold = 0.06)
  


#------------------
#subROC

m1 = glm(eventBinary ~ fico_range_low+inq_last_6mths+borrowerrate, data, family = "binomial")
subROC(model = m1, subsetVar = "flag_priordq", binevent = "eventBinary", data)
subROC.vec(prediction = predict(m1, data), subsetVar = "flag_priordq", binevent = "eventBinary", data)

formul = as.formula(survObj ~ strata(grade) + fico_range_low+inq_last_6mths+borrowerrate)
m2 = coxph(formul, data = data, model = TRUE)
subROC(model = m2, subsetVar = "flag_priordq", binevent = "eventBinary", data)
subROC.vec(prediction = predict(m2, data), subsetVar = "flag_priordq", binevent = "eventBinary", data)


#------------------
#varInfl
m1 = glm(eventBinary ~ fico_range_low+inq_last_6mths+borrowerrate, data, family = "binomial")
varInfl.one(m1, "inq_last_6mths", data)
varInfl.mod(m1, data)


#------------------
#summaryDataset
summary.dataset(data, conVarName = c("fico_range_low"), 
                strataName = c("grade"), nBins = 10)
#breaks


#------------------
#modeling
formul = as.formula(survObj ~ strata(grade) + fico_range_low+inq_last_6mths+borrowerrate)
modeling_descr(formul, data, seed = 12345, te.pc = 0.2, sepbins = "quantile", 
               binEvent = "eventBinary", mobEvent = "event_time_all", na.rm = TRUE, plotGroupname = "term")
  
modeling_ggsurv(formul, data, predOn = "test", quantil = c(0, 0.3, 0.7, 1),
                           na.rm = TRUE, te.pc = 0.2, seed = 12345)
  

#------------------
#incrScore
formul = as.formula(survObj ~ strata(grade) + fico_range_low+inq_last_6mths+borrowerrate)
incrScore(formul, "fico_range_low", data, quantil = c(0.05, 0.25, 0.5, 0.75, 0.95))
  