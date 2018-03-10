
#--------------------------------------------
#--------------- Opgave 2.1.1.1 del 1
#--------------------------------------------
# 
# N = 10
# 
# dataList <- trainingSplit(data,N,1,0,true) # number of folds, size of test set, size of validation set, shuffle
# 
# training <- dataList[1]
# test <- dataList[2]
# 
# model <- prComp(training)
# model.sdev
# bitplot(model)

maxVariance <- 0.8
maxfactors <- 2

#-------------------------------------------------------------
# Get the rotation matrix corrosponding to at least the level of variance selected (0 to 1).'
# Returns a matrix, input is : (matrix/dataframe , number)
#-------------------------------------------------------------
getAcceptableVarianceRotationalMatrix <- function(data, leastVariance)
{
  model <- prcomp(data, scale = TRUE, center = TRUE)
  
  variance <- model$sdev
  variance <- variance / sum(variance)
  
  cumulativeVariance <- cumsum(variance)
  
  acceptableFactorVariances <- variance[1:(length(cumulativeVariance[cumulativeVariance < leastVariance])+1)]
  NAcceptable <- length(acceptableFactorVariances)
  
  
  return (model$rotation[,1:NAcceptable])
  
}


#res <- prcomp(id100Small)
#summary(res)

# giver åbenbart et andet resultat
#summary(prcomp(id200Small, scale = TRUE))

## signs are random
require(graphics)

## the variances of the variables in the
## USArrests data vary by orders of magnitude, so scaling is appropriate
# prcomp(USArrests)  # inappropriate
model <- prcomp(USArrests, scale = TRUE, center = TRUE)
model
model$sdev
model[1]
model$rotation

# prcomp(~ Murder + Assault + Rape, data = USArrests, scale = TRUE)
# plot(prcomp(USArrests))
# summary(prcomp(id100Small, scale = TRUE))
# biplot(prcomp(USArrests, scale = TRUE))

variance <- model$sdev
variance <- variance / sum(variance)
cumulativeVariance <- cumsum(variance)
bestFactorVariances <- variance[1:maxfactors]
acceptableFactorVariances <- variance[1:(length(cumulativeVariance[cumulativeVariance < maxVariance])+1)]
NAcceptable <- length(acceptableFactorVariances)

plot(cumsum(acceptableFactorVariances), ylim = c(0,1), type = "o")
  
getAcceptableVarianceRotationalMatrix(USArrests, 0.8)


#--------------------------------------------
#--------------- Opgave 2.1.1.1 del 2
#--------------------------------------------
