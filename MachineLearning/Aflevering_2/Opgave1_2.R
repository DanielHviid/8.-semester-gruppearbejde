
#--------------------------------------------
#--------------- Opgave 2.1.1.2
#--------------------------------------------

# for (data in list(allPersonsInSplit(id100Small), disjunctSplit(id100Small)))
# {
#   
# }


data = allPersonsInSplit(id100Small)

training = data[[1]]
trainingClasses = training[,1]
trainingData = training[,-1]
test = data[[2]]
testClasses = test[,1]
testData = test[,-1]

model <- prcomp(trainingData)

leastVariances = list(0.8,0.9,0.95,0.99)

for (i in 1:length(leastVariances))
{
  variance = model$sdev
  variance = variance / sum(variance)
  
  cumulativeVariance = cumsum(variance)
  
  acceptableFactorVariances = variance[1:(length(cumulativeVariance[cumulativeVariance < leastVariances[i]])+1)]
  NAcceptable = length(acceptableFactorVariances)

  reducedTraining <- data.matrix(training) %*% data.matrix(model$rotation[,1:NAcceptable])
 
}
# 
# getAcceptableVarianceRotationalMatrix(model, 0)
# 
# plot(model$sdev, type = "o")
# 
# variance <- model$sdev^2
# variance <- variance / sum(variance)
# plot(variance, ylim = c(0,1), type = "o")
# 
# cumulativeVariance <- cumsum(variance)
# plot(cumulativeVariance, ylim = c(0,1), type = "o")

