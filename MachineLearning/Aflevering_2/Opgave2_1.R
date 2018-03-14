
#--------------------------------------------
#--------------- Opgave 2.2.1 a - pre normalization
#--------------------------------------------

data = allPersonsInCross(id100Small)

listOfAccuracies <- matrix(,1,10)
listOfNPCs <- matrix(,1,10)

K = 3

for (i in 1:10)
{
  training = rbindlist(data[-i])
  test = data[[i]]
  trainingClasses = training[,1]
  trainingData = znormalize(training[,-1])
  testClasses = test[,1]
  testData = znormalize(test[,-1])
  
  model <- prcomp(trainingData)
  
  leastVariance = 0.95

  variance = model$sdev
  variance = variance / sum(variance)
  
  cumulativeVariance = cumsum(variance)
  
  acceptableFactorVariances = variance[1:(length(cumulativeVariance[cumulativeVariance < leastVariance])+1)]
  NAcceptable = length(acceptableFactorVariances)
  listOfNPCs[i] = NAcceptable
  
  reducedTraining <- data.frame(data.matrix(trainingData) %*% data.matrix(model$rotation[,1:NAcceptable]))
  reducedTest <- data.frame(data.matrix(testData) %*% data.matrix(model$rotation[,1:NAcceptable]))
  
  cat("Number of principle components: ", NAcceptable, "\n")

  test_pred <- knn(train = reducedTraining, test = reducedTest, cl = t(trainingClasses), k=K)

  listOfAccuracies[i] <- acc(test_pred,testClasses)
}


write(listOfAccuracies,"2-1-a-accuracies.txt")
write(listOfNPCs,"2-1-a-npcs.txt")


#--------------------------------------------
#--------------- Opgave 2.2.1 b - post normalization
#--------------------------------------------

data = allPersonsInCross(id100Small)


listOfAccuracies <- matrix(,1,10)
listOfNPCs <- matrix(,1,10)

K = 3

for (i in 1:10)
{
  training = rbindlist(data[-i])
  test = data[[i]]
  trainingClasses = training[,1]
  trainingData = training[,-1]
  testClasses = test[,1]
  testData = test[,-1]
  
  model <- prcomp(trainingData)
  
  leastVariance = 0.95
  
  variance = model$sdev
  variance = variance / sum(variance)
  
  cumulativeVariance = cumsum(variance)
  
  acceptableFactorVariances = variance[1:(length(cumulativeVariance[cumulativeVariance < leastVariance])+1)]
  NAcceptable = length(acceptableFactorVariances)
  listOfNPCs[i] = NAcceptable
  
  reducedTraining <- znormalize(data.frame(data.matrix(trainingData) %*% data.matrix(model$rotation[,1:NAcceptable])))
  reducedTest <- znormalize(data.frame(data.matrix(testData) %*% data.matrix(model$rotation[,1:NAcceptable])))
  
  cat("Number of principle components: ", NAcceptable, "\n")
  
  test_pred <- knn(train = reducedTraining, test = reducedTest, cl = t(trainingClasses), k=K)
  
  listOfAccuracies[i] <- acc(test_pred,testClasses)
}


write(listOfAccuracies,"2-1-b-accuracies.txt")
write(listOfNPCs,"2-1-b-npcs.txt")



