
#--------------------------------------------
#--------------- Opgave 2.1.1.2 a - all persons in
#--------------------------------------------

data = allPersonsInSplit(id100Large)

training = data[[1]]
trainingClasses = training[,1]
trainingData = training[,-1]
test = data[[2]]
testClasses = test[,1]
testData = test[,-1]

model <- prcomp(trainingData)

leastVariances = list(0.8,0.9,0.95,0.99)

listOfAccuracies <- matrix(,4,3)
listOfTimes <- matrix(,4,3)
listOfTimeVariation <- matrix(,4,3)
listOfNPCs <- matrix(,4,1)
timeSamples <- matrix(,10,1)

for (i in 1:length(leastVariances))
{
  variance = model$sdev
  variance = variance / sum(variance)
  
  cumulativeVariance = cumsum(variance)
  
  acceptableFactorVariances = variance[1:(length(cumulativeVariance[cumulativeVariance < leastVariances[i]])+1)]
  NAcceptable = length(acceptableFactorVariances)
  listOfNPCs[i] = NAcceptable
  
  reducedTraining <- data.frame(data.matrix(trainingData) %*% data.matrix(model$rotation[,1:NAcceptable]))
  reducedTest <- data.frame(data.matrix(testData) %*% data.matrix(model$rotation[,1:NAcceptable]))
  
  cat("Number of principle components: ", NAcceptable, "\n")
  kValues <- list(1,10,100)
  for (K in 1:length(kValues))
  {
    cat("K-Vakues: ", kValues[[K]], "\n")
    
     
    for (j in 1:10)
    {
      cat("j-Vakues: ", j, "\n")
      start.time <- Sys.time()
      test_pred <- knn(train = reducedTraining, test = reducedTest, cl = trainingClasses, k=kValues[K])
      end.time <- Sys.time()
      timeSamples[j] <- end.time - start.time
    }
    
    listOfTimes[i,K] <- mean(timeSamples)
    listOfTimeVariation[i,K] <- sd(timeSamples)
    
    #resultMatrix = CrossTable(x = testClasses, y = test_pred, prop.chisq=FALSE, prop.r = FALSE, prop.c = FALSE, prop.t = FALSE);
    #result = sum(diag(resultMatrix$t))/sum(resultMatrix$t)
    
    listOfAccuracies[i,K] <- acc(test_pred,testClasses)
  }
  
}

print(listOfAccuracies)
write(listOfAccuracies,"1-2-a-accuracies.txt")
write(listOfTimes,"1-3-a-times-means.txt")
write(listOfTimeVariation,"1-2-a-times-stds.txt")
write(listOfNPCs,"1-2-a-NPCs.txt")



#--------------------------------------------
#--------------- Opgave 2.1.1.2 b - disjunct
#--------------------------------------------

data = disjunctSplit(id100Large)

training = data[[1]]
trainingClasses = training[,1]
trainingData = training[,-1]
test = data[[2]]
testClasses = test[,1]
testData = test[,-1]

model <- prcomp(trainingData)

leastVariances = list(0.8,0.9,0.95,0.99)

listOfAccuracies <- matrix(,4,3)
listOfTimes <- matrix(,4,3)
listOfTimeVariation <- matrix(,4,3)
listOfNPCs <- matrix(,4,1)
timeSamples <- matrix(,10,1)

for (i in 1:length(leastVariances))
{
  variance = model$sdev
  variance = variance / sum(variance)
  
  cumulativeVariance = cumsum(variance)
  
  acceptableFactorVariances = variance[1:(length(cumulativeVariance[cumulativeVariance < leastVariances[i]])+1)]
  NAcceptable = length(acceptableFactorVariances)
  listOfNPCs[i] = NAcceptable
  
  reducedTraining <- data.frame(data.matrix(trainingData) %*% data.matrix(model$rotation[,1:NAcceptable]))
  reducedTest <- data.frame(data.matrix(testData) %*% data.matrix(model$rotation[,1:NAcceptable]))
  
  cat("Number of principle components: ", NAcceptable, "\n")
  kValues <- list(1,10,100)
  for (K in 1:length(kValues))
  {
    cat("K-Vakues: ", kValues[[K]], "\n")
    for (j in 1:10)
    {
      cat("j-Vakues: ", j, "\n")
      start.time <- Sys.time()
      test_pred <- knn(train = reducedTraining, test = reducedTest, cl = trainingClasses, k=kValues[K])
      end.time <- Sys.time()
      timeSamples[j] <- end.time - start.time
    }
    
    listOfTimes[i,K] <- mean(timeSamples)
    listOfTimeVariation[i,K] <- sd(timeSamples)
    
    #resultMatrix = CrossTable(x = testClasses, y = test_pred, prop.chisq=FALSE, prop.r = FALSE, prop.c = FALSE, prop.t = FALSE);
    #result = sum(diag(resultMatrix$t))/sum(resultMatrix$t)
    
    listOfAccuracies[i,K] <- acc(test_pred,testClasses)
  }
  
}

write(listOfAccuracies,"1-2-b-accuracies.txt")
write(listOfTimes,"1-3-b-times-means.txt")
write(listOfTimeVariation,"1-2-b-times-stds.txt")
write(listOfNPCs,"1-2-b-NPCs.txt")

