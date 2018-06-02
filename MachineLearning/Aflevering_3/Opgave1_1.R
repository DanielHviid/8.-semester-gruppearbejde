
#--------------------------------------------
#--------------- Opgave 3.1.1.1
#--------------------------------------------

input = id100Small

colors = c("black", "blue", "red", "yellow", "green", "cyan", "purple", "orange", "tan", "grey", "pink")

data <- disjunctSplit(input, Nfolds = 3)

NClusters = (1:10)*10
Nk = c(1,3,5,10,15)

training = data[[1]]
trainingClasses = training[,1]
print(typeof(trainingClasses))
trainingData = training[,-1]
test = data[[2]]
testClasses = test[,1]
testData = test[,-1]

clusteredAccuracy <- matrix(,5,10)
accuracy <- matrix(,5,1)

for (i in 1:5)
{
  predictions <- knn(train = trainingData, test = testData, cl = trainingClasses, k=Nk[i])
  
  result <- confusionMatrix(testClasses, predictions)
  accuracy[i] <- sum(diag(result$table))/sum(result$table)
  
  for (j in 1:10)
  {
    predictions <- clusterknn(training, test, NClusters[j], K = Nk[i])
    
    result <- confusionMatrix(testClasses, predictions)
    clusteredAccuracy[i,j] <- sum(diag(result$table))/sum(result$table)
  }
}

matplot(Nk, cbind(accuracy, clusteredAccuracy), "l", col = colors, ylim = c(0,1), xlim = c(1,21))
legend(x = "topright", 0.95, legend=c("No cluster", "Nclu 10", "Nclu 20", "Nclu 30", "Nclu 40", "Nclu 50", "Nclu 60", "Nclu 70", "Nclu 80", "Nclu 90", "Nclu 100"), lty=1:2, cex=0.8, col = colors)



#--------------------------------------------
#--------------- Opgave 3.1.1.2
#--------------------------------------------


input = id100Small

colors = c("black", "blue", "red", "yellow", "green", "cyan", "purple", "orange", "tan", "grey", "pink")

Ncross = 3

data <- disjunctCross(input, Nfolds = Ncross)

NClusters = (1:10)*10
Nk = c(1,3,5,10,15)

training = rbindlist(data[-2])
trainingClasses = unlist(training[,1])
print(typeof(trainingClasses))
trainingData = training[,-1]
test = data[[2]]
testClasses = test[,1]
testData = test[,-1]

clusteredAccuracy <- matrix(0,5,10)
accuracy <- matrix(0,5,1)
times <- matrix(0,5,11)

for (x in 1:Ncross)
{
  training = rbindlist(data[-x])
  trainingClasses = as.vector(training[,1])
  trainingData = training[,-1]
  test = data[[x]]
  testClasses = test[,1]
  testData = test[,-1]
  
  for (i in 1:5)
  {
    start.time <- Sys.time()
    predictions <- knn(train = trainingData, test = testData, cl = t(trainingClasses), k=Nk[i])
    end.time <- Sys.time()
    times[i,1] <- times[i,1] + (end.time - start.time) / Ncross
    
    result <- confusionMatrix(testClasses, predictions)
    accuracy[i] <- accuracy[i] + sum(diag(result$table))/sum(result$table) / Ncross
    
    for (j in 1:10)
    {
      start.time <- Sys.time()
      predictions <- clusterknn(training, test, NClusters[j], K = Nk[i])
      end.time <- Sys.time()
      times[i,(1+j)] <- times[i,(1+j)] + (end.time - start.time) / Ncross
      
      result <- confusionMatrix(testClasses, predictions)
      clusteredAccuracy[i,j] <- clusteredAccuracy[i,j] + sum(diag(result$table))/sum(result$table) / Ncross
    }
  }
}

matplot(Nk, cbind(accuracy, clusteredAccuracy), "l", col = colors, ylim = c(0,1), xlim = c(1,21))
legend(x = "topright", 0.95, legend=c("No cluster", "Nclu 10", "Nclu 20", "Nclu 30", "Nclu 40", "Nclu 50", "Nclu 60", "Nclu 70", "Nclu 80", "Nclu 90", "Nclu 100"), lty=1:2, cex=0.8, col = colors)


