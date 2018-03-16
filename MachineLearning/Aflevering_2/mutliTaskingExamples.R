library(foreach)
library(doParallel)

#setup parallel backend to use many processors
cores=detectCores()
cl <- makeCluster(cores[1]-1) #not to overload your computer
registerDoParallel(cl)


output <- foreach(j=1:10) %dopar% {
  cat("j-Vakues: ", j, "\n")
  start.time <- Sys.time()
  test_pred <- knn(train = reducedTraining, test = reducedTest, cl = trainingClasses, k=kValues[K])
  end.time <- Sys.time()
  timeSamples[j] <- end.time - start.time
  }


for (j in 1:10)
{
  
}
#stop cluster
stopCluster(cl)
