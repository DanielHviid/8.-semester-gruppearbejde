#--------------------------------------------
#--------------- Opgave 2.1.1.1 del 1
#--------------------------------------------

folds = createFolds(id100Small)









#-------------------------------------------------------------
#Vertical split of test data
# y @ input dataframe
# outputData @ list(training, test, validation)
#-------------------------------------------------------------
trainingSplitVertical <- function(y, Nfolds = 10, NTestSize = 1, NValidationSize = 0){

  #Preconditions 
  if(nrow(y) %% Nfolds !=0){
    print("invallid input folds not a multible of imput array")
    return(list())
  }
  if (NTestSize <1 || NTestSize > Nfolds - 1){
    print("invallid input for Test set")
    return(list())
  }
  if (NValidationSize < 0 || NValidationSize > Nfolds - 1){
    print("invallid input for vallidation set")
    return(list())
  }
  if (NValidationSize + NTestSize >= Nfolds){
    print("invallid input out of bounce")
    return(list())
  }
    

  # split and shuffle 
  folds <- createFolds(y$X1, k = Nfolds)

  #construct the dataPackage for outputData
  TeFolds <- folds[[1:NTestSize]]
  VaFolds <- folds[[NTestSize:(NTestSize+NValidationSize)]]
  TrFolds <- folds[[1:(NTestSize+NValidationSize)]]
  
  test <- y[TeFolds,]
  validation <- y[VaFolds,]
  training <- y[-TrFolds,]
  
  
  
  if(NValidationSize == 0){
    print("test1")
    validation <- data.frame()
  }
  if(NTestSize == 0){
    test <- data.frame()
  }
  
  #return data  
  outputData = list(training, test, validation)
  
  
  return(outputData)
}


test <- trainingSplitVertical(id100Small, Nfolds = 10, NTestSize = 1, NValidationSize = 0)

nrow(test[[1]])
nrow(test[[2]])
nrow(test[[3]])


#-------------------------------------------------------------
#Vertical split of test data
# y @ input dataframe
# outputData @ list(training, test, validation)
#-------------------------------------------------------------
trainingSpliHorisontal <- function(y, NMembers = 20, Nfolds = 10, NTestSize = 1, NValidationSize = 0){
  
  return(outputData)
}





