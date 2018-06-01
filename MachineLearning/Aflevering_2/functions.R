rotateSelf <- function(x) t(apply(x, 2, rev))

#This file contains 2 functions and some example code in the bottom
#for using the 2 functions.
#The example code use the functions to load and smoothen the image data.
#lastly it converts the data into a table more suitable for R based classification 

#-------------------------------------------------------------
#Smoothing function (you are welcome to use alternative functions from R)
#-------------------------------------------------------------
smoothImage <- function(grayImg){
  #define matrix for convolution
  kernel <- matrix( 
    1, # the data elements 
    3, # number of rows 
    3)
  kernel <- kernel/9

  
  #using r library for smoothing
  smoothed <- filter2(grayImg, kernel)

  return(smoothed)
}


#-------------------------------------------------------------
#Data loading function.
#This function load a single dataset where the data is
#specified by the DPI of the scanned images, the group number of the person
#and the number of the group member.
#Then the function use the smoothing function above, in order to smoothen the data.
#The function then places a grid around the digit cells, based on the corner file,
#so that it can be verified the data is read in a reasonable maner.
#Lastly a vector of table is returned. Each table in this vector represents
#a single type of digit, so the first table is only 0's, the second is 1's and so on.
#In the tables the rows represent the individual handwritten digits.
#The columns represents the pixel values.
#-------------------------------------------------------------
loadSinglePersonsData <- function(DPI,groupNr,groupMemberNr,folder){
  #load the scaned images
  ciffers <- list(readJPEG(paste(c(folder,groupNr,"/member",groupMemberNr,"/Ciphers",DPI,"-0.jpeg"), collapse = "")),
                  readJPEG(paste(c(folder,groupNr,"/member",groupMemberNr,"/Ciphers",DPI,"-1.jpeg"), collapse = "")),
                  readJPEG(paste(c(folder,groupNr,"/member",groupMemberNr,"/Ciphers",DPI,"-2.jpeg"), collapse = "")),
                  readJPEG(paste(c(folder,groupNr,"/member",groupMemberNr,"/Ciphers",DPI,"-3.jpeg"), collapse = "")),
                  readJPEG(paste(c(folder,groupNr,"/member",groupMemberNr,"/Ciphers",DPI,"-4.jpeg"), collapse = "")))
  
  #load the corner values
  corners <- read.csv(paste(c(folder,groupNr,"/member",groupMemberNr,"/Corners.txt"), collapse = ""))
  corners <- trunc(corners*DPI/300)
  #print(corners)
  
  #define lists to be used
  #  gray <- list(1:5)
  #  smoothed <- list(1:5)
  prepared <- list(1:5)
  
  
  
  
  #convert the images to gray scale.
  for(i in 1:5)
  {
    if( length(dim(ciffers[[1]]) ) == 3 ) {
      r <-ciffers[[i]][,,1]
      g <-ciffers[[i]][,,2]
      b <-ciffers[[i]][,,3]
      prepared[[i]] <- (r+g+b)/3
    } else {
      prepared[[i]] <- ciffers[[i]]
    }  
  }
  
  #smooth images based on the funtion in the top
  for(i in 1:5)
  {
    prepared[[i]] <- smoothImage(prepared[[i]])
  }  
  
  # Rotate image to correct orientation
  for( i in 1:5){
    if( dim(prepared[[i]])[2] > dim(prepared[[i]])[1] ){  
      prepared[[i]] <- rotateSelf(prepared[[i]]) #   EBImage::rotate(prepared[[i]])
    }
  }
  
  #extract individual ciffers
  #xStep and yStep is used to ensure the first corner of the
  #individual ciffers are placed fairly accurate
  xStep  <- (corners[1,7]-corners[1,1])/20;
  yStep  <- (corners[1,8]-corners[1,2])/20;
  
  #xStepT and yStepT is used to ensure that the feature vectors
  #from all people have the same size.
  
  xStepT <- 60*DPI/300
  yStepT <- 60*DPI/300
  
  dataMatrix <- matrix(1:((xStepT-2)*(yStepT-2) + 1)*10*20*20, nrow=10*20*20, ncol=(xStepT-2)*(yStepT-2) + 1)
  
  for(pages in 1:5)
  {
    for(box in 1:2)
    {
      for(cifX in 1:20)
      {
        aXbase <- corners[(pages-1)*2 + box,1] + xStep*(cifX-1)
        for(cifY in 1:20)
        {
          aYbase <- corners[(pages-1)*2 + box,2] + yStep*(cifY-1)
          
          dataMatrix[((pages-1)*2 + box - 1)*20*20 + (cifY-1)*20 + cifX ,1 ] <- (pages-1)*2 + box - 1
          
          for(px in 1:(xStepT-2))
          {
            for(py in 1:(yStepT-2))
            {
              #here things are read in
              dataMatrix[((pages-1)*2 + box - 1)*20*20 + (cifY-1)*20 + cifX ,1 + (px-1)*(yStepT-2) + py ] <- prepared[[pages]][aYbase+py+1,aXbase+px+1]
              
            }
          }
        }
      }
    }
  }
  
  return(dataMatrix)
}


# Accuracy code, set labels and prediction as input, complicated method
acc <- function(x, y) {
  accu = 0
  for(i in 1:length(x))
  {
    if( x[i] == y[i] )
    {
      accu <- accu + 1;
    }
  }
  return(100*accu/length(y))
}

# Smoothing example code, this can be included in your code and will change the code so remember to 
# load the images again, ( A nice feature of R )
smoothImage <- function(grayImg){ # This function is run in loadSinglePersonsData check the code
  kernel <- makeBrush(9, shape='Gaussian', step=TRUE, sigma=1.8) # There exist a number of different functions
  
  #print(kernel) # just to show what we have made
  smoothed <- filter2(grayImg, kernel) # filter the image using the kernel
  return(smoothed)
}




# code for reading all images into a list
getAllData <- function(dataList, DPI){
  id <- data.frame()
  idList <- list()
  # foreach(i=1:10) %dopar% {
  #   
  #   #loop contents here
  #   
  # }
  for(i in 1:length(dataList))
  {
    if( length(dataList[[i]]) > 0  ){
      for(j in 1:length(dataList[[i]])){
        cat("i: ", i, "j: ", j, "\n")
        idTemp <- loadSinglePersonsData(DPI,i - 1,j,folder)
        idList <- append(idList, list(idTemp))
      }
    }
  }
  id <- list()
  for(i in 1:length(idList)){
    id <- rbind(id, data.frame( idList[i]))
  }
  
  return(id)
}

#-------------------------------------------------------------
# Get the number of principle components required to account forat least the level of variance selected (0 to 1).'
# Returns a number, input is : (matrix/dataframe, number)
#-------------------------------------------------------------
getNumberOfPrincipleComponents <- function(model, leastVariance)
{
  variance <- model$sdev^2
  variance <- variance / sum(variance)
  
  cumulativeVariance <- cumsum(variance)
  
  acceptableFactorVariances <- variance[1:(length(cumulativeVariance[cumulativeVariance < leastVariance])+1)]
  return(length(acceptableFactorVariances))
}

#-------------------------------------------------------------
# Get the rotation matrix corrosponding to at least the level of variance selected (0 to 1).'
# Returns a matrix, input is : (matrix/dataframe , number)
#-------------------------------------------------------------
getAcceptableVarianceRotationalMatrix <- function(input, leastVariance)
{
  model = prcomp(input)
  
  variance = model$sdev
  variance = variance / sum(variance)
  
  cumulativeVariance = cumsum(variance)
  
  acceptableFactorVariances = variance[1:(length(cumulativeVariance[cumulativeVariance < leastVariance])+1)]
  NAcceptable = length(acceptableFactorVariances)
  
  
  return (model$rotation[,1:NAcceptable])
  
}

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

#-------------------------------------------------------------
# splits every person into test, training and validation
#-------------------------------------------------------------
allPersonsInSplit <- function(input, Nfolds = 10, NTestSize = 1, NValidationSize = 0)
{
  testData <- input[sample(nrow(input)),]
  
  N = length(testData[,1])
  testIndex = NTestSize*N/Nfolds
  validationIndex = testIndex + NValidationSize*N/Nfolds
  test <- testData[1:testIndex,]
  if (NValidationSize != 0)
  {
    validation <- testData[(testIndex+1):validationIndex,]
  } else
  {
    validation <- NULL
  }
  
  training <- testData[-(1:validationIndex),]
  
  return(list(training, test, validation))
}

#-------------------------------------------------------------
# Creates a split of data to be used with cross validation
#-------------------------------------------------------------
allPersonsInCross <- function(input, Nfolds = 10)
{
  testData <- input[sample(nrow(input)),]
  
  result = list()
  
  N = length(testData[,1])
  
  
  
  for (i in 1:Nfolds)
  {
    start = N/Nfolds * (i-1) + 1
    end = N/Nfolds * i
    result[[i]] = testData[start:end,]
  }
  
  return(result)
}

#-------------------------------------------------------------
# splits persons into test, training and validation
#-------------------------------------------------------------
disjunctSplit <- function(input, Nfolds = 10, NTestSize = 1, NValidationSize = 0)
{
  testData <- input[sample(nrow(input)),]
  
  N = length(testData[,1])
  testIndex = NTestSize*N/Nfolds
  validationIndex = testIndex + NValidationSize*N/Nfolds
  test <- testData[1:testIndex,]
  if (NValidationSize != 0)
  {
    validation <- testData[(testIndex+1):validationIndex,]
  } else
  {
    validation <- NULL
  }
  
  training <- testData[-(1:validationIndex),]
  
  return(list(training, test, validation))
}


#-------------------------------------------------------------
# splits persons into cross validations
#-------------------------------------------------------------
disjunctCross <- function(input, Nfolds = 10)
{
  testData <- input[sample(nrow(input)),]
  
  result = list()
  
  N = length(testData[,1])
  
  for (i in 1:Nfolds)
  {
    start = N/Nfolds * (i-1) + 1
    end = N/Nfolds * i
    result[[i]] = testData[start:end,]
  }
  
  return(result)
  
}


#-------------------------------------------------------------
# shuffle person position in data
#-------------------------------------------------------------
shufflePersons <- function(input, Npersons = length(input[,1])/4000)
{
  result = list()
  
  N = length(testData[,1])
  
  for (i in 1:Npersons)
  {
    start = N/Npersons * (i-1) + 1
    end = N/Npersons * i
    result[[i]] = testData[start:end,]
  }
  
  result = rbindlist(sample(result))
  
  return(result)
}


#-------------------------------------------------------------
# Apply znorm on dataframe
#-------------------------------------------------------------
znormalize <- function(input)
{
  result = data.matrix(input)
  cols = ncol(result)
  rows = nrow(result)
  
  return (data.frame(matrix(znorm(result),rows,cols)))
}

