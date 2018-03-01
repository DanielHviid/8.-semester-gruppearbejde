source('~/Desktop/8-semester/Ex1/loadImage.R')
source('~/Desktop/8-semester/Ex1/functions.R')
#source('~/Desktop/workspace/STML/workspace/exmaple code/loadImage.R')


# ------------------------------------------------------------------#
# -------------------------FUNCTIONS--------------------------------#
# ------------------------------------------------------------------#

# ------------------------------------------------------------------#
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
# ------------------------------------------------------------------#

# ------------------------------------------------------------------#
# Smoothing example code, this can be included in your code and will change the code so remember to 
# load the images again, ( A nice feature of R )
smoothImage <- function(grayImg){ # This function is run in loadSinglePersonsData check the code
  kernel <- makeBrush(9, shape='Gaussian', step=TRUE, sigma=0.8) # There exist a number of different functions
  #print(kernel) # just to show what we have made
  smoothed <- filter2(grayImg, kernel) # filter the image using the kernel
  return(smoothed)
}
# ------------------------------------------------------------------#


# ------------------------------------------------------------------#
# Example code for reading all images into a list, DPI 100
getAllData <- function(dataList){
  #id <- data.frame()
  idList <- list()
  for(i in 1:length(dataList))
  {
    if( length(dataList[[i]]) > 0  ){
      for(j in 1:length(dataList[[i]])){
        idTemp <- loadSinglePersonsData(100,i - 1,j,folder)
        idList <- append(idList, list(idTemp))
        cat("i: ", i, " j: ", j, "\n")
      }
    }
  }
  return(idList)
}
# ------------------------------------------------------------------#


# ------------------------------------------------------------------#
# -------------------------EXERCISES 1------------------------------#
# ------------------------------------------------------------------#

# ---------------------------1.4.1-----------------------------------
# read image and perform knn
#folder <- "/home/anders/Desktop/workspace/STML/trunk/2018/group"
folder <- "/home/daniel/Desktop/trunk/2018/group"


#id <- loadSinglePersonsData(100,1,2, folder)
#id <- data.frame(id)
#id$X1 <- factor(id$X1)


dataList <- list( list(), list(1,2,3), list(), list(), list(), list(), list(), list(), list(), list(), list(), list(), list(), list()  )
idList <- getAllData(dataList)

id <- rbind(idList[[1]],idList[[2]])
id <- rbind(id,idList[[3]])
id <- data.frame(id)

set.seed(423)
shuffledData <- id[sample(nrow(id)),]

test_pred <- knn(train = shuffledData[1:6000, -1], test = shuffledData[6000:12000, -1], cl = shuffledData[1:6000,1], k=1)

resultMatrix = CrossTable(x = shuffledData[6000:12000,1], y = test_pred, prop.chisq=FALSE, prop.t = FALSE, prop.r = FALSE, prop.c = FALSE)

sum(diag(resultMatrix$t))/sum(resultMatrix$t)



write.table(resultMatrix$t, "/home/anders/Desktop/workspace/STML/workspace/exercise1/resultMatrix1.txt", sep = "\t")

# ------------------------------------------------------------------#


# ---------------------------1.4.2-----------------------------------
# Performance of varying K: Show how performance ( speed and test recognition) changes with
# changing K.

speed <- list()
accuracy <- list()

# DO 1-P K SIZES IN OWN LOOP

k_values <- c(seq(from=1, to=9, by=1),seq(from=10, to=50, by=10))

# Looping for 10 -> 50 stepsize of 10.
for(j in 1:length(k_values)) {
  start.time <- Sys.time()
  
  test_pred <- knn(train = shuffledData[1:6000, -1], test = shuffledData[6000:12000, -1], cl = shuffledData[1:6000,1], k=k_values[j])
  end.time <- Sys.time()
  
  resultMatrix = CrossTable(x = shuffledData[6000:12000,1], y = test_pred, prop.chisq=FALSE, prop.c = FALSE, prop.r = FALSE, prop.t = FALSE)
  time.taken <- end.time - start.time
  cat('Accuracy',sum(diag(resultMatrix$t))/sum(resultMatrix$t))
  cat(' ')
  cat('k',j)
  cat(' ')
  cat('Time',time.taken)
  accuracy[j] <- sum(diag(resultMatrix$t))/sum(resultMatrix$t)
  speed[j] <- time.taken 
}


write.table(speed, "/home/anders/Desktop/workspace/STML/workspace/exercise1/resultMatrix2speed.txt", sep = "\t")
write.table(accuracy, "/home/anders/Desktop/workspace/STML/workspace/exercise1/resultMatrix2acc.txt", sep = "\t")


# Doing plots for accuracy and speed
plot(k_values,speed, "l",col = "blue",axes=FALSE,ann=FALSE, ylim = c(0,max(unlist(speed))))
axis(1, at=k_values)
axis(2, at=1:100)
box()
title(main="Perfomance Speed", col.main="black",font.main=4)
title(ylab="speed")
title(xlab= "K")


plot(k_values,accuracy, "l",col = "blue",axes=FALSE,ann=FALSE, ylim = c(0.5,1))
axis(1, at=k_values)
axis(2, at=seq(from=0.5, to=1, by=0.1))
box()
title(main="Perfomance Accuracy", col.main="black",font.main=4)
title(ylab="Accuracy")
title(xlab= "K")
# ------------------------------------------------------------------#


# ---------------------------1.4.3-----------------------------------
# Perform a cross validation with a 90% / 10% split with 10 runs. Report mean and standard deviation of the performance.
folds <- createFolds(id$X1, k = 10)
listOfFolders <- c(1:10)

for(i in 1:10){
  id_train <- id[-folds[[i]],-1]
  id_test <- id[folds[[i]],-1]
  
  id_train_labels <- id[-folds[[i]],1]
  id_test_labels <- id[folds[[i]],1]
  
  id_test_pred <- knn(train = id_train, test = id_test, cl = id_train_labels, k=1)
  
  listOfFolders[i] <- acc(id_test_pred,id_test_labels)
}

mean <- mean(listOfFolders)
std <- sd(listOfFolders)
cat('mean',mean)
cat('std deviation',std)
write.table(listOfFolders, "/home/anders/Desktop/workspace/STML/workspace/exercise1/resultMatrix3.txt", sep = "\t")
#print(listOfFolders)
#summary(listOfFolders)
#boxplot(listOfFolders,data=listOfFolders, main="10 Cross Test", xlab="", ylab="Recognition") 
# ------------------------------------------------------------------#


# ---------------------------1.4.4-----------------------------------
# Preprocessing: Apply one of the two smoothing functions to the to the images, instead of the  one implemented: 
# Perform again the steps in task 1.4.3 (Cross validation). Describe and analyze the results for one of the
# smoothing methods depending on the amount of smoothing.

mean <- list()
std <- list()

sigma_values <- c(seq(from=0.1, to=1, by=0.2), seq(1,3.5,0.5))

for(i in 1:length(sigma_values)){
  # Smoothing example code, this can be included in your code and will change the code so remember to 
  # load the images again, ( A nice feature of R )
  smoothImage <- function(grayImg){ # This function is run in loadSinglePersonsData check the code
    kernel <- makeBrush(9, shape='Gaussian', step=TRUE, sigma=sigma_values[i]) # There exist a number of different functions
    #print(kernel) # just to show what we have made
    smoothed <- filter2(grayImg, kernel) # filter the image using the kernel
    return(smoothed)
  }
  
  dataList <- list( list(), list(2), list(), list(), list(), list(), list(), list(), list(), list(), list(), list(), list(), list()  )
  idList <- getAllData(dataList)
  id <- idList[1]
  id <- data.frame(id)
  #for(k in 2:length(idList)){
  #  idTemp <- idList[k]
  #  idTemp <- data.frame(idTemp)
  #  id <- rbind(id, idTemp)
  #}
  

  folds <- createFolds(id$X1, k = 10)
  listOfFolders <- c(1:10)
  
  for(j in 1:10){
    id_train <- id[-folds[[j]],-1]
    id_test <- id[folds[[j]],-1]
    
    id_train_labels <- id[-folds[[j]],1]
    id_test_labels <- id[folds[[j]],1]
    
    id_test_pred <- knn(train = id_train, test = id_test, cl = id_train_labels, k=1)
    
    listOfFolders[j] <- acc(id_test_pred,id_test_labels)
  }
  
  mean[i] <- mean(listOfFolders)
  std[i] <- sd(listOfFolders)
  cat('i: ',i , '\t')
  cat('mean',mean[[i]], '\t')
  cat('std deviation',std[[i]], '\n')
}

plot(sigma_values,mean, "l",col = "blue",axes=FALSE,ann=FALSE, ylim = c(0,max(unlist(mean))))
axis(1, at=sigma_values)
axis(2, at=1:100)
box()
title(main="Mean Accuracy", col.main="black",font.main=4)
title(ylab="Mean")
title(xlab= "Sigma")

plot(sigma_values,std, "l",col = "blue",axes=FALSE,ann=FALSE, ylim = c(0,max(unlist(std))))
axis(1, at=sigma_values)
axis(2, at=0:5)
box()
title(main="Standard Deviation Accuracy", col.main="black",font.main=4)
title(ylab="Standard Deviation")
title(xlab= "Sigma")

write.table(mean, "/home/anders/Desktop/workspace/STML/workspace/exercise1/4mean.txt", sep = "\t")
write.table(std, "/home/anders/Desktop/workspace/STML/workspace/exercise1/4std.txt", sep = "\t")
# ------------------------------------------------------------------#


# ---------------------------1.4.5-----------------------------------
# Now try to apply k-nearest neighbor classification to the complete 
# data set from all students attending the course. Distinguish two cases: Having data from all individuals 
# in the training set and splitting the data according to individuals. Generate and explain the results.
# Clearing id
rm(id)

#Reset smoothing
smoothImage <- function(grayImg){ # This function is run in loadSinglePersonsData check the code
  kernel <- makeBrush(9, shape='Gaussian', step=TRUE, sigma=1) # There exist a number of different functions
  #print(kernel) # just to show what we have made
  smoothed <- filter2(grayImg, kernel) # filter the image using the kernel
  return(smoothed)
}


#---------- CASE 1 ---------------------------------------------------------------------------------------------
#----- Dividing vertical -----#
# Loading data
{
dataList <- list( list(), list(1,2,3), list(1,2,3), list(1,2,3), list(), list(1,2,3,4,5), list(), list(), list(1,2,3), list(1,2,3), list(), list(), list(), list()  )
idList <- getAllData(dataList)

id <- list()
for(i in 1:length(idList)){
  id <- rbind(id, data.frame( idList[i]))
}

folds <- createFolds(id$X1, k = 10)
listOfFolders5_1 <- c(1:10)

for(j in 1:10){
  cat("j: ", j, "\n")
  id_train <- id[-folds[[j]],-1]
  id_test <- id[folds[[j]],-1]
  
  id_train_labels <- id[-folds[[j]],1]
  id_test_labels <- id[folds[[j]],1]
  
  id_test_pred <- knn(train = id_train, test = id_test, cl = id_train_labels, k=1)
  
  listOfFolders5_1[j] <- acc(id_test_pred,id_test_labels)
}

mean5_1 <- mean(listOfFolders5_1)
std5_1 <- sd(listOfFolders5_1)
cat('mean5_1',mean5_1, '\t')
cat('std deviation 5_1',std5_1, '\n')


#---------- CASE 2 -----------#
#---- Dividing horizontal ----#
# Using 2 person as test data, and 18 as training

# dataList <- list( list(), list(1))
# idList<- getAllData(dataList)
# idListOld <- idList
# id <- data.frame(idList)
# id <- id[sample(nrow(id)),]

# dataList <- list( list(), list(1,2,3), list(1,2,3), list(1,2,3), list(), list(1,2,3,4,5), list(), list(), list(1,2,3), list(1,2,3), list(), list(), list(), list()  )
# idList <- getAllData(dataList)

# id <- list()
# for(i in 1:length(idList)){
#   id <- rbind(id, data.frame( idList[i]))
# }

listOfFolders5_2_10 <- c(1:10)
for(j in 0:9){
  cat("j: ", j, "\n")
  N <- nrow(id)/10
  a <- N*j+1
  b <- N*(j+1)
  id_test <- id[a:b,-1]
  id_train <- id[-(a:b),-1]
  
  id_test_labels <- id[a:b,1]
  id_train_labels <- id[-(a:b),1]

  id_test_pred <- knn(id_train, id_test, id_train_labels, 10)
  
  listOfFolders5_2_10[j+1] <- acc(id_test_pred,id_test_labels)

}

mean5_2 <- mean(listOfFolders5_2_10)
std5_2 <- sd(listOfFolders5_2_10)
cat('mean5_1',mean5_2, '\t')
cat('std deviation 5_1',std5_2, '\n')
}

# ------------------------------------------------------------------#


# ---------------------------1.4.6-----------------------------------
# Lastly report on timing of the prediction step for varying k and 
# using a small and large dataset. You don’t have to test every K simply give an overview on the 
# performance’s dependency on smaller and larger K and dataset. Discuss how the performance changes 
# with different sizes of the dataset, is K dependent on the dataset size?


# ------ Small data-set ------ #

#load data
dataList1 <- list( list(), list(1,2,3), list(), list(), list(), list(), list(), list(), list(), list(), list(), list(), list(), list()  )
idList1 <- getAllData(dataList1)

dataList2 <- list( list(), list(1,2,3), list(1,2,3), list(1,2,3), list(), list(1,2,3,4,5), list(), list(), list(1,2,3), list(1,2,3), list(), list(), list(), list()  )
idList2 <- getAllData(dataList2)

#format data
id1 <- list()
id2 <- list()
for(i in 1:length(idList1)){
  id1 <- rbind(id1, data.frame( idList1[i]))
}
for(i in 1:length(idList2)){
  id2 <- rbind(id2, data.frame(idList2[i]))
}

shuffledData1 <- id1[sample(nrow(id1)),]
shuffledData2 <- id2[sample(nrow(id2)),]

# knn performance test 
k_vals = c(seq(1,9,1),seq(10,50,10))

for(i in k_vals){
  start.time <- Sys.time()
  test_pred <- knn(train = shuffledData[1:2000, -1], test = shuffledData[2000:4000, -1], cl = shuffledData[1:2000,1],k_val)
  end.time <- Sys.time()
  time.taken <- end.time - start.time
  cat('Time',time.taken)
}

# ----- Large Data-set ------#



id <- list()
for(i in 1:length(idList)){
  id <- rbind(id, data.frame( idList[i]))
}
shuffledData <- id[sample(nrow(id)),]

for(i in seq(from=1, to=200, by=20)){
  
  start.time <- Sys.time()

  test_pred <- knn(train = shuffledData[1:72000, -1], test = shuffledData[72000:80000, -1], cl = shuffledData[1:72000,1], k=i)
  
  end.time <- Sys.time()
  time.taken <- end.time - start.time
  cat('Time',time.taken)
}


