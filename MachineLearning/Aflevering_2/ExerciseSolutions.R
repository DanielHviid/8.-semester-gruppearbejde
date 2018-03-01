# Exercise 1 Solution

# -------------------------------include sorce files   ------------------------------------------ 
#ubuntu
#source('~/Desktop/8.semester/Ex1/loadImage.R')
#source('~/Desktop/8.semester/Ex1/rExample.R')
# read image and perform knn
#folder <- "/home/daniel/Desktop/trunk/2018/group"

#windows
source('D:/Smartgit/8-semester/Ex1/loadImage.R')
source('D:/Smartgit/8-semester/Ex1/functions.R')
# read image and perform knn
folder <- "D:/SmartSVN/trunk/2018/group"

# -------------------------------- 1.4.1 K-Nearest Neighbour: --------------------------------------
{

# get group member data
dataList <- list(list(),  list(1,2,3) )
idList <- getAllData(dataList)
id <- rbind(idList[[1]], idList[[2]], idList[[3]])
id <- data.frame(id)
id$X1 <- factor(id$X1)

set.seed(423)
shuffledData <- id[sample(nrow(id)),]

print ("perform test on training data")
a <- nrow(id)/2
b <- nrow(id)
test_pred <- knn(shuffledData[1:a, -1], shuffledData[1:a, -1], shuffledData[1:a,1], k=1)
resultMatrix = CrossTable(x = shuffledData[1:a,1], y = test_pred, prop.chisq=FALSE, prop.r = FALSE, prop.c = FALSE, prop.t = FALSE);
sum(diag(resultMatrix$t))/sum(resultMatrix$t)

print("perform test on test data")
test_pred <- knn(train = shuffledData[1:a, -1], test = shuffledData[a:b, -1], cl = shuffledData[1:a,1], k=1)
resultMatrix = CrossTable(x = shuffledData[a:b,1], y = test_pred, prop.chisq=FALSE, prop.r = FALSE, prop.c = FALSE, prop.t = FALSE);
sum(diag(resultMatrix$t))/sum(resultMatrix$t)


}

# -------------------------------- 1.4.2 K-Nearest Neighbour: --------------------------------------



# folds <- createFolds(id$X1, k = 10)
# listOfFolders <- c(1:10)
# 
# for(i in 1:10)
# {
#   id_train <- id[-folds[[i]],-1]
#   id_test <- id[folds[[i]],-1]
#   
#   id_train_labels <- id[-folds[[i]],1]
#   id_test_labels <- id[folds[[i]],1]
#   
#   id_test_pred <- knn(train = id_train, test = id_test, cl = id_train_labels, k=1)
#   
#   listOfFolders[i] <- acc(id_test_pred,id_test_labels)
# }
# 
# print(listOfFolders)
# boxplot(listOfFolders,data=listOfFolders, main="10 Cross Test", xlab="", ylab="Recognition") 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# # Set a "list of list" for each group and member and run
# dataList <- list(list(),  list(1,2,3) )
# idList <- getAllData(dataList)
# 
# # You can now iterate trough the list
# for(i in 1:length(idList)){
#   idTemp <- idList[i]
#   idTemp <- data.frame(idTemp)
# }
# # you can combine the data frames using "id <- rbind(id, idTemp)"