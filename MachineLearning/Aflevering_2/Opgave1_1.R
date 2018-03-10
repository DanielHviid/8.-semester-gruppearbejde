
#--------------------------------------------
#--------------- Opgave 2.1.1.1
#--------------------------------------------

model <- prcomp(id100Small, center = TRUE, scale = TRUE)

plot(model$sdev[1:20], type = "o")

variance <- model$sdev^2
variance <- variance / sum(variance)
plot(variance[1:20], ylim = c(0,1), type = "o")

cumulativeVariance <- cumsum(variance)
plot(cumulativeVariance[1:20], ylim = c(0,1), type = "o")




# 
# # 
# # N = 10
# # 
# # dataList <- trainingSplit(data,N,1,0,true) # number of folds, size of test set, size of validation set, shuffle
# # 
# # training <- dataList[1]
# # test <- dataList[2]
# # 
# # model <- prComp(training)
# # model.sdev
# # bitplot(model)
# maxVariance <- 0.8
# maxfactors <- 2
# 
# 
# 
# #res <- prcomp(id100Small)
# #summary(res)
# 
# # giver åbenbart et andet resultat
# #summary(prcomp(id200Small, scale = TRUE))
# 
# ## signs are random
# require(graphics)
# 
# ## the variances of the variables in the
# ## USArrests data vary by orders of magnitude, so scaling is appropriate
# # prcomp(USArrests)  # inappropriate
# model <- prcomp(USArrests, scale = TRUE, center = TRUE)
# model
# model$sdev
# model[1]
# model$rotation
# 
# # prcomp(~ Murder + Assault + Rape, data = USArrests, scale = TRUE)
# # plot(prcomp(USArrests))
# # summary(prcomp(id100Small, scale = TRUE))
# # biplot(prcomp(USArrests, scale = TRUE))
# 
# variance <- model$sdev
# variance <- variance / sum(variance)
# cumulativeVariance <- cumsum(variance)
# bestFactorVariances <- variance[1:maxfactors]
# acceptableFactorVariances <- variance[1:(length(cumulativeVariance[cumulativeVariance < maxVariance])+1)]
# NAcceptable <- length(acceptableFactorVariances)
# 
# plot(cumsum(acceptableFactorVariances), ylim = c(0,1), type = "o")
#   
# getAcceptableVarianceRotationalMatrix(USArrests, 0.8)

