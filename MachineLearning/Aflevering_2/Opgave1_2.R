
#--------------------------------------------
#--------------- Opgave 2.1.1.2
#--------------------------------------------

data = trainingSplitVertical(id100Small)
training = data[[1]]
length(training)

model <- prcomp(id100Small, center = TRUE, scale = TRUE)

plot(model$sdev[1:20], type = "o")

variance <- model$sdev^2
variance <- variance / sum(variance)
plot(variance[1:20], ylim = c(0,1), type = "o")

cumulativeVariance <- cumsum(variance)
plot(cumulativeVariance[1:20], ylim = c(0,1), type = "o")

