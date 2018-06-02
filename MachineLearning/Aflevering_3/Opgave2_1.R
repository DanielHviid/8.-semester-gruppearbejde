
#--------------------------------------------
#--------------- Opgave 3.2.1 
#--------------------------------------------

input = id100Small
data = classSplit(input)

subData = c()
classes = c()

for (i in 1:10)
{
  cipher = data[[i]]
  subData <- rbind(subData, cipher[1:5,-1])
  classes <- c(classes, list(i-1,i-1,i-1,i-1,i-1))
}

clusters <- hclust(dist(subData))

plot(clusters, labels = labels)

#--------------------------------------------
#--------------- Opgave 3.2.2
#--------------------------------------------

input = id100Small
clusters = cipherClusters(input, 5)

classes = c()

for (i in 1:10)
{
  classes <- c(classes, list(i-1,i-1,i-1,i-1,i-1))
}

clusters <- hclust(dist(clusters))

plot(clusters, labels = labels)