
# The last exercise concerns the comparison between two different ciphers. Take one instance of two different
# ciphers, (eg. 43 and 456 would give one 0 and one 1), compare the 10 first scores and see if you can spot a difference.

#overload blur function for non-blurred images.
smoothImage <- function(grayImg){ # This function is run in loadSinglePersonsData check the code
  kernel <- makeBrush(9, shape='Gaussian', step=TRUE, sigma=1) # There exist a number of different functions
  
  print(kernel) # just to show what we have made
  smoothed <- filter2(grayImg, kernel) # filter the image using the kernel
  return(smoothed)
}

id <- loadSinglePersonsData(100,1,1,folder)
id <- data.frame(id)
id$X1 <- factor(id$X1)

pca <- prcomp(id[,-1], scale = FALSE, center = TRUE )

diff <- pca$x[43,1:10] - pca$x[2853,1:10] # difference in scores, if close to 0 then they are pretty similar
print(diff)

# Try also this were you take the mean for all 400 instances of these ciphers and compare the
# first 10 scores. Can you spot a pattern when comparing with the loadings.

colMeans(pca$x[1:400,1:10]) # mean for zeroes
#apply(pca$x[1:400,1:10],2,sd) # stdv for zeroes

colMeans(pca$x[2801:3200,1:10]) # mean for eights
#apply(pca$x[2801:3200,1:10],2,sd) # stdv for eights

colMeans(pca$x[1:400,1:10])-colMeans(pca$x[2801:3200,1:10]) # difference of means


{
trunc <- colMeans(pca$x[3401:4000,1:10]) %*% t(pca$rotation[,1:10])
trunc <- scale(trunc, center = -1 * pca$center, scale=FALSE)

imageSize <- sqrt(ncol(id) - 1)
imageM <- matrix( trunc,nrow = imageSize,ncol = imageSize,byrow = FALSE)
imageM <- rotate(imageM,-90) # rotate is a function to rotate the image
image( imageM, col = gray(seq(0, 1, length = 256)) )
}

