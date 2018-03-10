

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


for (i in c(1,401,801,1201,1601,2001,2401,2801,3201,3601)){
  trunc <- pca$x[i,1:getNumberOfPrincipleComponents(pca,0.80)] %*% t(pca$rotation[,1:getNumberOfPrincipleComponents(pca,0.80)])
  trunc <- scale(trunc, center = -1 * pca$center, scale=FALSE)
  
  imageSize <- sqrt(ncol(id) - 1)
  imageM <- matrix( trunc,nrow = imageSize,ncol = imageSize,byrow = FALSE)
  imageM <- rotate(imageM,-90) # rotate is a function to rotate the image
  image( imageM, col = gray(seq(0, 1, length = 256)) )
}
