

# Plotting thre first 10 eigenvectors as images for 1 person.

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

for(i in 1:10) {
  imageSize <- sqrt(ncol(id) - 1)
  imageM <- matrix( pca$rotation[,i],nrow = imageSize,ncol = imageSize,byrow = FALSE)
  imageM <- rotate(imageM,-90) # rotate is a function to rotate the image
  image( imageM, col = gray(seq(0, 1, length = 256)))
 # print(imageM)
}

idmean <- colMeans(id[1:400,-1])
imageM <- matrix( idmean,nrow = imageSize,ncol = imageSize,byrow = FALSE)
imageM <- rotate(imageM,-90) # rotate is a function to rotate the image
image( imageM , col = gray(seq(0, 1, length = 256)))
