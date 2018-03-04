
#--------------------------------------------
#--------------- Opgave 2.1.1.1 del 1
#--------------------------------------------



## signs are random
require(graphics)

## the variances of the variables in the
## USArrests data vary by orders of magnitude, so scaling is appropriate
prcomp(USArrests)  # inappropriate
prcomp(USArrests, scale = TRUE, center = TRUE)
prcomp(~ Murder + Assault + Rape, data = USArrests, scale = TRUE)
plot(prcomp(USArrests))
summary(prcomp(USArrests, scale = TRUE))
biplot(prcomp(USArrests, scale = TRUE))





#--------------------------------------------
#--------------- Opgave 2.1.1.1 del 2
#--------------------------------------------
