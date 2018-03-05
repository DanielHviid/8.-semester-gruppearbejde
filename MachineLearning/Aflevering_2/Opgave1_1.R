
#--------------------------------------------
#--------------- Opgave 2.1.1.1 del 1
#--------------------------------------------

res <- prcomp(id100Small)
summary(res)

# giver åbenbart et andet resultat
summary(prcomp(id200Small, scale = TRUE))

## signs are random
require(graphics)

## the variances of the variables in the
## USArrests data vary by orders of magnitude, so scaling is appropriate
prcomp(USArrests)  # inappropriate
prcomp(USArrests, scale = TRUE, center = TRUE)
prcomp(~ Murder + Assault + Rape, data = USArrests, scale = TRUE)
plot(prcomp(USArrests))
summary(prcomp(id100Small, scale = TRUE))
biplot(prcomp(USArrests, scale = TRUE))





#--------------------------------------------
#--------------- Opgave 2.1.1.1 del 2
#--------------------------------------------
