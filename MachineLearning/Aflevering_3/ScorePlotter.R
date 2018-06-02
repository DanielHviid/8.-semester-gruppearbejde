

# load data
R1 = read.csv("f1.txt", sep=" ", header = F)
R2 = read.csv("recall.txt", sep=" ", header = F)
R3 = read.csv("precision.txt", sep=" ", header = F)

ExF1 <- as.matrix(R1)
ExRecall <- as.matrix(R2)
ExPrecision <- as.matrix(R3)

LargestF1Scores <- c()
#Plot F1 scores 
for (i in 1:13){
  max <- 0;
  for (j in 1:13){
    if(max < ExF1[[i,j]]){
      max <- ExF1[[i,j]]
    }
  }
  LargestF1Scores <- append(LargestF1Scores, max)
}


matplot(c(1:13), LargestF1Scores, "l", col = colors, ylim = c(0.95,1), ylab = "F1 Score", xlab = "K-value")
legend(x = "topright", 0.95, legend=c("No cluster", "Nclu 10", "Nclu 20", "Nclu 30", "Nclu 40", "Nclu 50", "Nclu 60", "Nclu 70", "Nclu 80", "Nclu 90", "Nclu 100"), lty=1:2, cex=0.8, col = colors)
title("F1 Score")


#plot the precicion recall curves












