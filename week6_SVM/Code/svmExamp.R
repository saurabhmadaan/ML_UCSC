# TODO: Add comment
# 
# Author: PatriciaHoffman
###############################################################################
#############################################################################
rm(list=ls())

require(e1071)


#read in the mixture simulation data  and plot it.  
mixSim <- read.table(file="mixtureSimData.data")

x1 <- mixSim[1:200,1]
mixSimData <- as.data.frame(x1)
x2 <- mixSim[201:400,1]

mixSimData <- cbind(mixSimData,x2)
plot(mixSimData)
points(mixSimData[101:200,1:2], col = 2)
points(mixSimData[1:100,1:2], col = 3)


#As a reminder, here's 15 nearest neighbor classification run on this 
#data set.  

maxX1 <- max(mixSimData[,1])
minX1 <- min(mixSimData[,1])
maxX2 <- max(mixSimData[,2])
minX2 <- min(mixSimData[,2])

#fog of test points
testMat <- matrix(0.0,10000,2)

for(i in 1:100){
	for(j in 1:100){
		x1 <- minX1 + i*(maxX1 - minX1)/100
		x2 <- minX2 + j*(maxX2 - minX2)/100
		index <- (i-1)*100 + j
		testMat[index,1] <- x1
		testMat[index,2] <- x2
	}	
}

XX <- c((1:100)*(maxX1 - minX1))
XX <- XX/100
XX <- XX + minX1

YY <- c((1:100)*(maxX2 - minX2))
YY <- YY/100
YY <- YY + minX2
Y <- rep(0,200)
Y[101:200] <- 1
Y <- as.factor(Y)

#1st knn plot (15)
require(class)
KNN <- knn(mixSimData, testMat, Y, 15)
ZZ <- matrix(0.0,100,100)
for(i in 1:100){
	for(j in 1:100){
		index <- (i-1)*100 + j
		ZZ[i,j] <- KNN[index]
	}
}

#here's the plot with contour.
I1 <- which(KNN == 1)
plot(testMat, pch=".")
points(testMat[I1,], col=2, pch=".")
points(testMat[-I1,],col = 3, pch=".")
points(mixSimData[1:100,], col = 3)
points(mixSimData[101:200,], col = 2)
contour(XX,YY,ZZ,levels = 1.5, drawlabels = FALSE, add = TRUE)



#now let's redo this classification using svm.  
#first, we'll use linear kernel.  
mixSimData <- cbind(mixSimData,Y)

C <- 2^(-10)

C = 2*C
model <- svm(Y ~ x1 + x2, data = mixSimData, kernel="linear", cost=C)
plot(model,mixSimData)

#that's a little dull compared to knn.  let's try radial basis functions
#for rbf, we should narrow down the parameter selection by doing 
#some tuning/cross-validation.  

tuneModel <- tune.svm(Y~x1+x2,data= mixSimData,
		gamma = 2^(-2:2),cost=2^(0:4))

plot(tuneModel)


C <- 2^(-10)

C = 2*C
model <- svm(Y ~ x1 + x2, data = mixSimData, cost=C, gamma=2)
plot(model,mixSimData)

