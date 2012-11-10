
#Multiclass - Iris Data

#  This is a multiclass problem
#     Three models are created using Ridge Regression  
#     In the first model: Setosa is separated from Versicolor and Virginica
#     In the second model: Versicolor is separated from Setosa and Virginica
#     In the third model: Virginica is separated from Setosa and Versicolor 
#  The three models are combined in the end to make predictions.
#
# Author: PatriciaHoffman
###############################################################################
rm(list=ls())
data(iris)
attach(iris)
library(MASS)


iris <- read.table("irisdata.csv",sep = ",",header = FALSE)

Y1 <- c(rep(-1,150))
Y2 <- Y1
Y3 <- Y2
Y1[1:50] <- 1        #Y1 will be used to separate out Setosa
Y2[51:100] <- 1      #Y2 is used to separate out Versicolor
Y3[101:150] <- 1     #Y3 is used to separate out Virginica

target <- cbind(Y1,Y2,Y3)
X <- iris[,-5]
I <- 1:150
Err <- matrix(nrow = 21, ncol = 3)

#The first step is to use cross validation
#  to find the best lambda 
#  for each of the three models
#These lambdas will be used to build 
#  the production models
prolambda <- c(rep(0,3)) # prolambda will hold these three lambdas

for(ident in seq(1,3)){  # find the best lambda for each model

for(iLambda in seq(from = 0, to = 20)){ # cross validation loop
	#
	exp <- (+2 -4*(iLambda/20))
	xlambda <- 10^exp
	
	testErr <- 0.0
	trainErr <- 0.0
	
	for(ixval in seq(from = 1, to = 10)){
		Iout <- which(I%%10 == ixval - 1)
		Xin <- X[-Iout,]
		Xout <- X[Iout,]
		Yin <- target[-Iout,ident]  # run with Y1, Y2 and Y3
		Yout <- target[Iout,ident]  # run with Y1, Y2 and Y3
		dataIn <- cbind(Xin,Yin)
		mod <- lm.ridge(Yin~.,data=dataIn,lambda=xlambda)
		C <- mod$coef/mod$scales
		XM <- Xin
		for(i in seq(from = 1, to = ncol(Xin))){
			XM[,i]<-Xin[,i]-mod$xm[i]
		} 
		XX <- as.matrix(XM)
		A <- as.array(C)
		Yh <- XX%*%A + mod$ym
		YhP <- Yh>=0.0
		Yp <- Yin>= 0.0
		trainErr <- trainErr + sum(YhP != Yp)/(length(Yin)*10)
		XM <- Xout
		for(i in seq(from = 1, to = ncol(Xout))){
			XM[,i]<-Xout[,i]-mod$xm[i]
		} 
		XX <- as.matrix(XM)
		A <- as.array(C)
		Yh <- XX%*%A + mod$ym
		YhP <- Yh>=0.0
		Yp <- Yout>= 0.0
		testErr <- testErr + sum(YhP != Yp)/(length(Yout)*10)
	}
	Err[(iLambda+1),1] = trainErr
	Err[(iLambda+1),2] = testErr
	Err[(iLambda+1),3] = xlambda
}#end of cross validation loop

# recall if lambda is zero you have the most complicated model
# if the min test error is the same for several lambda's, 
# choose the lambda which gives the least complicated model

prolambda[ident] <- Err[min(which(min(Err[,2])== Err[,2])),3]
#prolambda is the lambda to use in the production model

}#end of setting up the lambda


plot(Err[,1], type='p', col='red', ylim=c(0,1),
		main = 'Error vs Log(Lambda) For third model',
		ylab='Error',
		xlab='3 - Log(Lambda)')
points(Err[,1], pch=15, col='red')
#lines(out[,2], type='l', col='blue')
points(Err[,2], pch=16, col='blue')
legend(5, 0.4, c("TRAIN", "TEST"), cex = 1, col = c("red", "blue"),
		pch = c(15, 16), lty = 1:2)



# Create the three production models 
#  using the best lambdas found by cross validation

#Fit all three and calculate misclassification error


#Create the production model
# Separate out Setosa
xlambda <- prolambda[1]
Xin <- X
Yin <- Y1
dataIn <- cbind(Xin,Yin)
mod <- lm.ridge(Yin~.,data=dataIn,lambda=xlambda)
C <- mod$coef/mod$scales
XM <- Xin
for(i in seq(from = 1, to = ncol(Xin))){
	XM[,i]<-Xin[,i]-mod$xm[i]
} 
XX <- as.matrix(XM)
A <- as.array(C)
Yh1 <- XX%*%A + mod$ym

#Create the production model
# Separate out Versicolor
xlambda <- prolambda[2]
Yin <- Y2
dataIn <- cbind(Xin,Yin)
mod <- lm.ridge(Yin~.,data=dataIn,lambda=xlambda)
C <- mod$coef/mod$scales
XM <- Xin
for(i in seq(from = 1, to = ncol(Xin))){
	XM[,i]<-Xin[,i]-mod$xm[i]
} 
XX <- as.matrix(XM)
A <- as.array(C)
Yh2 <- XX%*%A + mod$ym

#Create the production model
# Separate out Viriginica
xlambda <- prolambda[3]
Yin <- Y3
dataIn <- cbind(Xin,Yin)
mod <- lm.ridge(Yin~.,data=dataIn,lambda=xlambda)
C <- mod$coef/mod$scales
XM <- Xin
for(i in seq(from = 1, to = ncol(Xin))){
	XM[,i]<-Xin[,i]-mod$xm[i]
} 
XX <- as.matrix(XM)
A <- as.array(C)
Yh3 <- XX%*%A + mod$ym

#Combine the predicitons
#positive predictions by class
Y1h <- (Yh1 > Yh2) & (Yh1 > Yh3)
Y2h <-(Yh2> Yh1) & (Yh2>Yh3)
Y3h <- (Yh3> Yh2) & (Yh3>Yh1)

#number of correct positives in each class

#               using lambda = 0.15 |  using best lambdas for each 
#               for all three       |     production model     
sum(Y1>0 & Y1h) #   50              |         49
sum(Y2>0 & Y2h) #   34              |         34
sum(Y3>0 & Y3h) #   43              |         44

# % error is unchanged

# note that by using the best lambda for the production models
#  one of the Setosa is now misclassified.
#  Why is one of the Setosa being misclassified now?

#        Data visualization


# The solid black dot in the following picture is the 
# misclassified Setosa flower.


# plot in V1 vs V2 axes

plot(iris$V1, iris$V2,
	main = 'Misclassified Setosa is Black Dot')
points(iris[1:50,1:2],col = "red")
points(iris[51:100,1:2], col = "blue")
points(iris[101:150,1:2],col = "green")
points(iris[42,1:2], col = "black", pch = 19)
legend(6.5, 4.2, c("Setosa", "Versicolor","Virginica"),cex = 1, 
		pch = c(20,20,20), lty = 1:2, col = c("red", "blue","green"))

# plot in V2 vs V3 axes
plot(iris[,2], iris[,3],
	main = 'Misclassified Setosa is Black Dot')
points(iris[1:50,2:3],col = "red")
points(iris[51:100,2:3], col = "blue")
points(iris[101:150,2:3],col = "green")
points(iris[42,2:3], col = "black", pch = 19)
legend(3.7, 4, c("Setosa", "Versicolor","Virginica"),cex = 1, 
		pch = c(20,20,20), lty = 1:2, col = c("red", "blue","green"))
