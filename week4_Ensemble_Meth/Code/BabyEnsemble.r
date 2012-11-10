# Baby Ensemble 1st subtle wrong way then correct way
# 
# Author: PatriciaHoffman
###############################################################################
#Part I)  Generate 10 linear models using Sonar training data set.  Each of these models will incorporate a different random subset of the attributes.  
#To generate one of these linear models:
#		A) choose n attributes randomly (pick n to be between 5 - 30)
#
#       B) fit the linear model to the training set using these n attributes
#
#       C) use this model to make predictions on both the training set 
#               and the hold out set
#       D) record the training error and test error
#       E) retain the predictions for both the training set and the test set 
#           (this will become an attribute for Part II)
#
#       F) rank this model (Give the model with the lowest test error the highest rank)
#
#Part II)  In this step, use linear regression to create an ensemble model.  
#Treat the output of the 10 linear models (from step E above) 
#as inputs to a new regression to create the ensemble model.  
#You now have 10 new attributes for each observation 
#(one from each of the predictions you made in step E above.)  
#The next step is to perform the linear regression over the ensemble training set which only has the 10 new attributes.  (Ignore the original 60 attributes.)   Compare the performance of the ensemble model on the training set with the performance of the ensemble model on the hold out set.  What are the coefficients for the 10 new attributes?  Compare these coefficients with the scores from part D) above.  



rm(list=ls())
library(MASS)

STrain <- read.table("sonar_train.csv",sep = ",",header = FALSE)
STest <- read.table("sonar_test.csv",sep = ",",header = FALSE)
Sonar <- rbind(STrain,STest)
#sonarMat <- as.matrix(Sonar)
x <- Sonar[,1:60]
y <- Sonar[,61]

xNew <- NULL

Icol <- 1:60
Irow <- 1:nrow(x)
nxval <- 10

#  First an incorrect example - 
#       choosing the attributes 
#    before doing the cross validataion 

#form 10 ordinary linear models by taking 10 random samples of 
#the columns of x.  Then cross-validate each model to get an estimate of 
#the individual error rates.  Form the output of the 10 models into a new 
#x-matrix
for(icol in 1:10){
	Isamp <- sample(Icol,15)
	xTemp <- x[,Isamp]
	trainErr <- 0.0
	testErr <- 0.0
	for(ixval in 1:10){
		Iout <- which(Irow%%nxval==ixval%%nxval)
		xOut <- xTemp[Iout,]
		xIn <- xTemp[-Iout,]
		yOut <- y[Iout]
		yIn <- y[-Iout]
		dataSet <- cbind(xIn,yIn)
		fit <- lm(yIn~.,data=dataSet)
		yh <- predict(fit,xIn)
		trainErr <- trainErr + sum(yIn*yh<0)/length(yIn)
		
		yh <- predict(fit,xOut)
		testErr <- testErr + sum(yOut*yh<0)/length(yOut)
	}
	print(icol)
	print(trainErr/nxval)
	print(testErr/nxval)
	dataSet <- cbind(xTemp,y)
#fit each of the 10 models over the whole training set. - see a problem here?
	fit <- lm(y~.,dataSet)
	xNew <- cbind(xNew,predict(fit,xTemp))
}

#check the correlations of the columns of the "xNew"
cor(xNew)
#set up for ridge regression using the xNew as input instead of
#the original x-matrix
exponent <- seq(from=4, to=-1, by=-0.1)
Err <-matrix(0.0, length(exponent),3)

for(ilambda in 1:length(exponent)){
	xlambda <- 10^(exponent[ilambda])
	testErr <- 0.0
	trainErr <- 0.0
	for(ixval in 1:nxval){
		Iout <- which(Irow%%nxval==ixval%%nxval)
		
		Xin <- as.matrix(xNew[-Iout,])
		Xout <- as.matrix(xNew[Iout,])
		Yin <- y[-Iout]
		Yout <- y[Iout]
		mod <- lm.ridge(Yin~Xin,lambda=xlambda)
		C <- mod$coef/mod$scales
		XM <- Xin
		for(i in seq(from = 1, to = ncol(Xin))){
			XM[,i]<-Xin[,i]-mod$xm[i]
		} 
		xTemp <- as.matrix(XM)
		A <- as.array(C)
		Yh <- xTemp%*%A + mod$ym
		trainErr <- trainErr + sum(Yh*Yin<0)/(nrow(as.matrix(Yin))*10)	
		XM <- Xout
		for(i in seq(from = 1, to = ncol(Xout))){
			XM[,i]<-Xout[,i]-mod$xm[i]
		} 
		xTemp <- as.matrix(XM)
		A <- as.array(C)
		Yh <- xTemp%*%A +mod$ym
		testErr <- testErr + sum(Yh*Yout<0)/(nrow(as.matrix(Yout))*10)		
	}
	Err[ilambda,1] <- testErr
	Err[ilambda,2] <- trainErr
	Err[ilambda,3] <- xlambda
}

minimum <- min(Err[,2])
maximum <- max(Err[,1])
plot(Err[,1],col="red2", ylim=c(minimum,maximum))
points(Err[,2],col="blue")
min(Err[,1])

#this gives around 15% miss-classification error rate.  too good to be true
#why??

#re-do with a sound approach
Icol <- 1:60
Irow <- 1:nrow(x)
nxval <- 10

nColSamp <- 15
IndexMat <- matrix(0.0,nColSamp,10)

for(icol in 1:10){
	Isamp <- sample(Icol,15)
	IndexMat[,icol] <- Isamp
}

#now form the 10 models inside the x-validation loop instead of outside
for(ilambda in 1:length(exponent)){
	xlambda <- 10^(exponent[ilambda])
	testErr <- 0.0
	trainErr <- 0.0
	for(ixval in 1:nxval){
		Iout <- which(Irow%%nxval==ixval%%nxval)
		xNewOut <- NULL
		xNewIn <- NULL
		xFullOut <- x[Iout,]
		xFullIn <- x[-Iout,]
		Yin <- y[-Iout]
		Yout <- y[Iout]
		for(icol in 1:10){
			nColSamp <- IndexMat[,icol]
			xTempIn <- xFullIn[,nColSamp]
			xTempOut <- xFullOut[,nColSamp]
			dataSet <- cbind(xTempIn,Yin)
			
			fit <- lm(Yin~.,dataSet)
			xNewIn <- cbind(xNewIn,predict(fit,xTempIn))
			xNewOut <- cbind(xNewOut,predict(fit,xTempOut))
		}
		
		Xin <- as.matrix(xNewIn)
		Xout <- as.matrix(xNewOut)
		
		mod <- lm.ridge(Yin~Xin,lambda=xlambda)
		C <- mod$coef/mod$scales
		XM <- Xin
		for(i in seq(from = 1, to = ncol(Xin))){
			XM[,i]<-Xin[,i]-mod$xm[i]
		} 
		xTemp <- as.matrix(XM)
		A <- as.array(C)
		Yh <- xTemp%*%A + mod$ym
		trainErr <- trainErr + sum(Yh*Yin<0)/(nrow(as.matrix(Yin))*10)	
		XM <- Xout
		for(i in seq(from = 1, to = ncol(Xout))){
			XM[,i]<-Xout[,i]-mod$xm[i]
		} 
		xTemp <- as.matrix(XM)
		A <- as.array(C)
		Yh <- xTemp%*%A +mod$ym
		testErr <- testErr + sum(Yh*Yout<0)/(nrow(as.matrix(Yout))*10)		
	}
	Err[ilambda,1] <- testErr
	Err[ilambda,2] <- trainErr
	Err[ilambda,3] <- xlambda
}

minimum <- min(Err[,2])
maximum <- max(Err[,1])
plot(Err[,1],col="red2", ylim=c(minimum,maximum))
points(Err[,2],col="blue")
legend(30, 0.27, c("TEST","TRAIN"), cex = 1, col = c("red2", "blue"),
		pch = c(16, 16), lty = 1:2)
min(Err[,1])





