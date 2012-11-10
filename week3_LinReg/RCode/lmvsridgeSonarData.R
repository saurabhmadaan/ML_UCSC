#      Use the Sonar Data Set
#
# First use ordinary linear regression (lm)
# Compare this with using ridge regression lm.ridge
#
# Author: PatriciaHoffman
###############################################################################

#############################################################################
#
#     ORDINARY LINEAR REGRESSION
#
#############################################################################

#  use regular ordinary linear regression to classify Sonar Data (mines vs rocks)


# Step 1.1 read in the sonar data and have a look

sonarTrain <- read.table("sonar_train.csv", sep = ",")

# Step 1.2 What's it look like? 
str(sonarTrain)

# Step 1.3 
# Perform a linear regression and see how well it works
lmSonar <- lm(V61~., data = sonarTrain)
sonarFit <- sonarTrain[,-61]
lmFit <- predict(lmSonar, newdata = sonarFit)

correct <- sum(lmFit*sonarTrain$V61 >0)       #  [1] 123
correct/length(sonarTrain$V61)                #  [1] 0.9461538

#
# Step 1.4 - 7 wrong out of 130 - doesn't seem bad on the face of it. But this is 
#      just on the training set.
#    How does it perform on the test set?
# 

sonarTest <- read.table("sonar_test.csv", sep = ",")
str(sonarTest)

sonarFitTest <- sonarTest[,-61]
lmFitTest <- predict(lmSonar, newdata = sonarFitTest)

correct <- sum(lmFitTest*sonarTest$V61 > 0)  #[1] 50
correct/length(sonarTest$V61)                #[1] 0.6410256

# The error rate on the training data  was 5%, the error rate on the test data is 36%
# Considerably worse.  Is there anything wrong here? 

# Overfitting - What is overfitting??

# Step 1.5 - Here's a way to use a small amount of data (we'll use 10% in the example)
# to estimate preformance while training

# Cross-validation
Sonar <- sonarTrain
trainErr <- 0.0
testErr <- 0.0
I <- seq(from = 1, to = nrow(Sonar))
for(ixval in seq(from = 1, to = 10)){
	Iout <- which(I%%10 == ixval - 1)
	SonarIn <- Sonar[-Iout,]
	SonarOut <- Sonar[Iout,]
	
	lmSonar <- lm(V61~., data = SonarIn)
	sonarFit <- SonarIn[,-61]
	lmFit <- predict(lmSonar, newdata = sonarFit)
	
	correct <- sum(lmFit*SonarIn$V61 >0)
	trainErr <- trainErr + (1-correct/length(SonarIn$V61))/10
	
	sonarFit <- SonarOut[,-61]
	lmFit <- predict(lmSonar, newdata = sonarFit)
	
	correct <- sum(lmFit*SonarOut$V61 >0)
	testErr <- testErr + (1-correct/length(SonarOut$V61))/10
}
print("trainErr = ")
trainErr                   #[1] 0.04529915
print("testErr = ")
testErr                    #[1] 0.2846154

# Step 1.6 - Let's see how much reduction we can get in the overtraining
# error by using both sonar_train and sonar_test  (increase the amount of data used in training)
Sonar <- rbind(sonarTrain, sonarTest)
trainErr <- 0.0
testErr <- 0.0
for(ixval in seq(from = 1, to = 10)){
	Iout <- which(I%%10 == ixval - 1)
	SonarIn <- Sonar[-Iout,]
	SonarOut <- Sonar[Iout,]
	
	lmSonar <- lm(V61~., data = SonarIn)
	sonarFit <- SonarIn[,-61]
	lmFit <- predict(lmSonar, newdata = sonarFit)
	
	correct <- sum(lmFit*SonarIn$V61 >0)
	trainErr <- trainErr + (1-correct/length(SonarIn$V61))/10
	
	sonarFit <- SonarOut[,-61]
	lmFit <- predict(lmSonar, newdata = sonarFit)
	
	correct <- sum(lmFit*SonarOut$V61 >0)
	testErr <- testErr + (1-correct/length(SonarOut$V61))/10
}
print("trainErr = ")
trainErr                     #[1] 0.09538462
print("testErr = ")     
testErr                      #[1] 0.2230769

# We could keep adding more data until the trainErr and testErr are close to the same
# but we're out of data.  What to do?  We need to reduce degrees of freedom somehow

#############################################################################
#
#     RIDGE REGRESSION
#
#############################################################################



#######################################################################
# RIDGE REGRESSION - Cross-validation in conjunction with ridge regression


# Step 2.1 read in both sonar_train and sonar_test and combine them
# into a single file.  Perform a ridge regression for a variey of penalty values
library(MASS)
sonarTrain <- read.table("sonar_train.csv", sep = ",")
sonarTest <- read.table("sonar_test.csv", sep = ",")
Sonar <- rbind(sonarTrain, sonarTest)

Err <- matrix(nrow = 31, ncol = 3)
I <- seq(1:nrow(Sonar))

for(iLambda in seq(from = 0, to = 30)){
	#
	exp <- (+3 -4*(iLambda/20))
	xlambda <- 10^exp
	
	testErr <- 0.0
	trainErr <- 0.0
	
	for(ixval in seq(from = 1, to = 10)){
		Iout <- which(I%%10 == ixval - 1)
		SonarIn <- Sonar[-Iout,]
		SonarOut <- Sonar[Iout,]
		#Xin <- X[-Iout,]
		Xin <- SonarIn[,1:60]
		Xout <- SonarOut[,1:60]
		Yin <- SonarIn[,61]
		Yout <- SonarOut[,61]
		mod <- lm.ridge(V61~.,data=SonarIn,lambda=xlambda)
		C <- mod$coef/mod$scales
		XM <- Xin
		for(i in seq(from = 1, to = ncol(Xin))){
			XM[,i]<-Xin[,i]-mod$xm[i]
		}
		X <- as.matrix(XM)
		A <- as.array(C)
		Yh <- X%*%A + mod$ym
		#trainErr <- trainErr + sum(abs(Yin - Yh))/(nrow(as.matrix(Yin))*10)
		trainErr <- trainErr + (1- sum(Yin*Yh > 0)/length(Yin))/10
		XM <- Xout
		for(i in seq(from = 1, to = ncol(Xout))){
			XM[,i]<-Xout[,i]-mod$xm[i]
		}
		X <- as.matrix(XM)
		A <- as.array(C)
		Yh <- X%*%A +mod$ym
		#testErr <- testErr + sum(abs(Yout - Yh))/(nrow(as.matrix(Yout))*10)
		testErr <- testErr + (1 - sum(Yout*Yh > 0)/length(Yout))/10
	}
	Err[(iLambda+1),1] = trainErr
	Err[(iLambda+1),2] = testErr
	Err[(iLambda+1),3] = xlambda
}
plot(Err[,1], type='p', col='red', ylim=c(0,1),
		main = 'Error vs Log(Lambda)',
		ylab='Error',
		xlab='3 - Log(Lambda)')
points(Err[,1], pch=15, col='red')
#lines(out[,2], type='l', col='blue')
points(Err[,2], pch=16, col='blue')
legend(5, 0.4, c("TRAIN", "TEST"), cex = 1, col = c("red", "blue"),
		pch = c(15, 16), lty = 1:2)

Err
min(Err[,2])   # the test error = [1] 0.187381

# the best test error for olr was 0.22 so using ridge regression improved the result
#  Recall that using rpart decision tree the error was # [1] 0.2820513
