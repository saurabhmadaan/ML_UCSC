setwd("/Users/smadaan/Documents/ML_UCSC/week2/Data")
require(rpart)


##--Q1s----
train<-read.csv("sonar_train.csv",header=FALSE)

str(train)




nxval <- 10
ndepth <- 10
trainOutput <- matrix(0.0,nrow = ndepth, ncol = 2)
testOutput <- matrix(0.0,nrow =ndepth, ncol = 2)
I <- seq(from = 1, to = nrow(train))



	for(ixval in seq(from =  1, to = nxval)){
		Iout <- which(I%%nxval == ixval%%nxval)
		trainIn <- train[-Iout,]
		trainOut <- train[Iout,]
		yin <- as.factor(trainIn[,61])
		yout <- as.factor(trainOut[,61])
		xin <- trainIn[,1:60]
		xout <- trainOut[,1:60]
		
		fit <- rpart(yin~.,xin,control=rpart.control(maxdepth=idepth))
		dum <- predict(fit,xin)
		yhat <- rep(0.0,nrow(dum))
		for(i in 1:nrow(dum)){
			yhat[i] <- 2*(which.max(dum[i,]) - 1) -1			
		}
		trainErr <- trainErr + (1-sum(yin==yhat)/length(yin))
		dum <- predict(fit,xout)
		yhat <- rep(0.0,nrow(dum))
		for(i in 1:nrow(dum)){
			yhat[i] <- 2*(which.max(dum[i,]) - 1) -1			
		}
		testErr <- testErr + (1-sum(yout==yhat)/length(yout))
	}
	trainOutput[idepth,1] <- idepth
	trainOutput[idepth,2] <- trainErr/nxval
	testOutput[idepth,1] <- idepth
	testOutput[idepth,2] <- testErr/nxval
}

maxval = max(testOutput[,2])
plot(trainOutput, ylim=c(0,maxval),
		main="Model Complexity",
		xlab="Model Complexity = Tree Depth",
		ylab="Prediction Error"
	)
points(testOutput, col = 2)

index <- which.min(testOutput[,2])
testOutput[index,2]
index


#Plot for most complicated tree depth
plot(fit)
text(fit)
print(fit)
post(fit,file="")


#  How did this complicated model do on the Test Set?
test<-read.csv("sonar_test.csv",header=FALSE)
testansw <- as.factor(test[,61])
testobv <- test[,1:60]
dum <- predict(fit,testobv)
yhat <- rep(0.0,nrow(dum))
for(i in 1:nrow(dum)){
	yhat[i] <- 2*(which.max(dum[i,]) - 1) -1			
}
testError <- (1-sum(testansw==yhat)/length(testansw))
# testError = [1] 0.2948718

#Look at the plots for the best tree depth
test<-read.csv("sonar_test.csv",header=FALSE)

# Create model using training data set Decision Tree Depth = 1
yin <- as.factor(train[,61])
xin <- train[,1:60]
fitone <- rpart(yin~.,xin,control=rpart.control(maxdepth=1))


plot(fitone)
text(fitone)
print(fitone)
post(fitone,file="")

# What do the numbers on the plot signify?
dim(xin)            # 130 x 60 note  66+64 = 130
rocks<-which(train$V61 >0)
length(rocks)       # 64 observations are rocks
V11less<-which(xin$V11 <= 0.1709) 
length(V11less)     # 51  = 8 + 43 observations have V11 <= 0.17
rocksSmall<-which(train[V11less,61]> 0)
length(rocksSmall)  # 43 of these 51 are rocks (51-43 = 8)


#  How did the simple model do on the Test Set?
dum <- predict(fitone,testobv)
yhat <- rep(0.0,nrow(dum))
for(i in 1:nrow(dum)){
	yhat[i] <- 2*(which.max(dum[i,]) - 1) -1			
}
testError <- (1-sum(testansw==yhat)/length(testansw))
    
#testError = [1] 0.2820513

# Yes the simpler model has a better resulting score

# confusion table
table(test$V61,yhat)

#yhat
#    -1  1
#-1  39  6
# 1  16 17

# There are 78 observations
# There are  6 observations coded incorrectly 
#            in which the answer should be -1 
# There are 16 observations coded incorrectly 
#            in which the answer should be +1

(16+6)/78   # [1] 0.2820513




yout<-predict(fit1,testdf, type="class")
#> yout
#1 2 3 4 
#1 0 0 0 
#Levels: 0 1