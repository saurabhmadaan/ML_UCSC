# rpart on the Concrete Data
#    10 fold cross validation
# Author: PatriciaHoffman
###############################################################################

setwd("/Users/smadaan/Documents/ML_UCSC/week2/Data")

#  This data contains 1030 observations of 9 variables
#  Concrete compressive Strength, the target variable,
#    is in the 9th column.  

rm(list=ls())

data <- read.table(file="Concrete_Data.csv",sep=",",header=TRUE)
str(data)
head(data)
train <- data
x <- data[,1:8]
#put response into Y
y <- data[,9]

require(rpart)
nxval <- 10
ndepth <- 10
trainOutput <- matrix(0.0,nrow = ndepth, ncol = 2)
testOutput <- matrix(0.0,nrow =ndepth, ncol = 2)
I <- seq(from = 1, to = nrow(train))

# For Debug
#   idepth<-1;trainErr<-0.0;testErr<-0.0;ixval<-1

for(idepth in 1:ndepth){
	trainErr <- 0.0
	testErr <- 0.0
	for(ixval in seq(from =  1, to = nxval)){
		Iout <- which(I%%nxval == ixval%%nxval)
		trainIn <- train[-Iout,]
		trainOut <- train[Iout,]
		yin <- trainIn[,9]
		yout <- trainOut[,9]
		xin <- trainIn[,1:8]
		xout <- trainOut[,1:8]		
		fit <- rpart(yin~.,xin,control=rpart.control(maxdepth=idepth))
		yhat <- predict(fit,xin)
		dY <- yin - yhat
		trainErr <- trainErr + sqrt(sum(dY*dY))/(length(yin))	
		yhat <- predict(fit,xout)
		dY <- yout - yhat
		testErr <- testErr + sqrt(sum(dY*dY))/(length(yout))	
		}
	trainOutput[idepth,1] <- idepth
	trainOutput[idepth,2] <- trainErr/nxval
	testOutput[idepth,1] <- idepth
	testOutput[idepth,2] <- testErr/nxval
}

maxval = max(testOutput[,2])

# Create the Model Complexity Graph
plot(trainOutput, ylim=c(0,maxval),
		main="Model Complexity",
		xlab="Model Complexity = Tree Depth",
		ylab="Prediction Error"
)
points(testOutput, col = 2)

#Plot for most complicated tree depth
plot(fit)
text(fit)
print(fit)
post(fit,file="")


index <- which.min(testOutput[,2])
testOutput[index,2]
index
bestDepth <- testOutput[index,1]
bestDepth # = 6
resultError <-testOutput[index,2] # [1] 0.9088021


#Check to see what the text error for all depths
testOutput[,2]
#[1] 1.4285704 1.2044311 1.0348191 0.9817373 0.9150126 0.9088021 0.9088021
#[8] 0.9088021 0.9088021 0.9088021

#The least complicated tree with the best error has depth 6

# Create the model that will be deployed 
# Use all the available data to create this model
#Look at the plots for the best tree depth

fitbest <- rpart(y~.,x,control=rpart.control(maxdepth=bestDepth))

plot(fitbest)
text(fitbest)
print(fitbest)
#post(fitbest,file="")
