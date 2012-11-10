###############################################################################
# Homework5
#
# Adapted from Patricia Hoffman's code
###############################################################################


###############################################################################
# Problem 1
###############################################################################
library(class)
library(e1071)

datafiledir = "/Users/steveb/Documents/Classes/MachineLearningUCSCExt/Week5/Homework/DataFiles"
datafile = paste(datafiledir, "winequality-red.csv", sep="/")

winedf = read.csv(datafile, header = TRUE, sep=';', quote="\"")
ncols         <- ncol(winedf)
nxcol         <- ncols - 1

train  <- winedf[, 1:nxcol ]
yvals  <- winedf[,ncols]

######################################################
### Run KNN using k=1 using cross validation (.cv)
######################################################
out <- knn.cv(train, yvals, k=1)
error <- 1 - sum( yvals == out ) / length(out)
error
# [1] 0.3846154

######################################################
### Center and scale data.
### Run KNN with k=1 and cross validation.
######################################################
trainCenterScaled <- train
for(i in seq(from = 1, to = ncol(train))){
	v = var(train[,i])
	m = mean(train[,i])
	trainCenterScaled[,i] <- (train[,i]-m)/sqrt(v)
}
out <- knn.cv(trainCenterScaled,yvals,k=1)
error = 1-sum(abs(yvals == out))/length(out)
error
# [1] 0.3414634

######################################################
### ****
### Run KNN a number of times with k ranging from
### 1 to 20, using cross validation. This uses the
### data that has been centered and scaled.
### ****
######################################################
Err <- rep(0,20)
for(kk in seq(from=1,to=20)){
	out <- knn.cv(trainCenterScaled,yvals,k=kk)
	Err[kk] <- 1-sum( yvals == out )/length(out)
}
Err
plot(Err, xlab="K")
which( Err == min(Err))
# [1] 1
Err[ which( Err == min(Err)) ]
# [1] 0.3414634

######################################################
### This does the same as the previous step but using
### data that has NOT been centered and scaled.
### The error is larger.
######################################################
Err <- rep(0,20)
for(kk in seq(from=1,to=20)){
	out <- knn.cv(train,yvals,k=kk)
	Err[kk] <- 1-sum( yvals == out )/length(out)
}
Err
plot(Err)
Err[ which(Err == min(Err))]
# [1] 0.3846154




###############################################################################
# Problem 2
###############################################################################

rm(list=ls())
library(class)
library(e1071)
setwd("/Users/sabesansp/Research/Machine_Learning/UCSC/class_1/working_dir")
iris <- read.table("irisdata.csv",sep = ",",header = FALSE)
str(iris)
train <- iris[,1:4]
labels <- iris[,5]
###Scale the data
train2 <- train
for(i in seq(from = 1, to = ncol(train))){
	v = var(train[,i])
	m = mean(train[,i])
	train2[,i] <- (train[,i]-m)/sqrt(v)
}
####Perform cross validation on the new data
out <- knn.cv(train2,labels,k=3)
1-sum(abs(labels == out))/length(out)
Err <- rep(0,50)
for(kk in seq(from=1,to=50)){
	out <- knn.cv(train2,labels,k=kk)
	Error <- 1-sum(abs(labels == out))/length(out)
	Err[kk] <- Error   
}
Err
###> Err
'
 [1] 0.05333333 0.05333333 0.05333333 0.06000000 0.05333333 0.04000000
 [7] 0.04000000 0.05333333 0.04666667 0.04000000 0.04666667 0.03333333
[13] 0.03333333 0.03333333 0.03333333 0.03333333 0.03333333 0.04000000
[19] 0.04666667 0.04000000 0.05333333 0.05333333 0.04666667 0.04666667
[25] 0.04666667 0.04666667 0.05333333 0.04000000 0.05333333 0.05333333
[31] 0.05333333 0.06000000 0.05333333 0.06000000 0.05333333 0.05333333
[37] 0.08666667 0.09333333 0.10000000 0.10666667 0.11333333 0.11333333
[43] 0.10666667 0.10666667 0.10666667 0.10666667 0.11333333 0.12000000
[49] 0.12666667 0.13333333
'
plot(Err)


###############################################################################
# Problem 3
###############################################################################

library(klaR)
setwd('/Users/smadaan/Documents/ML_UCSC/week2/Data')

# read data
wdata <- read.csv('winequality-red.csv', header=TRUE, sep=';')
str(wdata)

# convert wine quality to categorical variable
wdata$quality <- as.factor(wdata$quality)

# apply Naive Bayes classification
mod <- NaiveBayes(quality~.,data = wdata)

# predict based on classifier
qualityHat <- predict(mod, wdata[,1:11])

# calculate error
Err <- 1 - sum(qualityHat$class == wdata$quality)/length(wdata$quality)
Err

# [1] 0.4396498

# are dependent variables correlated
pairs(wdata[,1:11])


