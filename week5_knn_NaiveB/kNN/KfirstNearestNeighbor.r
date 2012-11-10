# KfirstNearestNeighbor.R
# Class Example for K-Nearest Neighbor
# 
# Author:  PatriciaHoffman
###############################################################################
rm(list=ls())
#install.packages("class")
library(class)
library(e1071)
#setwd("C:/Users/PatriciaHoffman/workspaceR/TestDataSets")
STrain <- read.table("sonar_train.csv",sep = ",",header = FALSE)
STest <- read.table("sonar_test.csv",sep = ",",header = FALSE)
Sonar <- rbind(STrain,STest)
train <- Sonar[,1:60]
labels <- Sonar[,61]
out <- knn.cv(train,labels,k=1)
1-sum(abs(labels == out))/length(out)
# [1] 0.1730769

####
####
# What effect does removing the mean and normalizing by the variance have?
####

train2 <- train
for(i in seq(from = 1, to = ncol(train))){
	v = var(train[,i])
	m = mean(train[,i])
	train2[,i] <- (train[,i]-m)/sqrt(v)
}
out <- knn.cv(train2,labels,k=1)
1-sum(abs(labels == out))/length(out)
#  [1] 0.125
####
####
#cross - validation
####
Err <- rep(0,20)
for(kk in seq(from=1,to=20)){
	out <- knn.cv(train2,labels,k=kk)
	Error <- 1-sum(abs(labels == out))/length(out)
	Err[kk] <- Error   
}
Err
plot(Err)

####
####
#Out of curiosity how does it work without normalizing?
####

Err <- rep(0,20)
for(kk in seq(from=1,to=20)){
	out <- knn.cv(train,labels,k=kk)
	Error <- 1-sum(abs(labels == out))/length(out)
	Err[kk] <- Error   
}
Err
plot(Err)