# rpart Example
#      explore the maxdepth parameter
#
# Data File: table4_8pg199.txt 
# Author: PatriciaHoffman
###############################################################################
rm(list=ls())
require("rpart")
#install.packages("rpart")
#library(rpart)
#Reference:
#http://www.stanford.edu/class/stats315b/minitech.pdf
setwd("/Users/smadaan/Documents/ML_UCSC/week2/Data")
# create the model (use the "train" function in R)
train<-read.csv("table4_8pg199.txt",header=TRUE)
y<-as.factor(train[,5])#class labels 0 or 1
x<-train[,2:4]
str(train)
x;y
is.numeric(train$a3)

# Tree of maxdepth = 5
fit<-rpart(y~.,x,control=rpart.control(minsplit=0,minbucket=0,cp=-1,
				maxcompete=0, maxsurrogate=0, usesurrogate=0, 
				xval=0,maxdepth=5))

1-sum(y==predict(fit,x,type="class"))/length(y)
# returns 0  all correct
plot(fit)
text(fit)
print(fit)
post(fit,file="")

# Tree of maxdepth = 2
fit<-rpart(y~.,x,control=rpart.control(minsplit=0,minbucket=0,cp=-1,
				maxcompete=0, maxsurrogate=0, usesurrogate=0, 
				xval=0,maxdepth=2))

1-sum(y==predict(fit,x,type="class"))/length(y)
# returns 0.222  20% error
plot(fit)
text(fit)

fit

print(fit)

answers <- predict(fit, type="class") 
answers
answers[2]
length(answers)


predict(fit, type="prob")   # class probabilities (default)
predict(fit, type="vector") # level numbers
predict(fit, type="class")  # factor
predict(fit, type="matrix") # level number, class frequencies, probabilities



#------- SM try

train[,5]=as.factor(train[,5])
set.seed(4355)
fit1<-rpart(Target~a1 + a2 + a3, data=train,control=rpart.control(minsplit=0,minbucket=0,cp=-1,
				maxcompete=0, maxsurrogate=0, usesurrogate=0, 
				xval=0,maxdepth=5))
plot(fit1)
text(fit1)












