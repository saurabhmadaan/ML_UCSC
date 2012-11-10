# TODO: Comparison of Ada Boost and Random Forest
# 
# Author: PatriciaHoffman
###############################################################################

# You only need to install packages once.  In order to install a package 
#      uncomment and use the 
#      following line of code
#install.packages("randomForest")
library(rpart)
train<-read.csv("sonar_train.csv",header=FALSE)
test<-read.csv("sonar_test.csv",header=FALSE)
y<-train[,61]
x<-train[,1:60]
y_test<-test[,61]
x_test<-test[,1:60]
train_error<-rep(0,500)
test_error<-rep(0,500)
f<-rep(0,130)
f_test<-rep(0,78)
i<-1

while(i<=500){
	w<-exp(-y*f)
	w<-w/sum(w)
	fit<-rpart(y~.,x,w,method="class")
	g<--1+2*(predict(fit,x)[,2]>.5)
	g_test<--1+2*(predict(fit,x_test)[,2]>.5)
	e<-sum(w*(y*g<0))
	alpha<-.1*.5*log ( (1-e) / e )
	f<-f+alpha*g
	f_test<-f_test+alpha*g_test
	train_error[i]<-sum(1*f*y<0)/130
	test_error[i]<-sum(1*f_test*y_test<0)/78
	i<-i+1
}
plot(seq(1,500),test_error,type="l",
		main="AdaBoost Error Plot",
		ylim=c(0,.5),ylab="Error Rate",xlab="Iterations",lwd=2)
lines(train_error,lwd=2,col="purple")
legend(4,.5,c("Training Error","Test Error"),
		col=c("purple","black"),lwd=2)
train_error[500] #[1] 0
test_error[500]  #[1] 0.2051282


#load randomForest package
library(randomForest)
train<-read.csv("sonar_train.csv",header=FALSE)
test<-read.csv("sonar_test.csv",header=FALSE)

y<-as.factor(train[,61])
x<-train[,1:60]
y_test<-as.factor(test[,61])
x_test<-test[,1:60]
fit<-randomForest(x,y)
1-sum(y==predict(fit,x))/length(y)
#[1] 0
1-sum(y_test==predict(fit,x_test))/length(y_test)
#[1] 0.1410256


# random forest did better for this data set