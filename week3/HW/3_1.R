

setwd('/Users/smadaan/Documents/ML_UCSC/week2/data')
data<-read.csv('winequality-red.csv',sep=";",header=TRUE)
data[,12]<-as.factor(data[,12])

levels(data[,12])
# [1] "3" "4" "5" "6" "7" "8"

I<-seq(1,1400)

train_data<-data[I,]
test_data<-data[-I,]


y3<-c(rep(-1,1400))
y4<-y3
y5<-y4
y6<-y5
y7<-y6
y8<-y7

y3[which(train_data[,12]==3)]=1
y4[which(train_data[,12]==4)]=1
y5[which(train_data[,12]==5)]=1
y6[which(train_data[,12]==6)]=1
y7[which(train_data[,12]==7)]=1
y8[which(train_data[,12]==8)]=1


#-----
library(MASS)
mod.p<- polr(train_data[,12]~.,train_data[,1:11])
pp<-predict(mod.p,train_data[,1:11])
error<- 1- length(which(pp==train_data[,12]))/nrow(train_data)
error
# [1] 0.3985714

pt<-predict(mod.p,test_data[,1:11])
test_error<- 1- length(which(pt== test_data[,12]))/nrow(test_data)
test_error
#[1] 0.4271357


summary(mod.p)