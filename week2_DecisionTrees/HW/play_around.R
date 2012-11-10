setwd("/Users/smadaan/Documents/ML_UCSC/week2/Data")



data<-read.csv('sonar_test.csv',header=FALSE, sep=',')
data[,61]<-as.factor(data[,61])

