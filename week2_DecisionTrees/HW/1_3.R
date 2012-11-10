setwd("/Users/smadaan/Documents/ML_UCSC/week2/Data")


##--Q1s----

df<-as.data.frame(read.table('table4_8pg199.txt',header=TRUE, sep=','))
df$Target<-as.factor(df$Target)

df1<-data.frame(data[,2:4])
fit1<-rpart(df$Target~.,df1,control=rpart.control(minsplit=0,minbucket=0,cp=-1,
				maxcompete=0, maxsurrogate=0, usesurrogate=0, 
				xval=0,maxdepth=5))
plot(fit1)
text(fit1)

a1<-c(TRUE,TRUE,FALSE,FALSE)
a2<-c(TRUE,FALSE, TRUE ,FALSE)
a3<-c(2.5,5.5,2.5,8.5)
testdf<-as.data.frame(cbind(as.factor(a1),as.factor(a2),a3))

yout<-predict(fit1,testdf, type="class")
#> yout
#1 2 3 4 
#1 0 0 0 
#Levels: 0 1