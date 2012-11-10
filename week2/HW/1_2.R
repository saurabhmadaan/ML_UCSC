setwd("/Users/smadaan/Documents/ML_UCSC/week2/Data")


##--Q1s----

data<-read.table('table4_8pg199.txt',header=TRUE, sep=',')
df<-data.frame(data[,4:5])
df$Target<-as.factor(df$Target)

fit<-rpart(Target~a3,data=df,control=rpart.control(minsplit=0,minbucket=0,cp=-1,
				maxcompete=0, maxsurrogate=0, usesurrogate=0, 
				xval=0,maxdepth=2))

1-sum(df$Target==predict(fit,df,type="class"))/length(df$Target)

plot(fit)
text(fit)

df1<-data.frame(data[,2:4])
