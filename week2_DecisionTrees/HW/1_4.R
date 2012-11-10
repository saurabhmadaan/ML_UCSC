setwd("/Users/smadaan/Documents/ML_UCSC/week2/Data")

##--Q1s----
A<-c(TRUE,TRUE,TRUE,TRUE,TRUE,FALSE,FALSE,FALSE,TRUE,TRUE)
B<-c(FALSE,TRUE,TRUE,FALSE,TRUE,FALSE,FALSE,FALSE,TRUE,FALSE)
y<-c(1,1,1,0,1,0,0,0,0,0)

df<-as.data.frame(as.matrix(cbind(A,B,y)))

for(i in seq(1:3))
{
	df[,i]<-as.factor(df[,i])
}


#--- use only A
results.A<-as.data.frame(matrix(nrow=5,ncol=2))
colnames(results.A)<-c('tree_depth','error_w_A')
results.A$tree_depth<-seq(1:5)
for(i in seq(1:5))
{
fit<-rpart(y~A,data=df,control=rpart.control(minsplit=0,minbucket=0,cp=-1,
				maxcompete=0, maxsurrogate=0, usesurrogate=0, 
				xval=0,maxdepth=i))

yout<-predict(fit,df[,1],type="class")
which(yout==df$y)
# [1] 1 2 3 5 6 7 8

ll<-length(which(yout==df$y))
#[1] 7
results.A[i,2]=1-ll/length(df[,1])
}

results.A
'
  tree_depth error_w_A
1          1     0.3
2          2     0.3
3          3     0.3
4          4     0.3
5          5     0.3
'



#--- use only B
results.B<-as.data.frame(matrix(nrow=5,ncol=2))
colnames(results.B)<-c('tree_depth','error_w_B')
results.B$tree_depth<-seq(1:5)
for(i in seq(1:5))
{
fit<-rpart(y~B,data=df,control=rpart.control(minsplit=0,minbucket=0,cp=-1,
				maxcompete=0, maxsurrogate=0, usesurrogate=0, 
				xval=0,maxdepth=i))

yout<-predict(fit,df[,1:2],type="class")
which(yout==df$y)
# [1] 2  3  4  5  6  7  8 10

ll<-length(which(yout==df$y))
#[1] 8
results.B[i,2]=1-ll/length(df[,1])
}

results.B
'
> results.B
  tree_depth error_w_B
1          1     0.2
2          2     0.2
3          3     0.2
4          4     0.2
5          5     0.2
'



#--- use both variables
results.AB<-as.data.frame(matrix(nrow=5,ncol=2))
colnames(results.AB)<-c('tree_depth','error_w_AB')
results.AB$tree_depth<-seq(1:5)
for(i in seq(1:5))
{
fit<-rpart(y~A+B,data=df,control=rpart.control(minsplit=0,minbucket=0,cp=-1,
				maxcompete=0, maxsurrogate=0, usesurrogate=0, 
				xval=0,maxdepth=i))

yout<-predict(fit,df[,1:2],type="class")
which(yout==df$y)
# [1] 2  3  4  5  6  7  8 10

ll<-length(which(yout==df$y))
#[1] 8
results.AB[i,2]=1-ll/length(df[,1])
}

results.AB

'> results.AB
  tree_depth error_w_AB
1          1             0.2
2          2             0.2
3          3             0.2
4          4             0.2
5          5             0.2
'

result<-merge(merge(results.A,results.B),results.AB)
'
> result
  tree_depth error_w_A error_w_B error_w_AB
1          1       0.3       0.2        0.2
2          2       0.3       0.2        0.2
3          3       0.3       0.2        0.2
4          4       0.3       0.2        0.2
5          5       0.3       0.2        0.2
'