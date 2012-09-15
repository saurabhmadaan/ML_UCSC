setwd("/Users/smadaan/Documents/ML_UCSC/week1/HW")
orange<-as.data.frame(Orange)

##--Q1s---- 
#-- explore the data
head(orange)
levels(orange[,1])
str(orange)
summary(orange)

# 3 variables: Tree(factor), age (numeric), circumference (numeric)
# range for age: 118-1582
# range for circumference: 30-214

#-- plot age vs circumference
plot(orange$age, orange$circumference, xlab="Age of Tree (days since 1968/12/31)", 
	ylab="Trunk Circumference (mm)", pch=20,main="Circumference vs. Age for Trees", 
	col=orange$Tree)
legend('bottomright', legend = levels(factor(orange$Tree)), 
		text.col=seq_along(levels(orange$Tree)), title="Tree Type")
##--Q1e----

##--Q2s---- Cor between age and circ for 1st tree
orange.1<-orange[which(orange$Tree==1),]
cor(orange.1$age,orange.1$circumference)
# [1] 0.9854675
##--Q2e----

##--Q3s----
names(orange)
t.levels<-sort(levels(orange$Tree))

stats<-data.frame(matrix(nrow=length(t.levels),ncol=0))
stats$Tree<-t.levels
stats$COVARIANCE<-as.matrix(by(orange, orange$Tree,
			function(x){cov(x$age,x$circumference)}))			
stats$CORRELATION<-as.matrix(by(orange, orange$Tree,
			function(x){cor(x$age,x$circumference)}))
stats
'
> stats
  Tree COVARIANCE CORRELATION
1    1   22239.83   0.9881766
2    2   22340.07   0.9854675
3    3   30442.81   0.9877376
4    4   34290.45   0.9873624
5    5   37062.62   0.9844610
'
##--Q3e----

#Alt
corrs<-by(orange[,2:3],orange$Tree,function(x){cor(x$age,x$circumference)})
corr.df<-as.data.frame(as.matrix(result))
corr.df$TREE<-rownames(result)
colnames(corr.df)[1]<-"CORRELATION"
corr.df<-as.data.frame(lapply(corr.df,sort))


##--Q4s---- stats for inflated circ
new.stats<-data.frame(matrix(nrow=length(t.levels),ncol=0))
new.stats$Tree<-t.levels
new.stats$COVARIANCE<-as.matrix(by(orange, orange$Tree,
			function(x){cov(x$age,x$circumference+10)}))			
new.stats$CORRELATION<-as.matrix(by(orange, orange$Tree,
			function(x){cor(x$age,x$circumference+10)}))
new.stats
# no change in covariance or correlation
'
> new.stats
  Tree COVARIANCE CORRELATION
1    1   22239.83   0.9881766
2    2   22340.07   0.9854675
3    3   30442.81   0.9877376
4    4   34290.45   0.9873624
5    5   37062.62   0.9844610
'
##--Q4e----

##--Q5s---- stats for double circ
new2.stats<-data.frame(matrix(nrow=length(t.levels),ncol=0))
new2.stats$Tree<-t.levels
new2.stats$COVARIANCE<-as.matrix(by(orange, orange$Tree,
			function(x){cov(x$age,x$circumference*2)}))			
new2.stats$CORRELATION<-as.matrix(by(orange, orange$Tree,
			function(x){cor(x$age,x$circumference*2)}))
new2.stats
'
> new2.stats
  Tree COVARIANCE CORRELATION
1    1   44479.67   0.9881766
2    2   44680.14   0.9854675
3    3   60885.62   0.9877376
4    4   68580.90   0.9873624
5    5   74125.24   0.9844610
'
# covariance doubles, correlation remains same
##--Q5e----

##--Q6s---- stats for circ*(-2)
new3.stats<-data.frame(matrix(nrow=length(t.levels),ncol=0))
new3.stats$Tree<-t.levels
new3.stats$COVARIANCE<-as.matrix(by(orange, orange$Tree,
			function(x){cov(x$age,x$circumference*(-2))}))			
new3.stats$CORRELATION<-as.matrix(by(orange, orange$Tree,
			function(x){cor(x$age,x$circumference*(-2))}))
new3.stats
'
> new3.stats
  Tree COVARIANCE CORRELATION
1    1  -44479.67  -0.9881766
2    2  -44680.14  -0.9854675
3    3  -60885.62  -0.9877376
4    4  -68580.90  -0.9873624
5    5  -74125.24  -0.9844610
'
# new.corr = orig.corr*-2, correlation becomes -ve
##--Q6e----




