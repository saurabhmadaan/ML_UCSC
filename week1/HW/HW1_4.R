setwd("/Users/smadaan/Documents/ML_UCSC/week1/HW")
orange<-as.data.frame(Orange)

# -- explore the data
class(orange)
str(orange)
head(orange)
levels(orange[,1])
names(orange)
summary(orange)

# 4.1 -- plot age vs circumference
plot(orange$age, orange$circumference, xlab="age", ylab="circumference",
	 pch=20,main="Circumference vs. Age for Trees", col=orange$Tree)
#palette()
legend('topright', legend = levels(factor(orange$Tree)), 
		text.col=seq_along(levels(orange$Tree)))

#4.2 Cor between age and circ for 1st tree
orange.1<-orange[which(orange$Tree==1),]
cor(orange.1$age,orange.1$circumference)
# [1] 0.9854675


#4.3 
names(orange)
t.levels<-sort(levels(orange$Tree))

stats<-data.frame(matrix(nrow=length(t.levels),ncol=0))
stats$Tree<-t.levels
stats$COVARIANCE<-as.matrix(by(orange, orange$Tree,
			function(x){cov(x$age,x$circumference)}))			
stats$CORRELATION<-as.matrix(by(orange, orange$Tree,
			function(x){cor(x$age,x$circumference)}))
stats

#Alt
corrs<-by(orange[,2:3],orange$Tree,function(x){cor(x$age,x$circumference)})
corr.df<-as.data.frame(as.matrix(result))
corr.df$TREE<-rownames(result)
colnames(corr.df)[1]<-"CORRELATION"
corr.df<-as.data.frame(lapply(corr.df,sort))


#4.4 stats for inflated circ
new.stats<-data.frame(matrix(nrow=length(t.levels),ncol=0))
new.stats$Tree<-t.levels
new.stats$COVARIANCE<-as.matrix(by(orange, orange$Tree,
			function(x){cov(x$age,x$circumference+10)}))			
new.stats$CORRELATION<-as.matrix(by(orange, orange$Tree,
			function(x){cor(x$age,x$circumference+10)}))
new.stats
# no change in covariance or correlation


#4.5 stats for double circ
new2.stats<-data.frame(matrix(nrow=length(t.levels),ncol=0))
new2.stats$Tree<-t.levels
new2.stats$COVARIANCE<-as.matrix(by(orange, orange$Tree,
			function(x){cov(x$age,x$circumference*2)}))			
new2.stats$CORRELATION<-as.matrix(by(orange, orange$Tree,
			function(x){cor(x$age,x$circumference*2)}))
new2.stats
# covariance doubles, correlation remains same

#4.6 stats for circ*(-2)
new3.stats<-data.frame(matrix(nrow=length(t.levels),ncol=0))
new3.stats$Tree<-t.levels
new3.stats$COVARIANCE<-as.matrix(by(orange, orange$Tree,
			function(x){cov(x$age,x$circumference*(-2))}))			
new3.stats$CORRELATION<-as.matrix(by(orange, orange$Tree,
			function(x){cor(x$age,x$circumference*(-2))}))
new3.stats
# covariance doubles, new.corr = orig.corr*-2





