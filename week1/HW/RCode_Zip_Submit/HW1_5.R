setwd("/Users/smadaan/Documents/ML_UCSC/week1/HW")
data.desert<-read.csv("HW01pb3Desertdata.csv",header=FALSE)

# -- explore the data
class(data.desert)
str(data.desert)
head(data.desert)
names(data.desert)
summary(data.desert)

##--Q1s---- median value

median = function (data.vector)
{
	median=NA;
	l = length(data.vector);
	sv<-sort(data.vector);
	median=ifelse(is.integer(l/2),sv[l/2],sv[l/2+1]);
	return(median);
}


median(data.desert[,1]) 
#89
mean(data.desert[,1]) 
#144.0348
##--Q1e----

##--Q2s----
hist(data.desert[,1], main="Distribution of Desert Home Prices", 
	xlab="Home Price", ylab="Number of Homes")
# also, median < mean, positive/right skewed
##--Q2e----

##--Q3s---- if all data pts increase by 10k
median(data.desert[,1]+10) 
#99
#median also increases by 10k
##--Q3e----

##--Q4s---- if all data doubles
median(data.desert[,1]*2) 
#178
#median also doubles
##--Q4e----





