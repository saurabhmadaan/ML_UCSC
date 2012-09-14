setwd("/Users/smadaan/Documents/ML_UCSC/week1/HW")
data.desert<-read.csv("HW01pb3Desertdata.csv",header=FALSE)

# -- explore the data
class(data.desert)
str(data.desert)
head(data.desert)
names(data.desert)
summary(data.desert)

# 5.1 median value

median = function (data.vector)
{
	median=NA;
	l = length(data.vector);
	sv<-sort(data.vector);
	median=ifelse(is.integer(l/2),sv[l/2],sv[l/2+1]);
	return(median);
}

#v<-c(3,4,2,1,6,5,7,10,12)
#median(v)

median(data.desert[,1]) #89
mean(data.desert[,1]) #144.0348
hist(data.desert[,1])
#median < mean, positive/right skewed

#5.2 if all data pts increase by 10k
median(data.desert[,1]+10) #99
#median also increases by 10k

#5.3 if all data doubles
median(data.desert[,1]*2) #178
#median also doubles






