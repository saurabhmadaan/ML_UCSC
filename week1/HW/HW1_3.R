setwd("/Users/smadaan/Documents/ML_UCSC/week1/HW")
data.ocean<-read.csv("HW01pb3OceanViewdata.csv",header=FALSE)
data.desert<-read.csv("HW01pb3Desertdata.csv",header=FALSE)

# 3.a -- Box plots
boxplot(data.desert[,1], data.ocean[,1],col=c("red","blue"),
	main="House Box Plots: Comparison of House Prices",
	names=c("Desert View","Ocean View"),ylab="Prices (in thousand dollars)")

# 3.b -- Histograms
hist(data.ocean[,1],breaks=seq(from=0,to=3000,by=500),
	 xlab="Price (in thousand dollars)", main="Price Distribution of Ocean-view Houses")

# 3.c -- ecdf plots
plot(ecdf(data.ocean[,1]),xlim=c(0,2500),verticals=T,col="blue",
	 main="Empirical Cumulative Distribution",xlab="Price in Thousands",ylab="ecdf")
lines(ECDF(data.desert[,1]),verticals=T,col="red")
legend(1900,0.8,c("Ocean-view Houses","Desert Houses"),
		lty=c(1,1),col=c("blue","red"))