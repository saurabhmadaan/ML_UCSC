setwd("/Users/smadaan/Documents/ML_UCSC/week1/HW")
data.ocean<-read.csv("HW01pb3OceanViewdata.csv",header=FALSE)
data.desert<-read.csv("HW01pb3Desertdata.csv",header=FALSE)

##--Q1s---- Box plots
boxplot(data.desert[,1], data.ocean[,1],col=c("red","blue"),
	main="House Box Plots: Comparison of House Prices",
	names=c("Desert View","Ocean View"),ylab="Prices (in thousand dollars)")
##--Q1e---- Box plots

##--Q2s---- Histograms
hist(data.ocean[,1],breaks=seq(from=0,to=3000,by=500),
	 xlab="Price (in thousand dollars)", main="Price Distribution of Ocean-view Houses")
##--Q2e---- Histograms

##--Q3s---- ecdf plots
plot(ecdf(data.ocean[,1]),xlim=c(0,2500),verticals=T,col="blue",
	 main="Empirical Cumulative Distribution for Houses",xlab="Price in Thousand Dollars",ylab="ECDF")
lines(ecdf(data.desert[,1]),verticals=T,col="red")
legend(1800,0.8,c("Desert","Ocean-view"),
		lty=c(1,1),col=c("red","blue"))##--Q3e----