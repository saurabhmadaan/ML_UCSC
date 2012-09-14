##--Q1s----
setwd("/Users/Saurabh/Documents/ML_UCSC/week1/HW")
data<-read.csv("HW01pb2data.csv",header=FALSE)
str(data)
# 'data.frame':	2000000 obs. of  1 variable:

nrow(data)
#[1] 2000000

# selecting a sample of 10,000 random records
ss<-seq(1,nrow(data))
rand.ind<-sample(ss,10000,replace=F) #set of random indices for subset
small_data<-data[,1][rand.ind]
length(small_data) 
#[1] 10000
##--Q1e----


##--Q2s----
#mean, max and other descriptive stats
mean(small_data) 
#[1] 9.41002
max(small_data) 
#[1] 16.93748
var(small_data) 
#[1] 4.004991
quantile(small_data,0.25) 
#8.079612
##--Q2e----



mean(data)
max(data)
var(data)
quantile(data[,1],0.25)
> mean(data)
'      
V1 
9.451468 
> max(data)
[1] 18.96657
> var(data)
         V1
V1 4.001822
> quantile(data[,1],0.25)
    25% 
8.10388 
'



