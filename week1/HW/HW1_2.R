setwd("/Users/smadaan/Documents/ML_UCSC/week1/HW")
data<-read.csv("HW01pb2data.csv",header=FALSE)
class(data)
str(data)
# 'data.frame':	2000000 obs. of  1 variable:

is.factor(data[,1])
#FALSE
is.numeric(data[,1])
#TRUE

plot(data[,1])
plot(data[,4])

length(data[,1])
#[1] 2000000

nrow(data)
#[1] 2000000

# selecting a sample of 10,000 random records
ss<-seq(1,2000000)
rand.ind<-sample(ss,10000,replace=F)
small_data<-data[,1][rand.ind]
small_data
length(small_data)

#mean, max and other descriptive stats
mean(small_data)
max(small_data)
var(small_data)
quantile(small_data,0.25)
'
> length(small_data)
[1] 10000
> mean(small_data)
[1] 9.41002
> max(small_data)
[1] 16.93748
> var(small_data)
[1] 4.004991
> quantile(small_data,0.25)
     25% 
8.079612 
'
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



