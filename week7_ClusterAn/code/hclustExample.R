# Hierarchical Clustering - Iris Data
# 
# Author: PatriciaHoffman
# hclust correctly classifies 91% of Iris Data
#  diana correctly classifies 85% of Iris Data
###############################################################################
##########################################
#library(help="stats")
#?stats
help(hclust)
?identify.hclust
?rect.hclust
rm(list=ls())
require(graphics)
str(USArrests)
head(USArrests)
#hclust(d, method = "complete", members=NULL) where 
#  method = the agglomeration method to be used. 
# "ward", "single", "complete", "average", "mcquitty", "median" or "centroid". 
hc <- hclust(dist(USArrests), "ave")
plot(hc)
plot(hc, hang = -1)
USArrests[15,]    # Iowa and New Hampshire are paired early
USArrests[29,]
USArrests[32,]    # New York and Illinois are paired early
USArrests[13,]
memb <- cutree(hc,k=10)
#memb
memb[15];memb[29]; # Iowa & New Hampshire are in cluster 8
memb[32];memb[13]  # New York and Illinois are in cluster 1

memb <- cutree(hc,k=3)
memb[15];memb[29]; # Iowa & New Hampshire are in cluster 3
memb[32];memb[13]  # New York and Illinois are in cluster 1
# Example of drawing the rectangle around 3 clusters
hca <- hclust(dist(USArrests))
plot(hca)
rect.hclust(hca, k=3, border="red")
x <- rect.hclust(hca, h=50, which=c(2,7), border=3:4)
x


## Do the same with centroid clustering and squared Euclidean distance,
## cut the tree into ten clusters and reconstruct the upper part of the
## tree from the cluster centers.
hc <- hclust(dist(USArrests)^2, "cen")
plot(hc)
plot(hc, hang = -1)

memb <- cutree(hc, k = 10)
memb
memb[15];memb[29];memb[32];memb[13]
cent <- NULL
for(k in 1:10){
	cent <- rbind(cent, colMeans(USArrests[memb == k, , drop = FALSE]))
}
hc1 <- hclust(dist(cent)^2, method = "cen", members = table(memb))
opar <- par(mfrow = c(1, 2))
plot(hc, labels = FALSE, hang = -1, main = "Original Tree")
plot(hc1, labels = FALSE, hang = -1, main = "Re-start from 10 clusters")
par(opar)


#what's it look like for iris
x <- iris[-5]
hc <- hclust(dist(x),"ave")
memb <- cutree(hc,k=3)
memb
myData <- as.data.frame(cbind(iris[5],memb))
myTable <- table(myData)
myTable

# Calculate % correct
(max(myTable[1,])+ max(myTable[2,])+ max(myTable[3,]))/nrow(iris)
#[1] 0.9066667

#try distance squared
x <- iris[-5]
hc <- hclust(dist(x)^2,"ave")
memb <- cutree(hc,k=3)
myData <- as.data.frame(cbind(iris[5],memb))
myTable <- table(myData)
myTable

# Calculate % correct
(max(myTable[1,])+ max(myTable[2,])+ max(myTable[3,]))/nrow(iris)
#[1] 0.92

# method = "ward", "single", "complete", "average", "mcquitty", "median" or "centroid".


#diana() is a divisive clustering program that's part of the "cluster" package
#after you've installed the cluster package you can make the programs in that 
#package available to your current workspace with the following line of code

#Description of Data
#A data frame with the percents of votes 
#  given to the republican candidate in presidential elections 
#  from 1856 to 1976. 
#  Rows represent the 50 states, and columns the 31 elections. 

require(cluster)

data(votes.repub)
head(votes.repub)
str(votes.repub)
votes.repub <- na.omit(votes.repub) 
str(votes.repub)
summary(votes.repub)
# metric = manhattan, euclidean
dv <- diana(votes.repub, metric = "manhattan", stand = TRUE)
#
print(dv)
plot(dv)


## Cut into 2 groups:
dv2 <- cutree(as.hclust(dv), k = 2)
table(dv2) # 8 and 42 group members
rownames(votes.repub)[dv2 == 1]



## try it on the iris data
#the variable "x" still holds the unlabelled iris data.  
dv <- diana(x, metric = "manhattan", stand = TRUE)
plot(dv)

#how well does it do?
memb <- cutree(as.hclust(dv), k=3)
memb

myData <- as.data.frame(cbind(iris[5],memb))
myTable <- table(myData)
myTable

# Calculate % correct
(max(myTable[1,])+ max(myTable[2,])+ max(myTable[3,]))/nrow(iris)
#[1] 0.8533333



#try euclidean
dv <- diana(x, metric = "euclidean", stand = TRUE)
memb <- cutree(as.hclust(dv), k=3)

myData <- as.data.frame(cbind(iris[5],memb))
myTable <- table(myData)
myTable

# Calculate % correct
(max(myTable[1,])+ max(myTable[2,])+ max(myTable[3,]))/nrow(iris)
#[1] 0.8133333

#install.packages("clv")
require(clv)
data(iris)
iris.data <- iris[,1:4]
# fix arguments for cls.stab.* function
iter = c(2,3,4,5,6,7,9,12,15)
smp.num = 5
ratio = 0.8
res1 = cls.stab.sim.ind( iris.data, iter, rep.num=smp.num, subset.ratio=0.7)
res2 = cls.stab.opt.assign( iris.data, iter, clust.method=c("hclust","kmeans"))
print(res1)
boxplot(res1$agnes.average$sim.ind)
plot(res2$hclust.single)

