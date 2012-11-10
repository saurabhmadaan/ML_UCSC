# KMeans - Prototype Based Clustering
#	First on made up data and then on the Iris Data Set
#   kmeans() correctly classifies 89% of the Iris Data
# 
# Author: PatriciaHoffman
###############################################################################
##

rm(list=ls())
require(graphics)
# a 2-dimensional example
#in the following line of r-code, the function rnorm(100,sd = 0.3) generates 
#100 normally distributed random numbers 
#with zero mean and standard deviation (sd) of 0.3
#the function matrix() takes the vector of random numbers from rnorm and arranges it into a 
#matrix with two columns.  that gives us 50 points x,y centered at 0,0.  The second function 
#"matrix" generates another 50 points but these points are centered at 1,1.  the function rbind
#for "row" bind, stacks the two matrices atop one another for the final data matrix.  
x <- rbind(matrix(rnorm(100, sd = 0.3), ncol = 2),
		matrix(rnorm(100, mean = 1, sd = 0.3), ncol = 2))

#give the columns names
colnames(x) <- c("x", "y")

#run the kmeans algorithm on the data set we just built.  
cl <- kmeans(x, 2)

#let's have a look at what we get from the function kmeans
str(cl)

#let's look to see how the clustering agrees with what we know about how these data
#were generated.  
cl$cluster

#how close did the centers come
cl$centers[1,]
cl$centers[2,]

#here's a plot
plot(x, col = cl$cluster)
points(cl$centers, col = 1:2, pch = 8, cex=2)

## random starts do help here with too many clusters
cl <- kmeans(x, 5, nstart = 5)
plot(x, col = cl$cluster)
points(cl$centers, col = 1:5, pch = 8)

#try this with nstart=1
#try using "Hartigan-Wong"

cl <- kmeans(x, 5, nstart = 5,algorithm="Hartigan-Wong")
plot(x, col = cl$cluster)
points(cl$centers, col = 1:5, pch = 8)


#let's try some real data
#here's the iris set
iris

summary(iris)

# Data visualization
# plot in Sepal.Length vs Sepal.Width axes
plot(iris$Sepal.Length, iris$Sepal.Width)
points(iris[1:50,1:2],col = 4)
points(iris[51:100,1:2], col = 2)
points(iris[101:150,1:2],col = 3)

# plot in 2nd columnn vs 3rd column
plot(iris[,2], iris[,3])
points(iris[1:50,2:3],col = 4)
points(iris[51:100,2:3], col = 2)
points(iris[101:150,2:3],col = 3)

# plot in 3rd column vs 4th column
plot(iris[,3], iris[,4])
points(iris[1:50,3:4],col = "red")
points(iris[51:100,3:4], col = "blue")
points(iris[101:150,3:4],col = "green")


#convert the iris classification problem into a clustering problem
#remove the class labels.  normally you wouldn't do this.  why throw 
#away information about class memberships.  we're trying to learn 
#about clustering
x <- iris[-5]
x

#now cluster the resulting unlabelled values
cl<- kmeans(x,3)

#print out the results
cl$cluster
table(iris[,5],cl$cluster)

#             1   2   3
#setosa       0   0  50
#versicolor   2  48   0
#virginica   36  14   0

(50+48+36)/150 # 89%

#Let's look at plots

xx<-(x[1:2])
plot(xx, col = cl$cluster)
points(cl$centers[,1:2], col = 1:5, pch = 8)

xx<- x[2:3]
plot(xx, col = cl$cluster)
points(cl$centers[,2:3], col = 1:5, pch = 8)

xx<- x[3:4]
plot(xx, col = cl$cluster)
points(cl$centers[,3:4], col = 1:5, pch = 8)


# one measure of how tight the cluster is ... withinss
cl$withinss

# another measure
clone <- which(cl$cluster == 1)

distance <- (x[clone, ] - cl$center[1,])^2
distance <- sum(sqrt(apply(distance,1,sum)))
distance <- distance / length(clone)

# here distance is the average distance between
#  the points in the first cluster and the center
#   of that cluster



# Will kmeans do a better job, if data is scaled first?
#try normalizing
#first take out the average values
for(i in 1:4){
	#sum(x)/lengh(x) = average
	x[,i] <- x[,i]-sum(x[,i])/length(x[,i])
}

#next calculate the variance in each dimension and divide out the 
#square root of the variance (the result has var = sd = 1)
#p.s. it's easier to use the function scale() to do this
for(i in 1:4){
	c <- sum(x[,i]*x[,i])/length(x[,i])
	c <- 1/sqrt(c)
	x[,i] <- x[,i]*c
}

cl<- kmeans(x,3,algorithm="Hartigan-Wong")
#print out the results
cl$cluster
#[1] 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3
#[38] 3 3 3 3 3 3 3 3 3 3 3 3 3 1 1 2 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
#[75] 1 1 1 2 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 2 1 2 2 2 2 1 2 2 2 2
#[112] 2 2 1 1 2 2 2 2 1 2 1 2 1 2 2 1 1 2 2 2 2 2 1 2 2 2 2 1 2 2 2 1 2 2 2 1 2
#[149] 2 1


#rate of getting the right answers in first cluster

cl$cluster[1:50]
m1cl1 <- which(cl$cluster[1:50]==1)
m2cl1 <- which(cl$cluster[1:50]==2)
m3cl1 <- which(cl$cluster[1:50]==3)
length(m1cl1)
mc1 <- max(length(m1cl1),length(m2cl1),length(m3cl1))
mc1/50 #[1] 1

#rate of getting the right answers in second cluster
cl$cluster[51:100]
m1cl1 <- which(cl$cluster[51:100]==1)
m2cl1 <- which(cl$cluster[51:100]==2)
m3cl1 <- which(cl$cluster[51:100]==3)
length(m3cl1)
mc2 <- max(length(m1cl1),length(m2cl1),length(m3cl1))
mc2/50 #[1] 0.96

#rate of getting the right answers in third cluster
cl$cluster[101:150]
m1cl1 <- which(cl$cluster[101:150]==1)
m2cl1 <- which(cl$cluster[101:150]==2)
m3cl1 <- which(cl$cluster[101:150]==3)
length(m3cl1)
mc3 <- max(length(m1cl1),length(m2cl1),length(m3cl1))
mc3/50 #[1] .72

(mc1+mc3+mc3)/150 # [1] 0.813

# get cluster means
aggregate(x,by=list(cl$cluster),mean)

#Group.1 Sepal.Length Sepal.Width Petal.Length Petal.Width
#  1         6.85        3.07         5.74       2.071
#  2         5.01        3.43         1.46       0.246
#  3         5.90        2.75         4.39       1.434


# append cluster assignments
newdata <- data.frame(x, cl$cluster)
head(newdata)
newdata[1:5,]
newdata[51:55,]
newdata[101:105,]

#  What about making some plots?
library(cluster)
newiris<-data.frame(iris, cl$cluster)
head(newiris)
clusplot(newiris[1:3],
		cl$cluster,
		color=TRUE,
		shade=TRUE,
		lines=0)

# Another idea to try - - -
#   as kmeans starts with arbitrary centers, run the 
#   algorithm several times and choose the best results
#  Recall that probabality is NOT on our side for this


bestClust = function (my_data, numClust = 2)
{
	ss<- 1000000
	for(i in 1:100){
		km <- kmeans(my_data,numClust)
		if(mean(km$withinss)<=ss){
			ss<- mean(km$withinss)
			km.best<-km
		}
	}
	return(km.best)
}
cl <- bestClust(x,3)

#rate of getting the right answers in first cluster

cl$cluster[1:50]
m1cl1 <- which(cl$cluster[1:50]==1)
m2cl1 <- which(cl$cluster[1:50]==2)
m3cl1 <- which(cl$cluster[1:50]==3)
length(m1cl1);length(m2cl1);length(m3cl1)
mc1 <- max(length(m1cl1),length(m2cl1),length(m3cl1))
mc1/50 #[1] 1

#rate of getting the right answers in second cluster
cl$cluster[51:100]
m1cl1 <- which(cl$cluster[51:100]==1)
m2cl1 <- which(cl$cluster[51:100]==2)
m3cl1 <- which(cl$cluster[51:100]==3)
length(m1cl1);length(m2cl1);length(m3cl1)
mc2 <- max(length(m1cl1),length(m2cl1),length(m3cl1))
mc2/50 #[1] 0.96

#rate of getting the right answers in third cluster
cl$cluster[101:150]
m1cl1 <- which(cl$cluster[101:150]==1)
m2cl1 <- which(cl$cluster[101:150]==2)
m3cl1 <- which(cl$cluster[101:150]==3)
length(m1cl1);length(m2cl1);length(m3cl1)
mc3 <- max(length(m1cl1),length(m2cl1),length(m3cl1))
mc3/50 #[1] .72

(mc1+mc3+mc3)/150 # [1] 0.813

#What about trying simple bisecting kmeans?

#            Easy to do it for Iris Data - 
# This is a kludge way ... 
#            Homework 8 asks
#    you to write a wrapper fuction for kmeans()
#    to work with any data set

#First create two clusters
answers <- iris
x1 <- iris[-5]
cl <- bestClust(x1,2)

#
# add the cluster with the lowest withinss to the list
index1 <- which(min(cl$withinss)==cl$withinss)
index2 <- which(cl$cluster == index1)
answers[index2,6] <- 3

#Break the cluster with highest withinss into two new clusters
x2 <- x1[-index2,]
c2 <- bestClust(x2,2)

answers[-index2,6]<- c2$cluster

#How did it stack up?
table(answers[,5],answers[,6])

#                 1   2   3
#setosa           0   0  50
#versicolor      45   2   3
#virginica       14  36   0

#


# We know there are 3 Iris species.
#   But .... what would be a way to figure out
#   the number of clusters to use?
# Determine number of clusters - One Idea
#   Look for the knee in the curve
nclust <- function(mydata, maxclust=15) {
	wss <- (nrow(mydata)-1)*sum(apply(mydata,2,var)) 
	for (i in 2:maxclust) wss[i] <- sum(kmeans(mydata, 
		centers=i)$withinss) 
    plot(1:15, wss, type="b", col="red", pch=19, 
	xlab="Number of Clusters", 
	ylab="Within groups sum of squares")
}

nclust(x) # 3 looks like the knee

#note $withinss is the
#    within-cluster sum of squares for each cluster.


#An object of class "kmeans" which has a print method and is a list with components:
#		cluster	 A vector of integers (from 1:k) indicating the cluster to which each point is allocated.
#centers:	 A matrix of cluster centres.
#withinss:	 The within-cluster sum of squares for each cluster.
#totss:	     The total within-cluster sum of squares.
#tot.withinss:	 Total within-cluster sum of squares, i.e., sum(withinss).
#betweenss:	 The between-cluster sum of squares.
#size:	     The number of points in each cluster.





#Could also try creating 6 clusters and then trying to aggregate them to 3 clusters
cl <- kmeans(x,6)
cl$cluster
# algorithm = c("Hartigan-Wong", "Lloyd", "Forgy", "MacQueen")


