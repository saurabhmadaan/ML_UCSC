# Expection - Maximization mclust
# 
# Author: PatriciaHoffman
# Expection - Maximization Iris data 97% classified correctly
###############################################################################
#install.packages("mclust")
require(mclust)
help(mclustModelNames)

irisMclust <- Mclust(iris[,-5])
## Plot BIC for various numbers of components and various types of models
#Notice that the best BIC occurs for 2 clusters
plot(irisMclust, data = iris[,-5])

##let's look at the best one based on BIC
irisBIC <- mclustBIC(iris[,-5])

plot(irisBIC)
# yes seperated two clusters well
#    but we've always known that Setosa separates well from the other two species

mclustModel(iris[,-5], irisBIC)

## Does that correspond to the best one in the graph? 
## But that one only picks two components 
##   and it classifies them perfectly
## We know that there are 3.  Let's see how we do if we force 3

irisBIC <- mclustBIC(iris[,-5],G = 3)

#plot(irisBIC,data = iris[,-5])

model <- mclustModel(iris[,-5], irisBIC)
model

index = seq(150)
plot(index, model$z[,1])
plot(index, model$z[,2])
plot(index, model$z[,3])

# By looking at plot 
#  count 5 misclassified examples 
#   - not bad given that the labels are erased.  

irisClass <- rep(0.0, 150)

for(i in 1:150){
	irisClass[i] <- which.max(model$z[i,])
}

irisClass

sum(irisClass[1:50]==1)    # 50 of Setosa Correctly classified
sum(irisClass[51:100]==2)  # 45 of Versicolor Correctly classified  
sum(irisClass[101:150]==3) # 50 of Virginica Correctly classified
145/150 #97% classified correctly

names(model)
# the parameters for the three clusters are given by
model$parameters
#plot(model, data = iris[,-5])


irisSummary <- summary(irisBIC, data = iris[,-5])
irisSummary
#
#classification table:
#		1  2  3 
#      50 45 55 
#
#best BIC values:
#		VEV,3     VVV,3     EEV,3 
#-562.5514 -580.8399 -610.0853 

#pairs plot for Iris Data
clPairs(data = iris[,-5], classification = iris[,5])

#more iris plots
irisBIC <- mclustBIC(iris[,-5],G = 3)
irisSummary3 <- summary(irisBIC, data = iris[,-5], G = 3)
coordProj( data = iris[,-5], dimens = c(2,4), what = "classification",
		parameters = irisSummary3$parameters, z = irisSummary3$z)
coordProj( data = iris[,-5], dimens = c(2,4), what = "uncertainty",
		parameters = irisSummary3$parameters, z = irisSummary3$z)
coordProj( data = iris[,-5], dimens = c(2,4), what = "errors",
		parameters = irisSummary3$parameters, z = irisSummary3$z, truth = iris[,5])
