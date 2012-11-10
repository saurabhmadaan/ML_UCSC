

#Perform Naive Bayes on the Iris Data Set
# 
# Author: PatriciaHoffman
###############################################################################

rm(list=ls())
#install.packages("klaR")
#install.packages("e1071")
library(klaR)

require(klaR)

data(iris)
#have a look at the data
iris
labels(iris)

m <- NaiveBayes(Species ~ ., data = iris)

#have a look at posterior densities as a function of individual variables
data(iris)
mN <- NaiveBayes(Species ~ ., data = iris)
str(mN)
plot(mN)  

#Here's a visualization of the decision boundaries in two dimensions

library(MASS)
data(iris)
partimat(Species ~ ., data = iris, method = "naiveBayes")


#what does a prediction look like?

data(iris)
m <- NaiveBayes(Species ~ ., data = iris)
out <- predict(m)

Err <- 1 - sum(out$class == iris$Species)/length(iris$Species)
Err
#0.04