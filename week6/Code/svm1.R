# SVM Examples
# on Iris data: 
#     use cross validation to tune gama parameter
#  Glass data:
#     compare SVM and rpart
# regression on fabricated data
# 
# Author: PatriciaHoffman
###############################################################################
rm(list=ls())

library(e1071)
library(rpart)




data(iris)
attach(iris)
## classification mode
# default with factor response:
summary(iris)
model <- svm(Species ~ ., data = iris)
print(model)
summary(model)


x <- subset(iris, select = -Species)
y <- Species
# test with train data
pred <- predict(model, x)
# Check accuracy:
table(pred, y)


# visualize (classes by color, SV by crosses):
# cmdscale: principal coordinates analysis
plot(cmdscale(dist(iris[,-5])),
		col = as.integer(iris[,5]),
		pch = c("o","+")[1:150 %in% model$index + 1])

plot(model, iris, Petal.Width ~ Petal.Length,
		slice = list(Sepal.Width = 3, Sepal.Length = 4))
## plot with custom symbols and colors
plot(model, iris, Petal.Width ~ Petal.Length,
		slice = list(Sepal.Width = 2, Sepal.Length = 3))
 
#		svSymbol = "v", dataSymbol = "o", symbolPalette = rainbow(4),
#		color.palette = terrain.colors)




data(iris)
## tune `svm' for classification with RBF-kernel (default in svm),
## using one split for training/validation set
obj <- tune(svm, Species~., data = iris,
		ranges = list(gamma = 2^(-1:1), cost = 2^(2:4)),
		tunecontrol = tune.control(sampling = "cross")
)

summary(obj)
plot(obj)

#- best parameters:                   #- best parameters:
#		gamma cost                    #		   gamma cost
#         0.5    4                    ##        1.0    8
#- best performance: 0.04             #- best performance: 0.02 
#

## classification mode
# default with factor response:
model <- svm(Species ~ ., data = iris, gamma = 0.5, cost = 4)
print(model)
summary(model)
x <- subset(iris, select = -Species)
y <- Species
# test with train data
pred <- predict(model, x)
# Check accuracy:
table(pred, y)

obj <- tune(svm, Species~., data = iris,
		ranges = list(gamma = seq(.5,1.5,0.1), cost = seq(7,9,0.5)),
		tunecontrol = tune.control(sampling = "cross")
)
summary(obj)
plot(obj)
obj$best.parameters

#try it again with best gamma and cost
model <- svm(Species ~ ., data = iris, gamma = 1.0, cost = 8)
print(model)
summary(model)



# test with train data
pred <- predict(model, x)
# Check accuracy:
table(pred, y)

#
#gamma = 1.0, cost = 8
#               y
#pred         setosa versicolor virginica
#setosa         50          0         0
#versicolor      0         49         0
#virginica       0          1        50

#install.packages("mlbench")
data(Glass, package = "mlbench")
#
# http://archive.ics.uci.edu/ml/datasets/Glass%20Identification
#
str(Glass)
Glass$Type
# There are seven classifications 
#   the classification target is Glass$Type
#      -- 1 building_windows_float_processed
#      -- 2 building_windows_non_float_processed
#      -- 3 vehicle_windows_float_processed
#      -- 4 vehicle_windows_non_float_processed (none in this database)
#      -- 5 containers
#      -- 6 tableware
#      -- 7 headlamps
# The 10 attributes include refractive index, various elements(ie Sodium, Magnesium, etc.) 
index <- 1:nrow(Glass)
testindex <- sample(index, trunc(length(index)/3))
testset <- Glass[testindex, ]
trainset <- Glass[-testindex, ]
svm.model <- svm(Type ~ ., data = trainset, cost = 100, gamma = 1)
svm.pred <- predict(svm.model, testset[, -10])
table(pred = svm.pred, true = testset[, 10])
length(testset[,10])
1-sum(svm.pred == testset[,10])/length(testset[,10])
#   0.3239437

rpart.model <- rpart(Type ~ ., data = trainset)
rpart.pred <- predict(rpart.model, testset[, -10], type = "class")
table(pred = rpart.pred, true = testset[, 10])
1-sum(rpart.pred == testset[,10])/length(testset[,10])
#0.3661972

#Let's see if we can improve with parameter selection.
obj <- tune(svm, Type~., data = trainset,
		ranges = list(gamma = 2^(-4:0), cost = 2^(2:4)),
		tunecontrol = tune.control(sampling = "cross"))
summary(obj)
plot(obj)

#
#
#- best parameters:
#		gamma cost
#0.25    8
#
#- best performance: 0.2876190 




#rerun with best values of regularization parameters. 
svm.model <- svm(Type ~ ., data = trainset, cost = 8, gamma = 0.25)
svm.pred <- predict(svm.model, testset[, -10])
table(pred = svm.pred, true = testset[, 10])
length(testset[,10])
1-sum(svm.pred == testset[,10])/length(testset[,10])
#  0.2957746

## try regression mode on two dimensions
# create data
x <- seq(0.1, 5, by = 0.05)
y <- log(x) + rnorm(x, sd = 0.2)


# estimate model and predict input values
m <- svm(x, y)
new <- predict(m, x)


# visualize
plot(x, y, col = 1)
points(x, log(x), col = 2)
points(x, new, col = 4)
legend(3, -1, c("actual y", "log(x)", "predicted"), col = c(1,2,4), pch=1)


