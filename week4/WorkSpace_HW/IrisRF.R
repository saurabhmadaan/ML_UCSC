summary(iris)
str(iris)
library(randomForest)
head(iris)

testidx <- which(1:length(iris[,1])%%5 == 0)


iris.train<-iris[-testidx,]
iris.test<-iris[testidx,]

rf.model<-randomForest(iris.train[,1:4], iris.train$Species)

1-sum(iris.train$Species==predict(rf.model,iris.train[,1:4]))/length(iris.train[,1])
# 0

1-sum(iris.test$Species==predict(rf.model,iris.test[,1:4]))/length(iris.test[,1])
# [1] 0.06666667
