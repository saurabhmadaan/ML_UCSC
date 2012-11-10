library(klaR)
setwd('/Users/smadaan/Documents/ML_UCSC/week2/Data')

wdata <- read.csv('winequality-red.csv', header=TRUE, sep=';')
wdata$quality <- as.factor(wdata$quality)

str(wdata)

mod <- NaiveBayes(quality~.,data = wdata)

qualityHat <- predict(mod, wdata)

Err <- 1 - sum(qualityHat$class == wdata$quality)/length(wdata$quality)
Err

# [1] 0.4396498
