library(klaR)
setwd('/Users/smadaan/Documents/ML_UCSC/week2/Data')

# read data
wdata <- read.csv('winequality-red.csv', header=TRUE, sep=';')
str(wdata)

# convert wine quality to categorical variable
wdata$quality <- as.factor(wdata$quality)

# apply Naive Bayes classification
mod <- NaiveBayes(quality~.,data = wdata)

# predict based on classifier
qualityHat <- predict(mod, wdata[,1:11])

# calculate error
Err <- 1 - sum(qualityHat$class == wdata$quality)/length(wdata$quality)
Err

# [1] 0.4396498

# are dependent variables correlated
pairs(wdata[,1:11])