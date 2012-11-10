library(MASS)

# -- Prepare the data, assign numeric values to the categorical variables
data<-iris
levels(data$Species) = c(0,-1,1)
data$Species.num <- as.numeric(levels(data$Species))[data$Species]
data <- subset(data, select = -c(Species))
data<-data[51:150,]

l <- seq(1:nrow(data))
Err <- matrix(nrow = 21, ncol = 3)

for(iLambda in seq(from = 0, to = 20)){
	#
	exp <- (+3 -4*(iLambda/20))
	xlambda <- 10^exp
	
	TrainErr = 0
	TestErr = 0

for(i in seq(1,10)){
	testidx <- which(l%%10 == i-1)
	data.train<-data[-testidx,]
	data.test<-data[testidx,]
	
	mod.lm <- lm.ridge(Species.num~.,data.train, lambda= xlambda)
	
	yest.train <- drop(as.matrix(cbind(1, data.train[,1:4])) %*% as.matrix(coef(mod.lm)))
	yest.test <- drop(as.matrix(cbind(1, data.test[,1:4])) %*% as.matrix(coef(mod.lm)))
	
	TrainErr <- TrainErr + (1 - sum(yest.train*data.train$Species.num>0)/nrow(data.train))/10
	TestErr <- TestErr  + (1 - sum(yest.test*data.test$Species.num>0)/nrow(data.test))/10
}

	Err[(iLambda+1),1] = TrainErr
	Err[(iLambda+1),2] = TestErr
	Err[(iLambda+1),3] = xlambda
}


plot(Err[,1], type='p', col='red', ylim=c(0,1),
		main = 'Error vs Log(Lambda)',
		ylab='Error',
		xlab='3 - Log(Lambda)')
points(Err[,1], pch=15, col='red')
#lines(out[,2], type='l', col='blue')
points(Err[,2], pch=16, col='blue')
legend(5, 0.4, c("TRAIN", "TEST"), cex = 1, col = c("red", "blue"),
		pch = c(15, 16), lty = 1:2)

Err
min(Err[,2])   # the test error = [1] 0.1

mindex <- which(min(Err[,2]) == Err[,2])
xlambda <- Err[mindex,10]

mod.fin <- lm.ridge(Species.num~.,data,lambda=xlambda)
yest <- drop(as.matrix(cbind(1, data[,1:4])) %*% as.matrix(coef(mod.fin)))
# > range(yest)
# [1] -1.61365  1.98629

trainErr <- 1-sum(data$Species.num*yest>0)/nrow(as.matrix(data))   
# 0.03

# Generate ROC curve
p <- sum(data$Species>0)
n <- sum(data$Species<0)
tp <- c(rep(0,100))
fp <- c(rep(0,100))



# As the value of yest is varying from -1.61365 to 1.98629, 
#     to generate the the ROC curve the threshold
#     needs to vary through these values.

for(i in 1:100){
	thresh <- 2 - i*0.04
	y <- yest>=thresh
	tp[i] <- sum(y & (data$Species>0))/p
	fp[i] <- sum(y & (data$Species<0))/n
	
}

plot(fp,tp)
abline(0,1)





# simple linear regression
'
mod <- lm(Species.num~.,data.train)
yhat.train <- predict(mod, data.train)
yhat.test <- predict(mod, data.test)

TrainErr <- 1 - sum(yhat.train*data.train$Species.num>0)/nrow(data.train)
# [1] 0.0375
TestErr <- 1 - sum(yhat.test*data.test$Species.num>0)/nrow(data.test)
# [1] 0.05
'




