setwd("/Users/smadaan/Documents/ML_UCSC/week2/Data")
require(rpart)


data<-read.csv('winequality-red.csv',header=TRUE,sep=';')
data$quality<-as.factor(data$quality)


nxval <- 10
ndepth <- 10
trainOutput <- matrix(0.0,nrow = ndepth, ncol = 2)
testOutput <- matrix(0.0,nrow =ndepth, ncol = 2)
I <- seq(from = 1, to = nrow(data))

for(idepth in 1:ndepth){
	trainErr <- 0.0
	testErr <- 0.0
	for(ixval in seq(from =  1, to = nxval)){
		Iout <- which(I%%nxval == ixval%%nxval)
		trainIn <- data[-Iout,]
		trainOut <- data[Iout,]
		yin <- as.factor(trainIn[,12])
		yout <- as.factor(trainOut[,12])
		xin <- trainIn[,1:11]
		xout <- trainOut[,1:11]
		
		fit <- rpart(yin~.,xin,control=rpart.control(maxdepth=idepth))
		dum <- predict(fit,xin)
		yhat <- rep(0.0,nrow(dum))
		for(i in 1:nrow(dum)){
			yhat[i] <- 2*(which.max(dum[i,]) - 1) -1			
		}
		trainErr <- trainErr + (1-sum(yin==yhat)/length(yin))
		dum <- predict(fit,xout)
		dum1 <- predict(fit,xout,type='class')
		yhat <- rep(0.0,nrow(dum))
		for(i in 1:nrow(dum)){
			yhat[i] <- 2*(which.max(dum[i,]) - 1) -1			
		}
		testErr <- testErr + (1-sum(yout==yhat)/length(yout))
	}
	trainOutput[idepth,1] <- idepth
	trainOutput[idepth,2] <- trainErr/nxval
	testOutput[idepth,1] <- idepth
	testOutput[idepth,2] <- testErr/nxval
}

maxval = max(testOutput[,2])
plot(trainOutput, ylim=c(0,maxval),
		main="Model Complexity",
		xlab="Model Complexity = Tree Depth",
		ylab="Prediction Error"
	)
points(testOutput, col = 2)

index <- which.min(testOutput[,2])
testOutput[index,2]
# [1] 0.8174057

index
# 5

#Plot for most complicated tree depth
plot(fit)
text(fit)
print(fit)
'> print(fit)
n= 1440 

node), split, n, loss, yval, (yprob)
      * denotes terminal node

 1) root 1440 820 5 (0.0042 0.032 0.43 0.4 0.13 0.01)  
   2) alcohol< 10.525 887 360 5 (0.0056 0.034 0.59 0.33 0.033 0.0023)  
     4) sulphates< 0.575 357  98 5 (0.0056 0.056 0.73 0.2 0.0084 0) *
     5) sulphates>=0.575 530 262 5 (0.0057 0.019 0.51 0.42 0.049 0.0038)  
      10) total.sulfur.dioxide>=81.5 90  16 5 (0 0.011 0.82 0.16 0.011 0) *
      11) total.sulfur.dioxide< 81.5 440 233 6 (0.0068 0.02 0.44 0.47 0.057 0.0045)  
        22) volatile.acidity>=0.535 206  93 5 (0.0097 0.034 0.55 0.38 0.029 0) *
        23) volatile.acidity< 0.535 234 105 6 (0.0043 0.0085 0.35 0.55 0.081 0.0085) *
   3) alcohol>=10.525 553 276 6 (0.0018 0.029 0.17 0.5 0.28 0.024)  
     6) volatile.acidity>=0.425 301 136 6 (0.0033 0.043 0.23 0.55 0.16 0.013) *
     7) volatile.acidity< 0.425 252 140 6 (0 0.012 0.095 0.44 0.41 0.036)  
      14) sulphates< 0.735 138  66 6 (0 0.022 0.12 0.52 0.3 0.036) *
      15) sulphates>=0.735 114  52 7 (0 0 0.07 0.35 0.54 0.035) *
      '
post(fit,file="")