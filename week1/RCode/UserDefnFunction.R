


# UserDefnFunction
#      Uses Prostrate Data
#      scale()
# 
# Author: PatriciaHoffman
###############################################################################
rm(list=ls())
?source

##################################################

#		User Defined Function - roll die

##################################################
#    ?trunc  (others are ceiling(), floor(), round(x,digits = 0), 
#                                signif(x, digits = 6)
#trunc takes a single numeric argument x and 
#returns a numeric vector containing the integers 
#formed by truncating the values in x toward 0

#     ?runif     runif generates random deviates default - between 0 and 1



rolladie = function (num.sides =6, num.rolls = 1)
{
	simulated.rolls <- trunc(runif(num.rolls)*num.sides+1)
	return(simulated.rolls)
}
rolladie()

rolladie(num.sides =12)

rolladie(num.sides = 12, num.rolls = 10)



##################################################

#		User Defined Function - scale

##################################################

?scale

prostate<-read.csv("ProstateCancerDataESL.csv", sep=",",header=T)
prostate[1:3,]

names(prostate)
dim(prostate)

isTRUE(all.equal(prostate$age,prostate[,3]))

#put predictors into X
X <- prostate[,1:8]
class(X)


prostateScale <- scale(X, center = TRUE, scale = TRUE)
prostateScale[1:3,]

userScale <- function(x, numColm) {
#demean x and bring to unit variance
	for(i in 1:numColm){
		m <- sum(x[,i])
		m <- m/length(x[,i])
		x[,i] <- x[,i]-m
		v <- var(x[,i])
		if(v < 0.0000001) v <- 0.0000001    # include for case v = 0
		x[,i] <- x[,i]/sqrt(v)	# don't want a zero divide
	}
	return(x)
}

prostateUser <- userScale(X,8)
prostateUser[1:3,]

prostateUser <- as.data.frame(prostateUser)
prostateScale <- as.data.frame(prostateScale)


isTRUE(all.equal(prostateUser,prostateScale))

#
#
##    scale training set 
##    some of data are factors
##    scale test set using training parameters
##    the target value is in the first colm 
##    Don't scale the target
#
#
##testScale is matrix
##1	1	1	3
##0	2	1	3
##0	3	1	3
##1	4	1	3
##1	5	1	3
##scaleData is matrix
##1	1	1	3	T	red	5
##0	2	1	3	F	blue	4
##0	3	1	3	T	green	3
##1	4	1	3	T	purple	2
##1	5	1	3	F	yellow	1
#
#
#testunScale <- read.csv("testScale.csv", sep=",",header=F)
#testScale<- scale(testunScale, center = TRUE, scale = TRUE)
#testScale # notice that colms 3 and 4 are now full of NaN
#
#unstrain <- read.csv("scaleData.csv", sep=",",header=F)
#unstest <- unstrain;test<-unstest;train<-unstrain
##scale(unstrain) gives Error  'x' must be numeric
##demean train and bring to unit variance
##   m = means, v = variance, nc = list of numeric colms, scaled data set x
#m <- rep(0.0,ncol(unstrain)); v <-rep(1.0, ncol(unstrain)); nc <- rep(FALSE, ncol(unstrain))
#
#for(i in 1:ncol(unstrain)){
#	if(is.numeric(unstrain[,i])) nc[i] <- TRUE	
#	}
#nc
#nc[1] <- FALSE     # don't want to scale the target
#
#index <- which(nc == TRUE)
#for(i in index){
#		m[i]<- sum(unstrain[,i])
#		m[i] <- m[i]/length(unstrain[,i])
#		train[,i] <- unstrain[,i]-m[i]
#		v[i] <- var(train[,i])
#		if(v[i] < 0.1) v[i] <- 1
#		train[,i] <- train[,i]/sqrt(v[i])	
#	}
#
#index <- which(nc == TRUE)
#test[,index] <- (unstest[,index]-m[index]) / sqrt(v[index])
#
#train
#test
#nc
#m
#v

##################################################

#		User Defined Function - MATLAB backslash function

#                    compare with r function lm

##################################################


#
#As a second example, consider a function to emulate directly the Matlab backslash command,
#which returns the coefficients of the orthogonal projection of the vector y onto the column
#space of the matrix, X. (This is ordinarily called the least squares estimate of the regression
#coefficients.) This would ordinarily be done with the qr() function; however this is sometimes
#a bit tricky to use directly and it pays to have a simple function such as the following to use it
#safely.
#Thus given a n by 1 vector y and an n by p matrix X then X y is defined as (X^(T) X)^(-1) X^(T) y,
# where (XTX)^(-1) is a generalized inverse of X^(')X.

?qr

bslash <- function(X, y) {
	X <- qr(X)
	qr.coef(X, y)
}


#put response into Y
y <- prostate[,9]


# After the function bslash is created, it may be used in statements such as
regcoeff <- bslash(X, y)

?lm

regressionModel <- lm(y~. , X)

#  classical ordinary least squares
#beta <- solve(t(x) %*% x) %*% t(x) %*% y


##################################################

#		Examine a Model Object
#                    

##################################################

## EXAMPLE, ADDRESSING AN OBJECT

summary(regressionModel)

str(regressionModel)

regressionModel$call

regressionModel$model$y


## LITTLE MORE COMPLEX

attr(regressionModel$model, "terms")

attr(attr(regressionModel$model, "terms"), "term.labels")


## MAYBE USEFUL

quantile(regressionModel$residuals)



multipleReturn = function (x =6, y = 1)
{
	variabley <- x+y
	variablex <- 10
	z <- cbind(variablex,variabley)
	return(z)
}
z <- multipleReturn()
z
answerx <- z[1]
answery <- z[2]
answerx;answery

