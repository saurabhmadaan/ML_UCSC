# TODO: Add comment
# 
# Author: PatriciaHoffman
###############################################################################

# 
# Illustration of Basis Expansion

#####################################
x <- seq(from = -1, to = 3, by = 1)
x
y <- c(1.3,0,.07,4.2,8.8)
y
dataIn <- as.data.frame(cbind(x,y))
dataIn
lmmodel <- lm(y~., data = dataIn)
lmmodel

yEst<- lmmodel$coef[1]+lmmodel$coef[2] * x
#ynew <- 0.954 + 1.920*x

error <- sqrt((sum((y-yEst)^2))/length(y))
error   #[1] 1.929327
plot(x,y,
main = "Linear Model")
abline(lmmodel$coef)

# add more terms 

x2 <- x*x
x2

dataIn <- as.data.frame(cbind(x,x2,y))
dataIn
lmmodel <- lm(y~., data = dataIn)
lmmodel

yhat<- predict(lmmodel, newdata = dataIn[,1:2])

plot(x,yhat,col = 2, pch = 35,#)#,type = "l"),
		main = "Linear Model - Squared Terms Added",
		ylab='Estimate and Actual',)
points(x,y, col = "blue")
legend(0,4, c("Estimate", "Actual"), cex = 1, col = c(2, "blue"),
		pch = c(35, 16))

error <- sqrt((sum((y-yhat)^2))/length(y))
error   #[1] 0.3590002

# more resource efficient way to add more terms

#####################################
x <- seq(from = -1, to = 3, by = 1)
x
y <- c(1.3,0,.07,4.2,8.8)
y

# load required packages
require(plyr)
require(reshape)
require(MASS)

addTermsx <- mutate(as.data.frame(x), 
		xx = x*x)
dataIn <- as.data.frame(cbind(addTermsx,y))
dataIn
lmmodel <- lm(y ~. , data = dataIn[,1:2])
lmmodel

#yEst<-lmmodel$coef[1]+lmmodel$coef[2]*x++lmmodel$coef[3]*x*x
#yExt = -0.1789 -0.3457*x +1.1329*x^2

yEst<- predict(lmmodel, newdata = dataIn[,1:2])
error <- sqrt((sum((y-yEst)^2))/length(y))
error  # [1] 0.3590002


plot(x,yEst,col = 2, pch = 35,#)#,type = "l"),
		main = "Linear Model - Squared Terms Added",
		ylab='Estimate and Actual',)
points(x,y, col = "blue")
legend(0,4, c("Estimate", "Actual"), cex = 1, col = c(2, "blue"),
		pch = c(35, 16))


# What happens if we add even more terms?
#####################################

addTermsx <- mutate(as.data.frame(x), 
		xx = x*x, xxx = x*x*x)
dataIn <- as.data.frame(cbind(addTermsx,y))
dataIn
lmmodel <- lm(y ~. , data = dataIn[,1:3])
lmmodel

yEst<- predict(lmmodel, newdata = dataIn[,1:3])
error <- sqrt((sum((y-yEst)^2))/length(y))
error  # [1] 0.3356801


plot(x,yEst,col = 2, pch = 35,#)#,type = "l"),
		main = "Linear Model - Squared & Cubed Terms Added",
		ylab='Estimate and Actual',)
points(x,y, col = "blue")
legend(0,4, c("Estimate", "Actual"), cex = 1, col = c(2, "blue"),
		pch = c(35, 16))

