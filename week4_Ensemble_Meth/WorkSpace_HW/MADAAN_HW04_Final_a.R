##########################################################################
# Homework04 R-Code
##########################################################################

##########################################################################
# Set up working directory and clear data
##########################################################################
setwd("~/Documents/UCSC_ML_DataMining/Resources/DataFiles")
getwd()

rm(list=ls())

#############################################################################
####################################  1  ####################################
#############################################################################

###########  1a     ###########
#  load randomForest package
library(randomForest)
STrain <-read.csv("sonar_train.csv",header=FALSE)
STest<-read.csv("sonar_test.csv",header=FALSE)
Sonar <- rbind(STrain,STest)   # Row-wise Merge of both files

y<-as.factor(STrain[,61])
x<-STrain[,1:60]
y_test<-as.factor(STest[,61])
x_test<-STest[,1:60]
fit<-randomForest(x,y)
plot(fit, log="y", main="Random Forest Fit Error")
varImpPlot(fit, main="Variable importance as measured by a Random Forest")
trainErr <- 1-sum(y==predict(fit,x))/length(y)
#[1] 0
testErr <- 1-sum(y_test==predict(fit,x_test))/length(y_test)
#[1] 0.1410256

#######################
##############  1b  ###################
# Use rpart()  to generate 10 different trees with a depth of two on 15 randomly selected
# Sonar atributes, followed by lm.ridge(). Do this for 51 lambda's from 4 to -1 in increment of 0.1
# The code here is based on the BabyEnsemble.R example
#
library(MASS)
library("rpart")
x <- Sonar[,1:60]
y <- Sonar[,61]

xNew <- NULL

Icol <- 1:60
Irow <- 1:nrow(x)
nxval <- 10

# nColSamp <- 15
nColSamp <- 2
# Creat a 15 x 10 matrix for 10 models of 15 random attributes
# Each column will contain 15 rows with attribute #.
IndexMat <- matrix(0.0,nColSamp,nxval)

set.seed(1)
for(icol in 1:nxval){
  Isamp <- sort(sample(Icol,nColSamp))
  IndexMat[,icol] <- Isamp
}

exponent <- seq(from=4, to=-1, by=-0.1)   # for Lambda values for Ridge Regression
Err <-matrix(0.0, length(exponent),3)

#now form the 10 models inside the x-validation loop instead of outside
for(ilambda in 1:length(exponent)){
  xlambda <- 10^(exponent[ilambda])
  testErr <- 0.0
  trainErr <- 0.0
  for(ixval in 1:nxval){   # nxval = 10
    Iout <- which(Irow%%nxval==ixval%%nxval)
    xNewOut <- NULL
    xNewIn <- NULL
    xFullOut <- x[Iout,]
    xFullIn <- x[-Iout,]
    Yin <- y[-Iout]
    Yout <- y[Iout]
    for(icol in 1:nxval){    # nxval = 10
      nColSamp <- IndexMat[,icol]
      xTempIn <- xFullIn[,nColSamp]
      xTempOut <- xFullOut[,nColSamp]
      dataSet <- cbind(xTempIn,Yin)
      
#  Use rpart to get model
      
#     fit<-rpart(Yin~.,data=dataSet,control=rpart.control(minsplit=0,minbucket=0,cp=-1,
#               maxcompete=0, maxsurrogate=0, usesurrogate=0, 
#               xval=0,maxdepth=2))
#  Change parameters for rpart() since the previous rpart() parameters don't work here.
#
      fit<-rpart(Yin~.,data=dataSet,
                 method = 'class', 
                 parms=list( split='gini'))
      
      xNewIn <- cbind(xNewIn,predict(fit,xTempIn))
      xNewOut <- cbind(xNewOut,predict(fit,xTempOut))
    }
    
    Xin <- as.matrix(xNewIn)
    Xout <- as.matrix(xNewOut)
    
    #########  Ridge Regression  ################
    mod <- lm.ridge(Yin~Xin,lambda=xlambda)     

    C <- mod$coef/mod$scales
    XM <- Xin
    for(i in seq(from = 1, to = ncol(Xin))){
      XM[,i]<-Xin[,i]-mod$xm[i]
    } 
    xTemp <- as.matrix(XM)
    A <- as.array(C)
    Yh <- xTemp%*%A + mod$ym
    trainErr <- trainErr + sum(Yh*Yin<0)/(nrow(as.matrix(Yin))*10)  
    XM <- Xout
    for(i in seq(from = 1, to = ncol(Xout))){
      XM[,i]<-Xout[,i]-mod$xm[i]
    } 
    xTemp <- as.matrix(XM)
    A <- as.array(C)
    Yh <- xTemp%*%A +mod$ym
    testErr <- testErr + sum(Yh*Yout<0)/(nrow(as.matrix(Yout))*10)		
  }
  Err[ilambda,1] <- testErr
  Err[ilambda,2] <- trainErr
  Err[ilambda,3] <- xlambda
}

minimum <- min(Err[,2])
maximum <- max(Err[,1])
plot(Err[,1],col="red2", ylim=c(minimum,maximum))
points(Err[,2],col="blue")
min(Err[,1])


#############################################################################
####################################  2  ####################################
# 2. Saurabh's version
#############################################################################


library(MASS)

# -- Prepare the data, assign numeric values to the categorical variables
iris <- read.table("irisdata.csv", sep = ",", header = FALSE)
data<-iris
names(iris)[5] <- "Species"
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

####################################  2  ####################################
# 2. Huong's version
#############################################################################

library(MASS)
iris_data <- read.table("irisdata.csv", sep = ",", header = FALSE)
summary(iris_data)        
str(iris_data)        # View full iris data

iris_sub = iris_data[51:150,]      # Input data rows 51 through 150
summary(iris_sub)        
str(iris_sub)       # View iris subset rows 51 - 150

nycol = ncol(iris_sub)            # number of columns [1] 5
nxcol = nycol - 1                 # number of columns - 1 [1] 4

names(iris_sub)[nycol] = 'Y'        # Name the last column Y
iris_sub$Y = as.numeric(iris_sub$Y)
iris_sub$Y[iris_sub$Y == 2] = rep(-1, length(iris_sub$Y[iris_sub$Y==2]))  ### Set 2 value to -1
iris_sub$Y[iris_sub$Y == 3] = rep( 1, length(iris_sub$Y[iris_sub$Y==3]))  ### Set 3 value to 1

Err <- matrix(nrow = 21, ncol = 3)
I <- seq(1:nrow(iris_sub))

for(iLambda in seq(from = 0, to = 20)){
  # exp <- (+3 -4*(iLambda/20))
  exp <- (+2 -4*(iLambda/20))
  xlambda <- 10^exp
  
  testErr <- 0.0
  trainErr <- 0.0
  
  for(ixval in seq(from = 1, to = 10)){
    Iout <- which(I%%10 == ixval - 1)
    iris_in <- iris_sub[-Iout,]
    iris_out <- iris_sub[Iout,]
    Xin <- iris_in[,1:4]
    Xout <- iris_out[,1:4]
    Yin <- iris_in[,5]
    Yout <- iris_out[,5]
    dataIn <- cbind(Xin,Yin)
    mod <- lm.ridge(Yin~.,data=dataIn,lambda=xlambda)
    C <- mod$coef/mod$scales
    XM <- Xin

    for(i in seq(from = 1, to = ncol(Xin))){
      XM[,i]<-Xin[,i]-mod$xm[i]
    } 
    
    X <- as.matrix(XM)
    A <- as.array(C)
    Yh <- X%*%A + mod$ym
    trainErr <- trainErr + sum(abs(Yin - Yh))/(nrow(as.matrix(Yin))*10)    
    XM <- Xout
    
    for(i in seq(from = 1, to = ncol(Xout))){
      XM[,i]<-Xout[,i]-mod$xm[i]
    } 
    
    X <- as.matrix(XM)
    A <- as.array(C)
    Yh <- X%*%A + mod$ym
    testErr <- testErr + sum(abs(Yout - Yh))/(nrow(as.matrix(Yout))*10)
  }
  
  Err[(iLambda+1),1] = trainErr
  Err[(iLambda+1),2] = testErr
  Err[(iLambda+1),3] = xlambda
}

plot(Err[,1], type='p', col='red', ylim=c(0,1),
     main = 'Error vs Log(Lambda)',
     ylab='Error',
     xlab='3 - Log(Lambda)')
points(Err[,1], pch=15, col='red')
#lines(out[,2], type='l', col='blue')
points(Err[,2], pch=16, col='blue')
legend(5, 0.2, c("TRAIN", "TEST"), cex = 1, col = c("red", "blue"), pch = c(15, 16), lty = 1:2)


# Use the best lambda and all the data to create the production model
mindex <- which(min(Err[,2]) == Err[,2])
xlambda <- Err[mindex,3]

Xin <- iris_sub[,1:4]
Yin <- iris_sub[,5]
mod <- lm.ridge(Y~.,data=iris_sub,lambda=xlambda)
C <- mod$coef/mod$scales
XM <- Xin

for(i in seq(from = 1, to = ncol(Xin))){
  XM[,i]<-Xin[,i]-mod$xm[i]
} 

X <- as.matrix(XM)
A <- as.array(C)
Yh <- X%*%A + mod$ym
trainErr <- sum(abs(Yin - Yh))/nrow(as.matrix(Yin))    
# [1] 0.373121

# Use the production model to produce the ROC curve

YinP <- Yin >0
YinN <- Yin<0
YhP <- Yh>0
YhN<- Yh<0

##calculate misclassification error with threshold 0.0
sum(YinP == YhP)/length(Yin)
# [1] 0.97

#Generate ROC curve
p <- sum(YinP)
n <- sum(YinN)
tp <- c(rep(0.0,100))
fp <- c(rep(0.0,100))

for(i in 1:100){
  thresh <- 2 - i*0.04
  y <- Yh>=thresh
  tp[i] <- sum(y & YinP)/p
  fp[i] <- sum(y & YinN)/n 
}

plot(fp,tp)
abline(0,1)


#############################################################################
####################################  3  ####################################
#############################################################################

dataWine<-read.csv("winequality-red.csv",sep=";",header=T)
dataTrain<-dataWine[1:1400,];
dataTest<-dataWine[1401:1599,];


gbmFit<-gbm(quality~.,data=dataTrain,distribution="gaussian",n.trees=3000, shrinkage=0.005, interaction.depth=3, bag.fraction=0.5, train.fraction=0.5,cv.folds=5)

best.iter <- gbm.perf(gbmFit,method="cv")
print(best.iter)

summary(gbmFit,n.trees=best.iter)

q.predict<-predict.gbm(gbmFit,dataTest,best.iter)
q.predict<-round(q.predict)
errTest<-1-sum(q.predict==dataTest$quality)/length(dataTest$quality)
print(errTest)


#############################################################################
####################################  4  ####################################
#############################################################################

require(MASS)
iris <- read.table("irisdata.csv",sep = ",",header = FALSE)
str(iris)
### Create a data frame with 14 attributes, first four are as given followed by
### V1*V1, V1*V2, V1*V3, V1*V4, V2*V2, V2*V3, V2*V4, V3*V3, V3*V4, V4*V4
iris_df <- cbind(iris$V1,iris$V2,iris$V3,iris$V4,iris$V1*iris$V1,
                 iris$V1*iris$V2, iris$V1*iris$V3, iris$V1*iris$V4, iris$V2*iris$V2,
                 iris$V2*iris$V3, iris$V2*iris$V4, iris$V3*iris$V3, iris$V3*iris$V4,
                 iris$V4*iris$V4,iris$V5)
iris_df

### Create three separate label vectors, one for each class
Y1 <- c(rep(-1,150))
Y2 <- Y1
Y3 <- Y2
Y1[1:50] <- 1
Y2[51:100] <- 1
Y3[101:150] <- 1

# perform cross-validated ridge regression for each of these
#
help(lm.ridge)
??lm.ridge
str(iris_df)

# Train on Y1  ******************************************
#********************************************************
X <- iris_df[,-15]
str(X)
I <- 1:150
Err <- matrix(nrow = 21, ncol = 3)
for(iLambda in seq(from = 0, to = 20)){
  #
  exp <- (+2 -4*(iLambda/20))
  xlambda <- 10^exp
  testErr <- 0.0
  trainErr <- 0.0
  for(ixval in seq(from = 1, to = 10)){
    Iout <- which(I%%10 == ixval - 1)
    Xin <- X[-Iout,]
    Xout <- X[Iout,]
    Yin <- Y1[-Iout]
    Yout <- Y1[Iout]
    dataIn <- as.data.frame(cbind(Xin,Yin))
    mod <- lm.ridge(Yin~.,data=dataIn,lambda=xlambda)
    #str(mod)
    C <- mod$coef/mod$scales
    XM <- Xin
    for(i in seq(from = 1, to = ncol(Xin))){
      XM[,i]<-Xin[,i]-mod$xm[i]
    }
    XX <- as.matrix(XM)
    XX
    A <- as.array(C)
    Yh <- XX%*%A + mod$ym
    Yh
    YhP <- which(Yh>=0.0)
    Yp <- which(Yin>= 0.0)
    trainErr <- trainErr + sum(YhP != Yp)/(length(Yin)*10)
    XM <- Xout
    for(i in seq(from = 1, to = ncol(Xout))){
      XM[,i]<-Xout[,i]-mod$xm[i]
    }
    XX <- as.matrix(XM)
    A <- as.array(C)
    Yh <- XX%*%A + mod$ym
    YhP <- Yh>=0.0
    Yp <- Yout>= 0.0
    testErr <- testErr + sum(YhP != Yp)/(length(Yout)*10)
  }
  Err[(iLambda+1),1] = trainErr
  Err[(iLambda+1),2] = testErr
  Err[(iLambda+1),3] = xlambda
}
Err
plot(Err[,1], type='p', col='red', ylim=c(0,.02),
     main = 'Error vs Log(Lambda)',
     ylab='Error Y1',
     xlab='3 - Log(Lambda)')
points(Err[,1], pch=15, col='red')
#lines(out[,2], type='l', col='blue')
points(Err[,2], pch=16, col='blue')
legend(5, 0.02, c("TRAIN", "TEST"), cex = 1, col = c("red", "blue"),
       pch = c(15, 16), lty = 1:2)

# Train on Y2 ************************
#*******************************************************************************
X <- iris_df[,-15]
I <- 1:150
Err <- matrix(nrow = 21, ncol = 3)


for(iLambda in seq(from = 0, to = 20)){
  #
  exp <- (+2 -4*(iLambda/20))
  xlambda <- 10^exp
  
  testErr <- 0.0
  trainErr <- 0.0
  
  for(ixval in seq(from = 1, to = 10)){
    Iout <- which(I%%10 == ixval - 1)
    #Xin <- X[-Iout,]
    Xin <- X[-Iout,]
    Xout <- X[Iout,]
    Yin <- Y2[-Iout]
    Yout <- Y2[Iout]
    dataIn <- as.data.frame(cbind(Xin,Yin))
    mod <- lm.ridge(Yin~.,data=dataIn,lambda=xlambda)
    C <- mod$coef/mod$scales
    XM <- Xin
    for(i in seq(from = 1, to = ncol(Xin))){
      XM[,i]<-Xin[,i]-mod$xm[i]
    }
    XX <- as.matrix(XM)
    A <- as.array(C)
    Yh <- XX%*%A + mod$ym
    YhP <- Yh>=0.0
    Yp <- Yin>= 0.0
    #trainErr <- trainErr + sum(abs(Yin - Yh))/(nrow(as.matrix(Yin))*10)   
    trainErr <- trainErr + sum(YhP != Yp)/(length(Yin)*10)
    XM <- Xout
    for(i in seq(from = 1, to = ncol(Xout))){
      XM[,i]<-Xout[,i]-mod$xm[i]
    }
    XX <- as.matrix(XM)
    A <- as.array(C)
    Yh <- XX%*%A + mod$ym
    YhP <- Yh>=0.0
    Yp <- Yout>= 0.0
    testErr <- testErr + sum(YhP != Yp)/(length(Yout)*10)
  }
  Err[(iLambda+1),1] = trainErr
  Err[(iLambda+1),2] = testErr
  Err[(iLambda+1),3] = xlambda
}
Err
plot(Err[,1], type='p', col='red', ylim=c(0,0.6),
     main = 'Error vs Log(Lambda)',
     ylab='Error Y2',
     xlab='3 - Log(Lambda)')
points(Err[,1], pch=15, col='red')
#lines(out[,2], type='l', col='blue')
points(Err[,2], pch=16, col='blue')
legend(5, 0.2, c("TRAIN", "TEST"), cex = 1, col = c("red", "blue"),
       pch = c(15, 16), lty = 1:2)

# Step 6 - train on Y3  *********************************
#*************************************************************
X <- iris_df[,-15]
I <- 1:150
Err <- matrix(nrow = 21, ncol = 3)


for(iLambda in seq(from = 0, to = 20)){
  #
  exp <- (+2 -4*(iLambda/20))
  xlambda <- 10^exp
  
  testErr <- 0.0
  trainErr <- 0.0
  
  for(ixval in seq(from = 1, to = 10)){
    Iout <- which(I%%10 == ixval - 1)
    #Xin <- X[-Iout,]
    Xin <- X[-Iout,]
    Xout <- X[Iout,]
    Yin <- Y3[-Iout]
    Yout <- Y3[Iout]
    dataIn <- as.data.frame(cbind(Xin,Yin))
    mod <- lm.ridge(Yin~.,data=dataIn,lambda=xlambda)
    C <- mod$coef/mod$scales
    XM <- Xin
    for(i in seq(from = 1, to = ncol(Xin))){
      XM[,i]<-Xin[,i]-mod$xm[i]
    }
    XX <- as.matrix(XM)
    A <- as.array(C)
    Yh <- XX%*%A + mod$ym
    YhP <- Yh>=0.0
    Yp <- Yin>= 0.0
    #trainErr <- trainErr + sum(abs(Yin - Yh))/(nrow(as.matrix(Yin))*10)   
    trainErr <- trainErr + sum(YhP != Yp)/(length(Yin)*10)
    XM <- Xout
    for(i in seq(from = 1, to = ncol(Xout))){
      XM[,i]<-Xout[,i]-mod$xm[i]
    }
    XX <- as.matrix(XM)
    A <- as.array(C)
    Yh <- XX%*%A + mod$ym
    YhP <- Yh>=0.0
    Yp <- Yout>= 0.0
    testErr <- testErr + sum(YhP != Yp)/(length(Yout)*10)
  }
  Err[(iLambda+1),1] = trainErr
  Err[(iLambda+1),2] = testErr
  Err[(iLambda+1),3] = xlambda
}
Err
plot(Err[,1], type='p', col='red', ylim=c(0,1),
     main = 'Error vs Log(Lambda)',
     ylab='Error Y3',
     xlab='3 - Log(Lambda)')
points(Err[,1], pch=15, col='red')
#lines(out[,2], type='l', col='blue')
points(Err[,2], pch=16, col='blue')
legend(5, 0.2, c("TRAIN", "TEST"), cex = 1, col = c("red", "blue"),
       pch = c(15, 16), lty = 1:2)


# Step 5.6 - Fit all three and calculate misclassification error
xlambda <- 0.01
Xin_1 <- X
Yin_1 <- Y1
dataIn <- as.data.frame(cbind(Xin_1,Yin_1))
mod <- lm.ridge(Yin_1~.,data=dataIn,lambda=xlambda)
C_1 <- mod$coef/mod$scales
XM_1 <- Xin_1
for(i in seq(from = 1, to = ncol(Xin_1))){
  XM_1[,i]<-Xin_1[,i]-mod$xm[i]
}
XX_1 <- as.matrix(XM_1)
A_1 <- as.array(C_1)
Yh1 <- XX_1%*%A_1 + mod$ym
Yh1

xlambda <- 0.025
Xin_2 <- X
Yin_2 <- Y2
dataIn <- as.data.frame(cbind(Xin_2,Yin_2))
mod <- lm.ridge(Yin_2~.,data=dataIn,lambda=xlambda)
C_2 <- mod$coef/mod$scales
XM_2 <- Xin_2
for(i in seq(from = 1, to = ncol(Xin_2))){
  XM_2[,i]<-Xin_2[,i]-mod$xm[i]
}
XX_2 <- as.matrix(XM_2)
A_2 <- as.array(C_2)
Yh2 <- XX_2%*%A_2 + mod$ym
Yh2

xlambda <- 0.01
Xin_3 <- X
Yin_3 <- Y3
dataIn <- as.data.frame(cbind(Xin_3,Yin_3))
mod <- lm.ridge(Yin_3~.,data=dataIn,lambda=xlambda)
C_3 <- mod$coef/mod$scales
XM_3 <- Xin_3
for(i in seq(from = 1, to = ncol(Xin_2))){
  XM_3[,i]<-Xin_3[,i]-mod$xm[i]
}
XX_3 <- as.matrix(XM_3)
A_3 <- as.array(C_3)
Yh3 <- XX_3%*%A_3 + mod$ym
Yh3

#positive predictions by class
Y1h <- (Yh1 > Yh2) & (Yh1 > Yh3)
Y2h <-(Yh2> Yh1) & (Yh2>Yh3)
Y3h <- (Yh3> Yh2) & (Yh3>Yh1)

Y1h
Y2h
Y3h
#number of correct positives in each class
sum(Y1>0 & Y1h)
sum(Y2>0 & Y2h)
sum(Y3>0 & Y3h)
#
#> sum(Y1>0 & Y1h)
#[1] 50
#> sum(Y2>0 & Y2h)
#[1] 49
#> sum(Y3>0 & Y3h)
#[1] 48


#############################################################################
####################################  5  ####################################
#############################################################################
'
5) This is a multi-class problem. Consider the Glass Identification Data
  Set from the UC Irvine Data Repository. The Data is located at the web
  site:
   
    http://archive.ics.uci.edu/ml/datasets/Glass%2BIdentification
 
  This problem will only work with building and vehicle window glass
  (classes 1,2 and 3), so it only uses the first 163 rows of data.
  (Ignore rows 164 through 214) With this set up this is a three class
  problem.

  Use ridge regression to classify this data into the three
  classes: building windows float processed, building windows non float
  processed, and vehicle windows float processed.
' 
#############################################################################

library(MASS)

##########################################################################
### Applies the given model (mod) to the input data X and
### returns Y-Hat
##########################################################################
applyRRModel <- function(mod, X) {
    C <- mod$coef/mod$scales
    XM <- X
    for(i in seq(from = 1, to = ncol(X))){
        XM[,i] <- X[,i] - mod$xm[i]
    }
    XX  <- as.matrix(XM)
    A   <- as.array(C)
    Yh  <- XX %*% A + mod$ym
    return( Yh )
}

##########################################################################
### Creates a RR model, and calculates Y-Hat on the training data.
##########################################################################
calcRRModelAndYHat <- function(X, Y, lambda) {
    dataIn <- cbind(X ,Y)
    mod <- lm.ridge(Y ~ . , data = dataIn, lambda = lambda)
    Yh = applyRRModel(mod, X)
    retList = list( yhat = Yh, mod = mod )
    return( retList )
}

##########################################################################
# Set up data
##########################################################################

### Load the glass data then remove the un-needed rows as specfied.
glassdf = read.csv("glass.data.txt", header = FALSE, sep=',', quote="\"")
# Just grab first 163 rows.
glassdf = glassdf[1:163,]
ncols = ncol(glassdf)
# Name the last column GlassClass
names(glassdf)[ncols] = 'GlassClass'

### Make a list of values
glassClassValues <- sort( unique( glassdf$GlassClass ) )
nclassvals       <- length( glassClassValues )

### Create a matrix of coefficients for DEBUGGING
coefres = matrix( nrow = ncols, ncol = nclassvals )
colnames( coefres ) = glassClassValues

from_lambda =  0
to_lambda   = 30
nlambdavals = to_lambda - from_lambda + 1
Err = data.frame(Err=c(), Class=c(), Lambda=c())

### Loop through lambda values.
for(iLambda in seq(from = from_lambda, to = to_lambda) ) {
    testErr  <- 0.0
    trainErr <- 0.0
	### Set upf the lambda value using 'iLambda'
	expon <- (+2 -4*(iLambda/20))
	xlambda <- 10^expon

    for(iclassval in seq(from = 1, to = nclassvals) ) {
        classVal = glassClassValues[ iclassval ]
        #errFilt = which( Err$QualityValues == classVal )
        ############################################
        ### Do training and capture coefficients
        ############################################
        classFilt = which( glassdf$GlassClass == classVal )
        X = glassdf[, 1:(ncols-1)]
        Y = glassdf[, ncols]
        Y[ classFilt] =  1
        Y[-classFilt] = -1
        train_rr_res = calcRRModelAndYHat(X, Y, xlambda)
        # NOTE: The "coefres" is for DEBUGGING.
        coefres[,iclassval] = coef( train_rr_res$mod )
        rownames( coefres ) = names( coef( train_rr_res$mod ) )
        rownames( coefres )[1] = 'Intercept'
        ### Capture the error rates for the current glass type and lambda
        ymodpred = round( coef(train_rr_res$mod)[-1] %*% t( as.matrix(X) ) + coef(train_rr_res$mod)[1] )
        correct = sum(Y == ymodpred) / length(Y)
        Err = rbind(Err, cbind( 1 - correct, classVal, xlambda) )
    }
}
names(Err) = c('Err', 'Class', 'Lambda')

### Determine which is the best value of 
ErrSorted = Err[order(Err$Err, Err$Class),]
ErrC1 = ErrSorted[ ErrSorted$Class == 1, ]
ErrC2 = ErrSorted[ ErrSorted$Class == 2, ]
ErrC3 = ErrSorted[ ErrSorted$Class == 3, ]
### Dump the lowest error values for each class, and the corresponding lambda.
ErrC1[1,]
ErrC2[1,]
ErrC3[1,]
'
> ErrC1[1,]
         Err Class    Lambda
43 0.2576687     1 0.1584893
> ErrC2[1,]
         Err Class   Lambda
23 0.5337423     2 3.981072
> ErrC3[1,]
        Err Class Lambda
3 0.1104294     3    100
'
