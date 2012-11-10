###########################################################################
###########################################################################
### 
###  Homework 6
### 
###########################################################################
###########################################################################

###########################################################################
### For All Problems: run this to ensure all packages have been downloaded
### and loaded
###########################################################################

EnsurePackage<-function(x)
{
  x<-as.character(x)
  if (!require(x,character.only=TRUE))
  {
    install.packages(pkgs=x,repos="http://cran.r-project.org")
    require(x,character.only=TRUE)
  }
}
 
# PrepareTwitter -  Loads packages for working with Twitter
PreparePackages<-function()
{
  EnsurePackage('e1071')
  EnsurePackage('lattice')
  EnsurePackage('Cairo')
  EnsurePackage('rpart')
  EnsurePackage('mlbench')
  EnsurePackage('randomForest')
}

PreparePackages()


###########################################################################
### Problem 1
###########################################################################

winedf <- read.csv( 'winequality-red.csv', header=TRUE, sep=';')
nycol  <- ncol(winedf)

y_fact    <- factor( winedf$quality, ordered=TRUE )
y_numeric <- as.numeric( winedf$quality )
wine_fact    <- winedf
wine_numeric <- winedf

wine_fact$quality      <- y_fact
wine_numeric$quality  <- y_numeric

### As ordered factor
obj <- tune(svm, quality ~ . , data = wine_fact,
		ranges = list(gamma = 2^(-1:1), cost = 2^(2:4)),
		tunecontrol = tune.control(sampling = "cross"),
		kernel='radial'
)
obj$best.parameters
'
  gamma cost
2     1    4
'
model <- svm(quality ~ ., data = wine_fact, gamma = 1.0, cost = 4, kernel='radial')
x <- subset(wine_fact, select = -quality)
y <- wine_fact[,'quality']
pred <- predict(model, x)
err = 1 - sum(pred==y_fact)/length(y_fact)
err = 1 - sum(as.numeric(pred)==as.numeric(y_fact))/length(y_fact)
err
# [1] 0.008130081 ~1%


### As numeric
obj <- tune(svm, quality ~ . , data = wine_numeric,
		ranges = list(gamma = 2^(-1:1), cost = 2^(2:4)),
		tunecontrol = tune.control(sampling = "cross"),
		kernel='radial'
)
obj$best.parameters
'
  gamma cost
2     1    4
'
model <- svm(quality ~ ., data = wine_numeric, gamma = 1.0, cost = 4, kernel='radial')
x <- subset(wine_numeric, select = -quality)
y <- wine_numeric[,'quality']
pred <- predict(model, x)
# err = 1 - sum(pred==y_numeric)/length(y_numeric)
err = 1 - sum(as.numeric(round(pred))==as.numeric(y_numeric))/length(y_numeric)
err
# [1] 0.008130081 ~1%

### Attribute subsets
wine_2attrib_v1 = wine_fact[, c('fixed.acidity',    'alcohol', 'quality')]
wine_2attrib_v2 = wine_fact[, c('volatile.acidity', 'alcohol', 'quality')]
wine_2attrib_v3 = wine_fact[, c('density',          'alcohol', 'quality')]
 

svmModelWine <- function( wdf, col = 'black', rand_seed=2 ) {
    set.seed( rand_seed )
    plot(wdf[,1], wdf[,2], xlab=names(wdf)[1], ylab=names(wdf)[2], pch=19, cex=.4, col=col)
    obj <- tune(svm, quality ~ . , data = wdf,
            ranges = list(gamma = 2^(-1:1), cost = 2^(2:4)),
            tunecontrol = tune.control(sampling = "cross"),
            kernel='radial'
    )
    print( obj$best.parameters )
    gamma = as.numeric( obj$best.parameters[1] )
    cost  = as.numeric( obj$best.parameters[2] )
    model <- svm(quality ~ ., data = wdf, gamma = gamma, cost = cost, kernel='radial')
    x <- subset(wdf, select = -quality)
    y <- wdf[,'quality']
    pred <- predict(model, x)
    # err = 1 - sum(pred==y)/length(y)
    err = 1 - sum(as.numeric(pred)==as.numeric(y))/length(y)
    print( paste("Err: ", err, sep='') )
}

svmModelWine(wine_2attrib_v1, col='red')
'
  gamma cost
9     2   16
[1] "Err: 0.377110694183865"
'
svmModelWine(wine_2attrib_v2, col='blue')
'
  gamma cost
8     1   16
[1] "Err: 0.393370856785491"
'
svmModelWine(wine_2attrib_v3, col='magenta')
'
  gamma cost
9     2   16
[1] "Err: 0.385240775484678"
'


###########################################################################
### Problem 2
###########################################################################
setwd("/Users/jinhuali/Desktop/UCSC/Machinelearning")
getwd()

trainData<-read.csv("sonar_train.csv",header=F)
testData<-read.csv("sonar_test.csv",header=F)
wineData<-rbind(trainData,testData)

install.packages('e1071')
library('e1071')

# 1. radical kernel SVM
SVMmodel<-svm(trainData[,1:60],trainData[,61])
print(SVMmodel)
summary(SVMmodel)

#check model performance with training data
predict(SVMmodel,trainData[,1:60])
predTrain<-fitted(SVMmodel)
table(predTrain,trainData[,61])


predTest<-predict(SVMmodel,testData[,1:60])
table(predTest,testData[,61])

# correct classification
sum(predTest*testData[,61]>0)/length(predTest)


#2. linear kernel
SVMlinearmodel<-svm(trainData[,1:60],trainData[,61],kernel='linear')
print(SVMlinearmodel)
summary(SVMlinearmodel)

predict(SVMlinearmodel,trainData[,1:60])
predTrain1<-fitted(SVMlinearmodel)
table(predTrain1,trainData[,61])

predTest1<-predict(SVMlinearmodel,testData[,1:60])
table(predTest1,testData[,61])
sum(predTest1*testData[,61]>0)/length(predTest1)


###########################################################################
### Problem 3
###########################################################################

#  load randomForest package
install.packages("randomForest")
 
library(randomForest)
glass <- read.csv("glass.txt",header=FALSE)
 
index <- 1:nrow(glass)
testindex <- sample(index, trunc(length(index)/3))
testset <- Glass[testindex, ]
trainset <- Glass[-testindex, ]
 
y<-as.factor(trainset[,10])
x<-trainset[,1:9]
y_test<-as.factor(testset[,10])
x_test<-testset[,1:9]
fit<-randomForest(x,y)
 
plot(fit, log="y", main="Random Forest Fit Error")
varImpPlot(fit, main="Variable importance as measured by a Random Forest")
trainErr <- 1-sum(y==predict(fit,x))/length(y)
#[1] 0
testErr <- 1-sum(y_test==predict(fit,x_test))/length(y_test)
#[1] 0.2394366


###########################################################################
### Problem 4
###
###    Attribute Information:
###    
###    To construct the data, seven geometric parameters of wheat kernels were measured: 
###    1. area A, 
###    2. perimeter P, 
###    3. compactness C = 4*pi*A/P^2, 
###    4. length of kernel, 
###    5. width of kernel, 
###    6. asymmetry coefficient 
###    7. length of kernel groove. 
###    All of these parameters were real-valued continuous.
###    
###    Data Set Information:
###    
###    The examined group comprised kernels belonging to three different
###    varieties of wheat: Kama, Rosa and Canadian, 70 elements each, randomly
###    selected for the experiment. High quality visualization of the internal
###    kernel structure was detected using a soft X-ray technique. It is
###    non-destructive and considerably cheaper than other more sophisticated
###    imaging techniques like scanning microscopy or laser technology. The
###    images were recorded on 13x18 cm X-ray KODAK plates. Studies were
###    conducted using combine harvested wheat grain originating from
###    experimental fields, explored at the Institute of Agrophysics of the
###    Polish Academy of Sciences in Lublin. 
###    
##############################################################################
seedsDataFile = '/Users/steveb/Documents/Classes/MachineLearningUCSCExt/Week6/Homework/DataFiles/Problem4/Seeds/seeds_dataset_fixed.txt'
seedsdf = read.table(seedsDataFile)
names(seedsdf) = c('area', 'perimeter', 'compactness', 'kernel_length',
                    'kernel_width', 'asym_coef', 'kernel_groove_length',
                    'variety')
wheatVariety = c('Kama', 'Rosa', 'Canadian')
seedsdf$variety = factor(seedsdf$variety, labels=wheatVariety)

attach(seedsdf)

set.seed(2)
obj <- tune(svm, variety ~ . , data = seedsdf,
		ranges = list(gamma = 2^(-1:1), cost = 2^(2:4)),
		tunecontrol = tune.control(sampling = "cross")
)
obj$best.parameters
'
  gamma cost
7   0.5   16
'
kernelList = c('radial', 'linear', 'polynomial', 'sigmoid')
for( kn in kernelList ) {
    set.seed(2)
    obj <- tune(svm, variety ~ . , data = seedsdf,
            ranges = list(gamma = 2^(-1:1), cost = 2^(2:4)),
            tunecontrol = tune.control(sampling = "cross"), kernel=kn
    )
    # obj$best.parameters
    gamma = as.numeric(obj$best.parameters['gamma'])
    cost  = as.numeric(obj$best.parameters['cost'])

    model <- svm(variety ~ ., data = seedsdf, gamma = gamma, cost = cost, kernel=kn)
    # print(model)
    # summary(model)
    x <- subset(seedsdf, select = -variety)
    y <- variety
    pred <- predict(model, x)
    err = 1 - sum(pred==y)/length(y)
    print(
      paste('Kernel: ', kn, ' - gamma = ', gamma, ", cost = ", cost,
            ", error = ", sprintf("%0.3f", err), sep=''))
    print(table(pred, y))
}

# Default Kernel
'
          y
pred       Kama Rosa Canadian
  Kama       70    0        0
  Rosa        0   70        0
  Canadian    0    0       70

[1] "Kernel: radial - gamma = 0.5, cost = 16, error = 0.000"
          y
pred       Kama Rosa Canadian
  Kama       70    0        0
  Rosa        0   70        0
  Canadian    0    0       70
[1] "Kernel: linear - gamma = 0.5, cost = 16, error = 0.014"
          y
pred       Kama Rosa Canadian
  Kama       69    0        2
  Rosa        0   70        0
  Canadian    1    0       68
[1] "Kernel: polynomial - gamma = 0.5, cost = 4, error = 0.005"
          y
pred       Kama Rosa Canadian
  Kama       70    1        0
  Rosa        0   69        0
  Canadian    0    0       70
[1] "Kernel: sigmoid - gamma = 0.5, cost = 8, error = 0.252"
          y
pred       Kama Rosa Canadian
  Kama       42    9       16
  Rosa       11   61        0
  Canadian   17    0       54
'
### The "radial" got the best result (100% correct) and the
### sigmoid kernel had the worst error at 25.2%.


set.seed(2) # SB
nycol     <- ncol( seedsdf )
index     <- 1:nrow(seedsdf)
testindex <- sample(index, trunc(length(index)/3))
testset   <- seedsdf[testindex, ]
trainset  <- seedsdf[-testindex, ]

errTrain  <- data.frame(Depth=rep(-1, 10), Err=rep(-1, 10))
errTest   <- data.frame(Depth=rep(-1, 10), Err=rep(-1, 10))

for(idepth in 1:10) {
    rpart.model <- rpart(variety ~ ., data = trainset, control=rpart.control(maxdepth=idepth))
    
    rpart.pred <- predict(rpart.model, trainset[, -nycol], type = "class")
    table(pred = rpart.pred, true = trainset[, nycol])
    errTrain[idepth,] = c(idepth, 1-sum(rpart.pred == trainset[,nycol])/length(trainset[,nycol]))
    
    
    rpart.pred <- predict(rpart.model, testset[, -nycol], type = "class")
    table(pred = rpart.pred, true = testset[, nycol])
    errTest[idepth,] = c(idepth, 1-sum(rpart.pred == testset[,nycol])/length(testset[,nycol]))
}
errTrain
'
   Depth        Err
1      1 0.33571429
2      2 0.06428571
3      3 0.06428571
4      4 0.06428571
5      5 0.06428571
6      6 0.06428571
7      7 0.06428571
8      8 0.06428571
9      9 0.06428571
10    10 0.06428571
'
min(errTrain[,2])
# [1] 0.06428571

errTest
'
   Depth       Err
1      1 0.3714286
2      2 0.1142857
3      3 0.1142857
4      4 0.1142857
5      5 0.1142857
6      6 0.1142857
7      7 0.1142857
8      8 0.1142857
9      9 0.1142857
10    10 0.1142857
'
min(errTest[,2])
# [1] 0.1142857


###########################################################################
### Problem 5
###########################################################################

x <- seq(0.1, 5, by = 0.05)		    # the observed feature
y <- log(x) + rnorm(x, sd = 0.2)	# the target for the observed feature

xydf = data.frame( x=x, y=y )

nycol = ncol( xydf )
set.seed( rand_seed )
#plot(xydf[,1], xydf[,2], xlab=names(xydf)[1], ylab=names(xydf)[2], pch=19, cex=.4, col=col)
obj <- tune(svm, y ~ . , data = xydf,
        ranges = list(gamma = 2^(-1:1), cost = 2^(2:4)),
        tunecontrol = tune.control(sampling = "cross"),
        kernel='radial'
)
print( obj$best.parameters )
gamma = as.numeric( obj$best.parameters[1] )
cost  = as.numeric( obj$best.parameters[2] )
model <- svm(y ~ ., data = xydf, gamma = gamma, cost = cost, kernel='radial')
pred <- predict(model, x)


plot(x,y, col='black', pch=19)
points(x, as.numeric(pred), col='red', pch=19, cex=0.5)
legend(0, 2, c('Data', 'Prediction'), col=c('black', 'red'), pch=19)

new_xydf <- data.frame( x=xydf$x, x2=(xydf$x)^2, y=xydf$y )
nycol    <- ncol(new_xydf)
#nycol = ncol(new_xydf)
obj <- tune(svm, y ~ . , data = new_xydf,
        ranges = list(gamma = 2^(-1:1), cost = 2^(2:4)),
        tunecontrol = tune.control(sampling = "cross"),
        kernel='radial'
)
print( obj$best.parameters )
gamma = as.numeric( obj$best.parameters[1] )
cost  = as.numeric( obj$best.parameters[2] )
'
  gamma cost
9     2   16
'
model <- svm(y ~ ., data = new_xydf, gamma = gamma, cost = cost, kernel='radial')
pred <- predict(model, new_xydf[,-nycol])

plot(x,y, col='black', pch=19)
points(x, as.numeric(pred), col='red', pch=19, cex=0.5)
legend(0, 2, c('Data', 'Prediction'), col=c('black', 'red'), pch=19)

### Ridge Regression

require(MASS)
rm(list=ls())
library(class)
library(e1071)
setwd("/Users/sabesansp/Research/Machine_Learning/UCSC/class_1/working_dir")
x <- seq(0.1, 5, by = 0.05)
n <- length(x)
z <- x * x
print(z)
y <- log(x) + rnorm(x, sd = 0.2)
dataIn <- as.data.frame(cbind(x,z,y))
X <- dataIn[,-3]
I <- 1:n
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
        Yin <- y[-Iout]
        Yout <- y[Iout]
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
