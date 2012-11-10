# Homework02 R-Code

hwdir <- "/Users/steveb/Documents/Classes/MachineLearningUCSCExt/Week2/Homework"
setwd(hwdir)
getwd()
# [1] "/Users/steveb/Documents/Classes/MachineLearningUCSCExt/Week2/Homework"

#########################################################
##########################  1  ##########################
#########################################################

### 1)
ages <- c(19,23,30,30,45,25,24,20)
### 1) a.
sd(ages)
[1] 8.315218
mean(ages)
[1] 27
median(ages)

### 1) b.
# Using variance.
sqrt( var(ages) )
# Using the formula for sample standard deviation:
sqrt( sum( (ages-mean(ages))^2 ) / (length(ages)-1))
### 1) c.
sd(ages+10)
### 1) d.
sd(ages * 100)
### 1) e.
newTree <- 70
mean(   c(ages, newTree) )
median( c(ages, newTree) )

#########################################################
##########################  2  ##########################
#########################################################
# Compute the following:
#   - Information Gain using Entropy as the
#     purity measure for every split
#   - Where is the best place to split a3?
#   - Compute the classification error (CE)
#   - Using CE, where would be the best place to
#     split?
# 
# NOTES:
#   - I used the equaxtions from the begining of the homework
#   - I referenced the lectured notes "DataMiningTreesTricia-1.pdf
#     This helped with the "weighted" impurity measures; see slide
#     with "Calculate Purity Gain for Split A"
#   - I looked at Ch4 in the book (~ pages 160 to 163)

#==========================================================
# Calculates entropy: - sum( p(i|node) log2( p(i|node) )
# Equation from the homework assignment.
#==========================================================
entropy <- function( numList) {
  N = sum( numList )
  if( N == 0 ) {
    entr = 0
  } else {
    pin = numList / N
    filt = pin != 0
    pin = pin[filt]
    entr = - sum( pin * log(pin, base=2) )
  }
  entr
}
#==========================================================
# Calculates Classification Error: 1 - max( p(i | node) )
# Equation from the homework assignment.
#==========================================================
classificationError <- function(numList) {
  N = sum( numList )
  if( N == 0 ) {
    val = 0
  } else {
    pin = numList / N
    val = 1 - max( pin )
  }
  val
}
#==========================================================
# This computes a weighted average for the classification
# error of 2 nodes.
# Equation from the lecture notes.
#==========================================================
classificationErrorWeighted <- function(n1, n2) {
  total = sum(n1, n2)
  n1sum = sum(n1)
  n2sum = sum(n2)
  (n1sum/total) * classificationError(n1) + (n2sum/total) * classificationError(n2)
}
#======================================================================
# Computes purity gain: I(parent) - SUM_j=1-k( (N(vj) / N) * I(vj) )
# Equation from the homework assignment and lecture notes.
#======================================================================
purityGain <- function(parentNode, n1, n2) {
    # Impurity of Parent
    impres_par_node = entropy( parentNode )
    # Impurity of Child
    impres_n1 = entropy( n1 )
    impres_n2 = entropy( n2 )
    # 
    Nparsum = sum( parentNode )
    n1sum   = sum( n1 )
    n2sum   = sum( n2 )
    # Calculate Purity gain
    pg = impres_par_node - (n1sum/Nparsum) * impres_n1 - (n2sum/Nparsum) * impres_n2
    pg
}

#======================================================================
# Main code for answering the questions.
# Compute the following:
#   * Information Gain using Entropy as the
#     purity measure for every split
#   * Where is the best place to split a3?
#   * Compute the classification error (CE)
#   * Using CE, where would be the best place to
#     split?
#======================================================================

#== Set up the data.
a3        <- c(1.0, 6.0, 5.0, 4.0, 7.0, 3.0, 8.0, 7.0, 5.0)
tc        <- c(1,1,0,1,0,0,0,1,0)
targClass <- c("+", "+", "-", "+", "-", "-", "-", "+", "-")
binClass = data.frame( a3=a3, TargetClass=targClass)
#== Sort the rows by column a3: sorting is being done to make it
#== easier to see how the continuous variable (small to large)
#== outcome is distributed given the sorted input.
binClass = binClass[order(binClass$a3),]
#= Make a unique list of places to split.
splitValues = unique( sort( binClass$a3 ) )

# Parent node: arbitrarily chosen.
parentNode = c(4,5) # 4:+ and 5:-
pg_resdf = data.frame(SplitVal=splitValues, PurityGain=NA)
ce_resdf = data.frame(SplitVal=splitValues, CError=NA) # Classification error.
for( sv in splitValues ) {
  #print(sv)
  # Get items "<=" to sv
  tmpdf = binClass[binClass$a3 <= sv,]
  n1 = c( sum( tmpdf$TargetClass=='+' ), sum( tmpdf$TargetClass=='-' ) )
  # Get items ">" to sv
  tmpdf = binClass[binClass$a3 >  sv,]
  n2 = c( sum( tmpdf$TargetClass=='+' ), sum( tmpdf$TargetClass=='-' ) )
  #================================================
  # Calculate Purity Gain using Entropy
  # and the weighted Classification Error.
  #================================================
  pg = purityGain(parentNode, n1, n2) # , 'entropy')
  ce = classificationErrorWeighted(n1, n2)
  pg_resdf[pg_resdf$SplitVal==sv, 'PurityGain']   = pg
  ce_resdf[pg_resdf$SplitVal==sv, 'CError']    = ce
  #tmpstr = sprintf("%s: %f", sv, pg)
  #print(tmpstr)
}
binClass
# Contains the Purity Gain Values: higher is better
pg_resdf[ order(pg_resdf$PurityGain), ]
# Classification Error Values: lower is better.
ce_resdf[ order(ce_resdf$CError), ]
# Purity Gain: I(parent) - sum( (Nvj / N) I(vj)


#########################################################
##########################  3  ##########################
#########################################################

# NO CODING NEEDED.


#########################################################
##########################  4  ##########################
#########################################################

setwd("/Users/smadaan/Documents/ML_UCSC/week2/Data")

##--Q1s----
A<-c(TRUE,TRUE,TRUE,TRUE,TRUE,FALSE,FALSE,FALSE,TRUE,TRUE)
B<-c(FALSE,TRUE,TRUE,FALSE,TRUE,FALSE,FALSE,FALSE,TRUE,FALSE)
y<-c(1,1,1,0,1,0,0,0,0,0)

df<-as.data.frame(as.matrix(cbind(A,B,y)))

for(i in seq(1:3))
{
	df[,i]<-as.factor(df[,i])
}


#--- use only A
results.A<-as.data.frame(matrix(nrow=5,ncol=2))
colnames(results.A)<-c('tree_depth','error_w_A')
results.A$tree_depth<-seq(1:5)
for(i in seq(1:5))
{
fit<-rpart(y~A,data=df,control=rpart.control(minsplit=0,minbucket=0,cp=-1,
				maxcompete=0, maxsurrogate=0, usesurrogate=0, 
				xval=0,maxdepth=i))

yout<-predict(fit,df[,1:2],type="class")
which(yout==df$y)
# [1] 1 2 3 5 6 7 8

ll<-length(which(yout==df$y))
#[1] 7
results.A[i,2]=1-ll/length(df[,1])
}

results.A
#   tree_depth error_w_A
# 1          1     0.3
# 2          2     0.3
# 3          3     0.3
# 4          4     0.3
# 5          5     0.3


#--- use only B
results.B<-as.data.frame(matrix(nrow=5,ncol=2))
colnames(results.B)<-c('tree_depth','error_w_B')
results.B$tree_depth<-seq(1:5)
for(i in seq(1:5))
{
fit<-rpart(y~B,data=df,control=rpart.control(minsplit=0,minbucket=0,cp=-1,
				maxcompete=0, maxsurrogate=0, usesurrogate=0, 
				xval=0,maxdepth=i))

yout<-predict(fit,df[,1:2],type="class")
which(yout==df$y)
# [1] 2  3  4  5  6  7  8 10

ll<-length(which(yout==df$y))
#[1] 8
results.B[i,2]=1-ll/length(df[,1])
}

results.B
# > results.B
#   tree_depth error_w_B
# 1          1     0.2
# 2          2     0.2
# 3          3     0.2
# 4          4     0.2
# 5          5     0.2



#--- use both variables
results.AB<-as.data.frame(matrix(nrow=5,ncol=2))
colnames(results.AB)<-c('tree_depth','error_w_AB')
results.AB$tree_depth<-seq(1:5)
for(i in seq(1:5))
{
fit<-rpart(y~A+B,data=df,control=rpart.control(minsplit=0,minbucket=0,cp=-1,
				maxcompete=0, maxsurrogate=0, usesurrogate=0, 
				xval=0,maxdepth=i))

yout<-predict(fit,df[,1:2],type="class")
which(yout==df$y)
# [1] 2  3  4  5  6  7  8 10

ll<-length(which(yout==df$y))
#[1] 8
results.AB[i,2]=1-ll/length(df[,1])
}

results.AB

# '> results.AB
#   tree_depth error_w_AB
# 1          1             0.2
# 2          2             0.2
# 3          3             0.2
# 4          4             0.2
# 5          5             0.2
# '

result<-merge(merge(results.A,results.B),results.AB)

 result
#   tree_depth error_w_A error_w_B error_w_AB
# 1          1       0.3       0.2        0.2
# 2          2       0.3       0.2        0.2
# 3          3       0.3       0.2        0.2
# 4          4       0.3       0.2        0.2
# 5          5       0.3       0.2        0.2
# '


#########################################################
##########################  5  ##########################
#########################################################
### Adapted from Patricia Hoffman's code.

##  #setwd("C:/Users/PatriciaHoffman/workspaceR/TestDataSets")
##  #setwd("/Users/steveb/Documents/Classes/MachineLearningUCSCExt/Week2/DataFiles/SonarData")
##  

#===============================
# load data
#===============================
sonardatadf <- read.csv("sonar.all-data.txt", header=FALSE)
# Label the last column to indicate Rock or Mine
names(sonardatadf)[61] = 'RockOrMine'
# Indexes for data column values.
dataColIdxs = 1:60
# Sample train
train_size <- nrow( sonardatadf )

#===============================
# Make training and test set
# using sample.
#===============================
# Set random sample seed to get reprodcible runs from run to run.
set.seed(2) # 2 is arbitrary.
train_idxs <- sample( 1:train_size, train_size / 2, replace = FALSE, prob = NULL)
trainIn    <- sonardatadf[  train_idxs, ]
testIn     <- sonardatadf[ -train_idxs, ]

# Set up "control" for "rpart" using values specified in question.
rpart_control = rpart.control(
    minsplit=0,     minbucket=0,    cp=-1,
    maxcompete=0,   maxsurrogate=0,
    usesurrogate=0, xval=0,         maxdepth=5)
# Perform the fit using the training data
#   y_train: R or M
#   x_train: numeric values
y_train = as.factor( trainIn[,'RockOrMine'] )
x_train = trainIn[ , dataColIdxs]
fit  <- rpart(   y_train ~ . , x_train, control = rpart_control )
#===============================
# Determine what perceentage are
# inccorect (i.e. the error
# in the training set).
#===============================
predict_trainIn = predict( fit, x_train , type="class" )
errVal_train = 1 - sum( y_train == predict_trainIn) / length(y_train)

#===============================
# Determine what perceentage are
# inccorect (i.e. the error
# in the test set).
#===============================
# Now look at test set.
y_test = as.factor( testIn[,'RockOrMine'] )
x_test = testIn[ , dataColIdxs]
predict_testIn = predict( fit, x_test , type="class" )
# Determine what perceentage are inccorecte (i.e. the error
errVal_test = 1 - sum( y_test == predict_testIn) / length(y_test)

errVal_train
errVal_test





############################ 6 ##########################

###############################################################################
# Source code modified from Patricia's Code file : SonarRpartXval-1.R
###############################################################################

setwd("/Users/sabesansp/Research/Machine_Learning/class_1/working_dir")
rm(list=ls())
#install.packages("rpart")
require("rpart")
train<-read.csv("winequality-red.csv",sep=";",header=TRUE)
### 10-fold validation 
nxval <- 10 
ndepth <- 10
trainOutput <- matrix(0.0,nrow = ndepth, ncol = 2) 
#### trainOutput = [(0.0,0.0),...(0.0,0.0)]
testOutput <- matrix(0.0,nrow =ndepth, ncol = 2)
#### testOutput = [(0.0,0.0),...(0.0,0.0)]
I <- seq(from = 1, to = nrow(train))
#### I = [1 2 3 ... 1599] 
for(idepth in 1:ndepth){
    trainErr <- 0.0
    testErr <- 0.0
    for(ixval in seq(from =  1, to = nxval)){
        Iout <- which(I%%nxval == ixval%%nxval)
        ###### Iout = [1 11 21 31 ... ] in the first iteration
        trainIn <- train[-Iout,]
        #### trainIn = All rows minus the rows represented by Iout
        trainOut <- train[Iout,]
        #### trainOut = All rows minus the rows represented by Iout, namely 1, 11, 21, 31, etc...
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
testOutput
trainOutput
cex = .5
pch = 21
plot(trainOutput, ylim=c(0,maxval),
        main="Model Complexity",
        xlab="Model Complexity = Tree Depth",
        ylab="Training & Test Error",
        col='red',
        cex = cex,
        pch = pch
)
points(testOutput, col='blue', cex = cex, pch = pch)
legend(7, 0.2, c('Training Data', 'Testing Data'), cex=.8, col=c('red', 'blue'), pch=c(pch, pch)) #, lty=1:2)

# Plat the last fit.
plot(fit, main="Last fit Plot (not necessarily the best)")
text(fit)

index <- which.min(testOutput[,2])
testOutput[index,2]
index


