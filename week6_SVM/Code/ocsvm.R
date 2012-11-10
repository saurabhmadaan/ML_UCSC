# ocsvm.R: 
#    Demostration of One-Class SVM
# 
# Author: Stephen O'Connell
#         sao@saoconnell.com
###############################################################################

rm(list=ls())

require(e1071)
require(lattice)
require(Cairo)

set.seed(123)
N <- 1000
N_test <- 100


##--------------------------------------------------------------
## CREATE TRAINING DATA
x1 <- rnorm(N, mean=7, sd=.8)
x2 <- rnorm(N, mean=5, sd=.6)
train <- data.frame(x1, x2)


## TRAIN PLOT
plot(x1[1:1000], type='l', ylim=c(0,10), ylab="", xlab="")
lines(x2[1:1000], col='red')
title(main="Sample Training Metrics", ylab="Range of Data", xlab="Some Time Series")
legend("topleft", c("Train metric1","Train metric2"), lty=c(1,1), 
		col=c("black","red"),
		title="Metrics", inset=.05 )



#TRAIN DENSITY
plot(density(x1), xlim=c(0,10), ylim=c(0,1),
		main="Density of Training Metrics")
lines(density(x2), col='red')
legend("topleft", c("Train metric1","Train metric2"), lty=c(1,1), 
		col=c("black","red"),
		title="Metrics", inset=.05 )



## PLOT TRAINING DATA AS A SCATTER CHART
plot(x1,x2, col="#E41A1CA1", pch=18, ylim=c(0,10), xlim=c(0,10), ylab="", xlab="", cex=1.2)
title(main="Training Data as a Scatter Chart", ylab="Metric1", xlab="Metric2")
legend("topleft", c("Metric1/2"), pch=18, cex=1.2, 
		col=c("#E41A1CA1"),
		title="Metrics", inset=.05 )



##--------------------------------------------------------------
## CREATE TEST DATA
T1 <- c(rnorm(N_test/2, mean=7, sd=.8), 
		rnorm(N_test/2, mean=2, sd=.8))
T2 <- c(rnorm(N_test/2, mean=5, sd=.6), 
		rnorm(N_test/2, mean=3, sd=.6))
test <- data.frame(T1,T2)

testGroundTruth <- c(rep(TRUE, N_test/2), rep(FALSE, N_test/2))

## TEST PLOT
plot(T1, type='l', ylim=c(0,10), ylab="", xlab="")
lines(T2, col='red')
title(main="Test Data Metrics", ylab="Range of Data", xlab="Some Time Series")
legend("topleft", c("Test metric1","Test metric2"), lty=c(1,1), 
		col=c("black","red"),
		title="Metrics", inset=.05 )



## TEST DENSITY
## TRAIN DENSITY
plot(density(T1), xlim=c(0,10), ylim=c(0,1), lty=2,
		main="Density of Training and Test Metrics")
lines(density(T2), col='red', lty=2)
lines(density(x1))
lines(density(x2), col='red')
legend("topleft", c("Test metric1","Test metric2","Train metric1","Train metric2"), lty=c(2,2,1,1), 
		col=c("black","red"),
		title="Metrics", inset=.05 )




## PLOT TEST DATA AS A SCATTER CHART
plot(T1,T2, col="black", pch=18, ylim=c(0,10), xlim=c(0,10), ylab="", xlab="", cex=1.6)
points(T1[testGroundTruth],T2[testGroundTruth], col='yellow', pch=18, cex=1.6)
points(T1[testGroundTruth], T2[testGroundTruth], col='black', pch=5, cex=1.6)

title(main="Test Data as a Scatter Chart", ylab="Metric1", xlab="Metric2")
legend("topleft", c("Test FALSE", "Test TRUE"), pch=c(18,18), cex=1.2, 
		col=c("black","yellow"),
		title="Test Data", inset=.05 )




##--------------------------------------------------------------
## TRAIN THE MODEL
ocsvm <- svm(train, type='one-classification')

svm_cmd <- "ocsvm <- svm(train, type=\'one-classification\')"
train_indx <- predict(ocsvm, train)
t <- as.character(table(train_indx, rep(TRUE, N)))


## PLOT TRAINING DATA SELECTED IN OCSVM 
plot(x1,x2, col="#E41A1CA1", pch=18, ylim=c(0,10), xlim=c(0,10), ylab="", xlab="", cex=1.2)
points(x1[train_indx], x2[train_indx], col='#4DAF4AAF', pch=18)
title(main=paste("Training Data Included in SVM\nNo Tuning\n", svm_cmd), ylab="Metric1", xlab="Metric2")
legend("topleft", c(paste(t[1], "FALSE POSITIVE"), paste(t[2],"TRUE")), pch=c(18,18), cex=1.2, 
		col=c("#E41A1CA1", "#4DAF4AAF"),
		title="Training Data", inset=.05 )


##--------------------------------------------------------------
## TUNING THE MODEL
results <- list()
for (nu in seq(from=.05, to=.09, by=.002)) {
	for (gamma in seq(from=.12, to=.19, by=.005)) {
		key <- paste("nu_", as.character(nu), "_gamma_", as.character(gamma), sep='')
		ocsvm <- svm(train, type='one-classification', nu=nu, gamma=gamma)
		train_indx <- predict(ocsvm, train)
		t <- table(train_indx, rep(TRUE, N))
		results[key] <- t[1] / t[2]
	}
}

r <- do.call("rbind",results)
dimnames(r)[[1]][which.min(r)]
min(r)

plot(r)


##--------------------------------------------------------------
## TRAIN THE MODEL
ocsvm <- svm(train, type='one-classification', nu=.05, gamma=.14)
svm_cmd <- "ocsvm <- svm(train, type=\'one-classification\', nu=.05, gamma=.14)"

train_indx <- predict(ocsvm, train)
t <- as.character(table(train_indx, rep(TRUE, N)))

## PLOT TRAINING DATA SELECTED IN OCSVM 
plot(x1,x2, col="#E41A1CA1", pch=18, ylim=c(0,10), xlim=c(0,10), ylab="", xlab="", cex=1.2)
points(x1[train_indx], x2[train_indx], col='#4DAF4AAF', pch=18)
title(main=paste("Training Data Included in SVM\nWith Tuning\n", svm_cmd), ylab="Metric1", xlab="Metric2")
legend("topleft", c(paste(t[1], "FALSE POSITIVE"), paste(t[2],"TRUE")), pch=c(18,18), cex=1.2, 
		col=c("#E41A1CA1", "#4DAF4AAF"),
		title="Training Data", inset=.05 )




##----------------------------------------------------------
##  TESTING THE MODEL
test_indx <- predict(ocsvm, test)

table(train_indx, rep(TRUE, N))
table(test_indx, testGroundTruth)
tt <- as.character(table(test_indx, testGroundTruth))



## PLOT TRAINING VS TEST DATA 
plot(x1,x2, col="#E41A1C31", pch=18, ylim=c(0,10), xlim=c(0,10), ylab="", xlab="", cex=1.2)
points(x1[train_indx], x2[train_indx], col='#4DAF4A3F', pch=18)
points(T1, T2, col='black', pch=18, cex=1.6)
points(T1[testGroundTruth], T2[testGroundTruth], col='green', pch=18, cex=1.6)
points(T1[test_indx], T2[test_indx], col='yellow', pch=18, cex=1.6)
points(T1[test_indx], T2[test_indx], col='black', pch=5, cex=1.6)
title(main=paste("Test Data\nTuned OCSVM"), ylab="Metric1", xlab="Metric2")
legend("topleft", c(paste(tt[1], "Test FALSE"), paste(tt[4], "Test TRUE"), paste(tt[3], "FALSE POSITIVE")), pch=c(18,18,18), cex=1.6, 
		col=c('black', 'yellow', 'green'),
		title="Test Data", inset=.05 )


