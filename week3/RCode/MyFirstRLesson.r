# My First R Lesson
## 
# 
# Author: PatriciaHoffman
###############################################################################

#Access Help

#  Here are some useful commands

#  To get help on the read function > ??read or > help.search("read")
#      this returns a list of relevant topics
#  To see examples  > example(is.factor) gives examples of is.factor

#  help(help) gives information on the help command
#  To get help > ?is.factor or > help(is.factor)
#  To get help on an arithmetic operator >       ?'%%'
#  To get help on a logical operator >           help("!")
#  List of operator presidence 
#      http://127.0.0.1:27514/library/base/html/Syntax.html
#     help.search() is another function to try
#  Others to try are Apropos(), help.search(),help.start(),
#  To get online help, check out: help(RSiteSearch) 
#      or   RSiteSeaarch()
#http://journal.r-project.org/   is another web site to look at

# Web page for Index into packages by topic
#       http://cran.r-project.org/web/views/

# Web page for whole document
#  http://lib.stat.cmu.edu/R/CRAN/doc/manuals/fullrefman.pdf

#Documentation for sparce matrix operations 
#   ?SparseM::Ops.matrix.csr

# source("commands.R") To execute an external file      
# sink("record.lis")   To divert results to an external file
# to see the data sets loaded with R
# library(help="datasets")

# to see what is in the utility package
# library(help="utils")

#to check out various demos
#demo() # for attached packages
#
### All available demos:
#demo(package = .packages(all.available = TRUE))
#
### Display a demo, pausing between pages
#demo(lm.glm, package="stats", ask=TRUE) 

#tutorial web page
#http://math.illinoisstate.edu/dhkim/rstuff/rtutor.html

#more interesting web pages for learning r
# http://rwiki.sciviews.org/doku.php
# http://www.statmethods.net/
library(help="graphics")
#This will show you several graphics functions
#press the Enter key to mmove to each new graph
demo(graphics)
# this demo will show you how to create a histogram and a box plot


#to remove all the data from the working directory
#good practice to start with a clean workspace
rm(list=ls())

#####################################


#                 Read Data


#####################################

# 
#   Read data in from a file
#

#####################################

# first set the directory to where the file is located
setwd("C:/Users/PatriciaHoffman/workspaceR/TestDataSets")
# is it at the directory you expected it to be?
getwd()

# to get help reading 
#  help.search("read") # results in lists of read functions
#  ?gdata::read.xls    # results in doc for reading an excel file


help("read.csv")


#   tell R the data is comma seperated values
datax<-read.csv("filter60HzNotchblanks.txt", sep=" ",header=F)
datax[1:5];length(datax);class(datax)
datay<-read.csv("filter50HzNotch.txt", sep=",",header=F)
datay[1:5];length(datay)
data <-c(datax,datay)
class(datax)
#####################################

# 
#           Coercion 
#

#####################################
# Change the dimension of data - Change it into a matrix
class(data)
length(data)
dim(data)
dim(data) <- c(351,2)
dim(data)
class(data)

#   see what is in the 1st through 5th rows
data[1:5,]
#   see what is in the 1st column
data[,1]
datax[1:5]
# set the dim attribute
# coerce a vector into a matrix

morejunk <- seq(1:10)
morejunk
length(morejunk)
class(morejunk)
dim(morejunk)

attr(morejunk, "dim")<- c(2,5)
morejunk
dim(morejunk)
length(morejunk)
class(morejunk)

#Write Data to a File
write.csv(datax,file="dataMyFirstRLesson.txt")


int7 <- as.integer(c(7,3,4,5))
is.numeric(int7)
is.integer(int7)
num7 <-as.numeric(int7)
class(num7)
class(int7)
str(int7)

# Example of an Ordered Factor

survey <- factor(c("Less than 0","more than 15",">10 to 15",">5 to 10",">10 to 15","more than 15","Less than 0",">5 to 10",">10 to 15" ))
results <-factor(survey, levels = c("Less than 0",">5 to 10",">10 to 15","more than 15"),ordered = TRUE)
survey;results
results <- as.numeric(results)
results
#############################################

#                 Investigate Objects

#############################################

matrixletters<-read.csv("matrixletters.csv", sep=",",header=T)
matrixletters
dim(matrixletters)
attributes(matrixletters)

is.factor(matrixletters[1:8,1])
is.numeric(matrixletters[1:8,1])

is.factor(matrixletters[1:8,2])  #[1] TRUE
is.numeric(matrixletters[1:8,2]) #[1] FALSE

is.factor(matrixletters[1:8,3])  #[1] TRUE
is.numeric(matrixletters[1:8,3]) #[1] FALSE

#   tell R that - means there is No data for that entry
matrixnumbers  <-read.csv("matrixletters.csv", sep=",",header=T, na.strings = "-")
#
#is.factor(matrixnumbers[1:8,1])
#is.numeric(matrixnumbers[1:8,1])
#
#is.factor(matrixnumbers[1:8,2])
#is.numeric(matrixnumbers[1:8,2])

is.factor(matrixnumbers[1:8,3])  #[1] FALSE
is.numeric(matrixnumbers[1:8,3]) #[1] TRUE

class(matrixnumbers)
str(matrixnumbers)
levels(matrixnumbers$second.clm)
summary(matrixnumbers)
matrixnumbers[4,2]  


objects()
matrixletters
ls()
rm(matrixletters)
matrixletters
objects()
save.image(file = "MyFirstRLesson.RData")
rm(list=ls()) # this starts fresh ...
# no variables exist now
objects()
load("MyFirstRLesson.RData")
objects()
############################################

#             Operations

#############################################

rm(list=ls()) # this starts fresh ...
              # no variables exist now

aa<-c(1,2,144)
aa
class(aa)

#Manipulating data - simple operations:


aa+10
length(aa)
aa
bb<-c(2,6,12)
my_data_set<-data.frame(attributeA=aa,attributeB=bb)
my_data_set

#Indexing data - note [row,colm]  

my_data_set[,1]
my_data_set$attributeA
my_data_set[,2]
my_data_set$attributeB

my_data_set[3,2]
my_data_set$attributeB[3]
my_data_set[1:2,]
my_data_set[c(1,3),]


#Matrix Arithmetic:  (in Matlab this is aa ./ bb)


aa/bb
#
#Summary Statistics
#

sum(aa)
prod(aa)
mean(aa)
x<-aa
sum((x-mean(x))^2)/(length(x)-1)
var(x)
yy<- c(1,2,3,4,5,6)
xx <- aa + yy
xx
x <- 6*aa
x
mean(my_data_set[,1])
median(my_data_set[,1])
sqrt(var(my_data_set[,1]))

summary(my_data_set)

new_data_set <- cbind(c(1,2,3),c(10,20,30))
rbind(c(1,2,3),c(10,20,30))
c(c(1,2,3),c(10,20,30))
new_data_set
var(new_data_set[,1])
scaleData <- scale(new_data_set)
scaleData
var(scaleData[,1])
#
#Write a file out
#
write.csv(my_data_set,"my_data_set_file.csv")
#
#Get a help file
#
?write.csv

input  <- c(aa,bb)
matrixx1<- matrix(data = input, nrow =2,ncol =3)
matrixx1
matrixxx<- matrix(data = c(aa,bb), nrow =2,ncol =3)
matrixxx
#covariance matrix of aa and bb independent vectors.
matrixx2 = var(matrixxx)
matrixx2
xxx <- c(6, 2, 7, 3, 8,4)
yy
pmax(xxx,yy)
pmin(xxx,yy)

#   complex numbeer in r

sqrt(-1)     # error
sqrt(-1 +0i) #works

#   seq(from= ,to= , by = ,length = )

ccc <- seq(1,1.5,0.1)
ccc
cc1 <- seq(from = 1,by = 0.1, length = 5)
cc1

ccc <- seq(1,1.5,0.1)
ccc

cc1 <- seq(from = 1,by = 0.1, length.out = 5)
cc1

seq( 1,by = 0.1, length = 5)
cc2 <- 1:5
cc2
cc3  <- 1:5 -2
cc3
aa

#   rep 
 rep(aa, times = 3)
 
 rep(aa, each = 3)
 
 ############################################
 
#            Matrix Operations
 
 #############################################
 
 
 # multiplication
 
 Ident <- diag((rep(1,times = 3))) 
 Ident
 Z <- matrix(1:9, ncol = 3, nrow = 3)
 Z
 W <- Z %*% Ident
 W
 W <- Z * Ident
 W
 Ident2 <- 2*Ident
 Ident2
 W <- Z %*% Ident2
 W

 
 W <- Z/2
 W
W <- Z/Z
W
W <- Z%/% Z
W

firstList <- c(1,2,3)
secondList <- c(5,6,7)
newList <-c(firstList, secondList)
newList
############################################

#            Solve Ax = b

#############################################

a <- c(2,1,5,3)
ainverse <- c(3, -1, -5, 2)

A <- matrix(a, ncol = 2, nrow = 2)
Ainverse <- matrix(ainverse, ncol = 2, nrow = 2)

A
Ainverse


A %*% Ainverse

# the matrix Ainverse is the inverse of the matrix A

bfirst <- c(1,0)
bsecond <- c(0,1)

# 


# The first row of Ainverse = Ainverse %*% t(bfirst)
# The second row of Ainverse = Ainverse %*% t(bsecond)


Ainverse
Ainverse %*% bfirst
Ainverse %*% bsecond

?solve
solve(A,bfirst)
solve(A,bsecond)

help.search("transpose")
?base::t

A
t(A)
############################################

#             Simple Linear Regression Example

#############################################


x = seq(1,20,0.5)
y = 1 + 2*x + rnorm(39)

plot(x,y)
model <- lm(y~x)

abline(1.081,1.998)

model
#
#Call:
#		lm(formula = y ~ x)
#
#Coefficients:
#		(Intercept)            x  
#1.081        1.998  


summary(model)


abline(model$coef)

#  classical ordinary least squares
#beta <- solve(t(x) %*% x) %*% t(x) %*% y

######################################

############################################

#             Investigate Taking Samples

#############################################

data <- rnorm(2000, mean = 0, sd = 1)
data
mean(data)
?sample
my_seq <- seq(1,2000)
sam<-sample(seq(1,2000),10,replace=T)
?seq
my_seq
sam
my_sample<-data[sam] 
my_sample
#Sample Repeatedly and see what happens
real_mean<-mean(data)
?rep
store_diff<-rep(0,10000)
for (k in 1:10000){
	sam<-sample(seq(1,1922),10,replace=T)
	my_sample<-data[sam]
	store_diff[k]<-abs(mean(my_sample)-real_mean)
}
mean(store_diff)
#Change Sample size from 10 to 100
#   with the larger sample size the mean is more accurate
real_mean<-mean(data)
store_diff<-rep(0,10000) 
for (k in 1:10000){
	sam<-sample(seq(1,1922),100,replace=T)
	my_sample<-data[sam]
	store_diff[k]<-abs(mean(my_sample)-real_mean)
}
mean(store_diff)

#
############################################

#            Working with Graphs

#############################################

# generate 100 data points from a Normal Distribution
#              with variance = mean = 3
normalData <- rnorm(1000, mean = 3, sd = sqrt(3))
normalData[1:10]
# generate 100 data points from a Possion Distribution 
#              with lambda = variance = mean = 3
possionData <- rpois(1000, 3)
possionData[1:10]
###################################################
###            Box Plot
###################################################
boxplot(normalData,possionData,col=c("blue",'red'),
	main="Comparison of Normal and Poisson Distributions",
	names=c("Normal Distribution","Possion Distribution"),ylab="Values")


###################################################
###             Histogram
###################################################
hist(normalData, freq=TRUE, breaks=seq(from=-7,to=12,by=.5), 
		main="Normal Distribution")
hist(possionData, freq=TRUE, breaks=seq(from=0,to=12,by= 1), 
		main="Poisson Distribution")


###################################################
###         Empirical Cumulative Distribution Function 
###################################################
plot(ecdf(possionData), pch='+', col='red',
		main="Comparison of Normal and Poisson Distributions",
#		xlab="x",
#		ylab="Cumulative Percent of Values less than x"
)
lines(ecdf(normalData), pch='.', col='blue')
legend(2500, .5, c("normal","possion"), cex=1, 
		col=c("blue","red"), pch=c('.','+'), lty=1:2)

###################################################
###         Scatter Plot 
###################################################
normalDatasd20 <- rnorm(1000, mean = 3, sd = sqrt(20))
normalData[1:10]

plot(normalData, normalDatasd20, pch=20, col='blue', 
		main="Comparison of Normal Distributions with different variances",
		xlim=c(-10,20),ylim=c(-10,20),
		xlab="Variance = 3",
		ylab="Variance = 20"
)
############################################

#             Investigate Working with Dates

#############################################
########################################
# working with dates - Number of days since 1970-01-01
# help.start()
# install.packages("chron")
# help(package="chron")
library(chron)

dts <- dates(c("02/27/92", "02/27/92", "01/14/92",
				"02/28/92", "02/01/92"))
dtsmin <- min(dts)

# We can add or subtract scalars (representing days) to dates or
# chron objects:
c(dts[1], dts[1] + 10)
# [1] 02/27/92 03/08/92
dts[1] - 31
# [1] 01/27/92

# We can substract dates which results in a times object that
# represents days between the operands:
dts[1] - dts[3]
# Time in days:
# [1] 44
diffdate <- dts[1] - dts[3]
diffdate
as.numeric(diffdate)


dates <- c("02/27/92", "02/27/92", "01/14/92", "02/28/92", "02/01/92")
numday <- as.Date(dates, "%m/%d/%y")
julian(numday)
#julian(m, d, y, origin)
startDate <- as.Date(c("01/01/00"),"%m/%d/%y")
julian(numday,startDate)

numday

