# Comparison and Control Examples
# 
# Author: PatriciaHoffman
###############################################################################
rm(list=ls())

?rnorm
###################################################
#
##		Logical Expressions
#
###################################################


x <- c(1,1,2,2)
y <- c(2,2,1,1)
z <-  ifelse((x<y),  TRUE,  FALSE) 
z    #TRUE  TRUE FALSE FALSE

y <- c(2,0,3,1)
w <-  ifelse((x<y),  TRUE,  FALSE)
w    #TRUE FALSE  TRUE FALSE

z;w
!w
z&w
z&&w
z|w
z||w
xor(z,w)

##################################################

#		Comparison Examples

##################################################


x <- stats::rnorm(20)
x
x < 1
y <- x < 1
y
x[x > 0]


## INTEGER  

X1 <- 4 - 2

Y1 <- 3 - 1

X1 == Y1



# FLOATING  

X1 <- .4 - .2

Y1 <- .3 - .1

X1 == Y1

sprintf("%1.10f",X1)

sprintf("%1.20f",X1)


sprintf("%1.10f",Y1)

sprintf("%1.20f",Y1)


x1 <- 0.5 - 0.3
x2 <- 0.3 - 0.1
x1;x2
x1 == x2 # FALSE on most machines
identical(x1,x2)
all.equal(x1,x2)
identical(all.equal(x1, x2), TRUE) # TRUE everywhere

# inside an if clause use isTRUE(all.equal(x1,x2))

isTRUE(all.equal(x1,x2))


#z <- c(32:126, 160:255) # range of most 8-bit charsets, Latin-1 in Unicode
#x <- if(l10n_info()$MBCS) {
#			intToUtf8(z, multiple = TRUE)
#		} else rawToChar(as.raw(z), multiple= TRUE)
### by number
#writeLines(strwrap(paste(x, collapse=" "), width = 60))
### by locale collation
#writeLines(strwrap(paste(sort(x), collapse=" "), width = 60))

# individual variables
pi;355/113
all.equal(pi, 355/113)
# not precise enough (default tol) > relative error
all.equal(pi, 355/113, tolerance = .Machine$double.eps ^ 0.5)
all.equal(pi, 355/113, tolerance = 8.4e-08)
all.equal(pi, 355/113, tolerance = 8.5e-08)

isTRUE(all.equal(pi, 355/113, tolerance = 8.4e-08))
isTRUE(all.equal(pi, 355/113, tolerance = 8.5e-08))

# vectors
d45 <- pi*(1/4 + 1:10)
all (tan(d45) == rep(1,10)) # FALSE, since not exactly
all.equal(tan(d45), rep(1,10), tol=0) # to see difference

isTRUE(all.equal(tan(d45),rep(1,10)))

(tan(d45) == rep(1,10))

a <- 7; b <- 3
isTRUE(all.equal(a, (a %% b) + b * ( a %/% b ))) 


# comparisons
aa<-(c(3,2,1,NA,NaN))
aa
dim(aa) <- c(5,1)
aa
temp <- aa>2
temp
tempno <- temp[!is.na(temp)]
tempno
tempyes <- temp[is.na(temp)]
tempyes

naIndex <-which(is.na(aa))
naIndex
valueIndex<- which(!is.na(aa))
valueIndex


new <- (rep(0,nrow(aa)))
new
new[naIndex] <- 1
new
bb <- cbind(aa,new)
bb
bbframe <- as.data.frame(bb)
bbframe

?Quotes
labs <- paste(c("X","Y"), 1:10, sep="")
labs


#
## Example for identical
#
#identical(1, NULL) ## FALSE -- don't try this with ==
#identical(1, 1.) ## TRUE in R (both are stored as doubles)
#identical(1, as.integer(1)) ## FALSE, stored as different types
#x <- 1.0; y <- 0.99999999999
### how to test for object equality allowing for numeric fuzz :
#(E <- all.equal(x,y))
#isTRUE(E) # which is simply defined to just use
#identical(TRUE, E)
### If all.equal thinks the objects are different, it returns a
### character string, and the above expression evaluates to FALSE
### even for unusual R objects :
#identical(.GlobalEnv, environment())
#### ------- Pickyness Flags : -----------------------------
### the infamous example:
#identical(0., -0.) # TRUE, i.e. not differentiated
#identical(0., -0., num.eq = FALSE)
### similar:
#identical(NaN, -NaN) # TRUE
#identical(NaN, -NaN, single.NA=FALSE) # differ on bit-level

##################################################

#		for Loop Example

##################################################

for(i in 1:5) print(1:i)
i
for(n in c(10,100,1000,200000,500000)) {
	y <- stats::rnorm(n)
	cat(n,":",mean(y),var(y),"\n")
}
letters
f = factor(sample(letters[1:5], 10, replace=TRUE))
f
for( i in unique(f) ) print(i)
i
sort(f)

##################################################

#		if Statement Example

##################################################

mylogic <- TRUE
if(mylogic)print(pi)else print(2*pi)
mylogic <- FALSE
if(mylogic)print(pi)else print(2*pi)


w <- vector(mode = "numeric", length = 21)
x[21] <- 0
x
for(i in 1:21){
	if(x[i]<0) w[i]=0
	if(x[i]>0) w[i]=1	
	if( isTRUE(all.equal(x[i],0, tol = 0.1))) w[i] <- 100
}

x; w; i

##################################################

#		which - example for indexing

##################################################

rm(w)
w <- vector(mode = "numeric", length = 21)
w

x
which(x>0)
w[which(x>0)]   = 1
w[-which(x>0)]  = 0
w[which.min(x)] = 100

w
which.min(x)
w[17]

mylist <- c(1,2,3,4,5,6,7,NA)
small<-mylist[which(mylist < 4)]
small
large <-mylist[which(mylist >= 4)]
large
# use which to index create
#use isTRUE(all.equal(x1,x2))to compare

##################################################

#		Which Example - divide sample into test and train sets

##################################################


prostate<-read.csv("ProstateCancerDataESL.csv", sep=",",header=T)

dim(prostate)
names(prostate)
attributes(prostate)

# If the last attribute is FALSE then it is in the test set

I <- seq(from = 1, to = nrow(prostate))
Itest <- which(prostate[I,10] == FALSE)
y.train<-prostate[-Itest,9]
junk<-prostate$lpsa[-Itest]

isTRUE(all.equal(y.train,junk))

