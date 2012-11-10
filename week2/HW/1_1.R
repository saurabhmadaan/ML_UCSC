ages<-c(19,23,30,30,45,25,24,20)

##--Q1s----
sd(ages)
#[1] 8.315218

mean(ages)
#[1] 27

median(ages)
#[1] 24.5
##--Q1e----


##--Q2s----
m1<-mean(ages)
vr<-sum((ages-m1)^2)/(length(ages)-1)
stdv<-sqrt(vr)
stdv
#[1] 8.315218

##--Q2e----


##--Q3s----
#add 10 to all values
sd(ages+10)
#[1] 8.315218
# sd remains unchanged
##--Q3e----


##--Q4s----
#multiply all by 100
sd(ages*100)
#[1] 831.5218
# sd becomes 100 times
##--Q4e----

##--Q5s----
#multiply all by 100
sd(ages*100)
#[1] 831.5218
# sd becomes 100 times
##--Q5e----


##--Q6s----
ages.n<-append(ages,70)
mean(ages.n)
#[1] 31.77778

sd(ages.n)
#[1] 16.3078

#mean and std dev have both increased
##--Q6e----






