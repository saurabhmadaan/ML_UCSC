setwd("/Users/smadaan/Documents/ML_UCSC/week1/HW")
data<-read.csv("HW01pb1data.csv",header=FALSE)
class(data)
str(data)

is.factor(data[,4])
as.numeric(data[,5])

l4<-levels(data[,4])
#l4
'
> l4
 [1] "0"           "10"          "100"         "110"         "120"        
 [6] "140"         "15"          "150"         "160"         "20"         
[11] "200"         "25"          "30"          "35"          "40"         
[16] "5"           "50"          "55"          "60"          "65"         
[21] "70"          "80"          "85"          "90"          "thirty five"
'


which(data[,4]=="thirty five")
#[1] 405

l5<-levels(data[,5])
#l5
'
> l5
 [1] "0"           "10"          "120"         "140"         "15"         
 [6] "20"          "25"          "255"         "30"          "35"         
[11] "40"          "45"          "5"           "50"          "55"         
[16] "60"          "70"          "80"          "twenty five"
'
# [1] 531

plot(data[,1])
plot(data[,4])



