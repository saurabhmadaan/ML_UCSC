setwd('/Users/smadaan/Documents/ML_UCSC/week2/data')

sonar_train<-read.table('sonar_train.csv',sep=',')
sonar_test<-read.table('sonar_test.csv',sep=',')

str(sonar_train)
str(sonar_test)

sonar_all<-rbind(sonar_train,sonar_test)

I<-seq(1:nrow(sonar_all))
trainErr=0
testErr=0

for(n in seq(1:5)){
	ind <- which(I%%5 == n-1)
	sonarTrain <- sonar_all[-ind,]
	sonarTest <- sonar_all[ind,]
	
	mod <- lm(sonarTrain[,61]~., sonarTrain[,1:60])
	yhatTrain <- predict(mod,sonarTrain[,1:60])
	correct <- sum(yhatTrain*sonarTrain[,61]>0)
	trainErr <- trainErr + (1 - correct/(nrow(sonarTrain)))/5
	
	yhatTest <- predict(mod,sonarTest[,1:60])	
	correct <- sum(yhatTest* sonarTest[,61]>0)
	testErr <- testErr + (1 - correct/(nrow(sonarTest)))/5
}


print("trainErr = ")
trainErr                   #[1] 0.08174013
print("testErr = ")
testErr   	# [1] 0.2550523

