# Compare the Performance of 9 Different Classifiers	
# 
# Author: Shane Zabel
###############################################################################

###############################################################################
# 1. Install and Load Necessary Packages
###############################################################################

#Uncomment line below if e1071 package is not installed
#install.packages("e1071", dependencies=TRUE)
#Uncomment line below if caret package is not installed
#install.packages("caret", dependencies=TRUE)
#Uncomment line below if nnet is not installed
#install.packages("nnet", dependencies=TRUE)
#Uncomment line below if randomForest is not installed
#install.packages("randomForest", dependencies=TRUE)
#Uncomment line below if rpart is not installed
#install.packages("rpart", dependencies=TRUE)
#Uncomment line below if ada is not installed
#install.packages("ada", dependencies=TRUE)
#Uncomment line below if mlbench is not installed
#install.packages("mlbench", dependencies=TRUE)
#Uncomment line below if class is not installed
#install.packages("class", dependencies=TRUE)
#Uncomment line below if mboost is not installed
#install.packages("mboost", dependencies=TRUE)

.libPaths("/<path to>/R/x86_64-pc-linux-gnu-library/3.6")

#Load and attach the e1071 packagenn
library(e1071)
#Load and attach the caret package
library(caret)
#Load and attach the nnet package
library(nnet)
#Load and attach the randomForest package
library(randomForest)
#Load and attach the rpart package
library(rpart)
#Load and attach the ada package
library(ada)
#Load and attach the mlbench package
library(mlbench)
#Load and attach the class package
library(class)
#Load and attach the mboost package
library(mboost)

###############################################################################
# 1. Template
###############################################################################
args <- commandArgs(TRUE)
dataURL<-as.character(args[1])
header<-as.logical(args[2])
#Read in CSV data replacing "?" with "NA"
d<-read.csv(dataURL,header = header,na.strings = "?")
#Omit all rows containing "NA"
d<-na.omit(d)
#Set seed
set.seed(123)
#Initialize container for answers
ans1 <- ans2 <-ans3 <- ans4 <- ans5 <- ans6 <- ans7 <- ans8 <- ans9 <- NULL # Creates empty storage container

# create 10 samples
for(i in 1:10) {
	cat("Running sample ",i,"\n")
	
	#Create a 90/10 split for training/test data
	sampleInstances<-sample(1:nrow(d),size = 0.9*nrow(d))
	trainingData<-d[sampleInstances,]
	testData<-d[-sampleInstances,]
	
	#Setup a formula with the class attribute
	measurevar <- colnames(trainingData)[as.integer(args[3])]
	groupvars  <- c(".")
	Class <- as.formula(paste(measurevar, groupvars, sep=" ~ "))
	
	#Initialize a variable with the training/test data class attributes
	testClass<-testData[,as.integer(args[3])]
	trainingClass<-trainingData[,as.integer(args[3])]
	
	#Convert factors to {0,1} if not alread so
	if(trainingClass[1] != 0 && trainingClass[1] != 1 ){
		trainingClass<-as.numeric(trainingClass)-1
		trainingData[,as.integer(args[3])]<-as.numeric(trainingData[,as.integer(args[3])])-1
	}
	if(testClass[1] != 0 && testClass[1] != 1 ){
		testClass<-as.numeric(testClass)-1
		testData[,as.integer(args[3])]<-as.numeric(testData[,as.integer(args[3])])-1
	}

#1. Decision	Tree	
#Create a model using the rpart function
	dtModel <- rpart(Class, data = trainingData, method = 'class', parms=list(split='information'),minsplit=2, minbucket=1,na.action = na.omit)
#Create a pruned tree
	dtModel1 <- prune(dtModel,dtModel$cptable[which.min(dtModel$cptable[,"xerror"]),"CP"],na.action = na.omit)
	dtPred <- predict(dtModel1,newdata=testData,type="vector",na.action = na.omit)
#Calculate the accuracy
	result<-testClass-(dtPred-1)
	counter<-0
	for(i in 1:dim(testData)[1]){
		counter<-counter+abs(result[i])
	}
	dtAcc<-(1-counter/dim(testData)[1])
	
	cat("DT Accuracy = ",dtAcc,"\n")
	
	
#2. Support	Vector Machines	
#Use the training data to build a Support Vector Machine model using the "e1071" package: svm function
#Repeat for linear, polynomial, radial basis and sigmoid kernels
	svmModel1 <- svm(formula=Class, data=trainingData, kernel="linear", na.action = na.omit)
	svmModel2 <- svm(formula=Class, data=trainingData, kernel="polynomial", na.action = na.omit)
	svmModel3 <- svm(formula=Class, data=trainingData, kernel="radial", na.action = na.omit)
	svmModel4 <- svm(formula=Class, data=trainingData, kernel="sigmoid", na.action = na.omit)

#Use the models to find the predictions on the test data
	svmPred1 <- predict(svmModel1, testData, na.action = na.omit)
	svmPred2 <- predict(svmModel2, testData, na.action = na.omit)
	svmPred3 <- predict(svmModel3, testData, na.action = na.omit)
	svmPred4 <- predict(svmModel4, testData, na.action = na.omit)

#Convert factors to {0,1} if not alread so
	if(svmPred1[1] != 0 || svmPred1[1] != 1 ){
		svmPred1<-as.numeric(svmPred1)-1
		svmPred2<-as.numeric(svmPred2)-1
		svmPred3<-as.numeric(svmPred3)-1
		svmPred4<-as.numeric(svmPred4)-1
	}

#Use a threshold value and anything above that, you can assign to class=1 others to class=0
	threshold=0.5
	svmPrediction1<-sapply(svmPred1, FUN=function(x) if (x>threshold) 1 else 0)
	svmPrediction2<-sapply(svmPred2, FUN=function(x) if (x>threshold) 1 else 0)
	svmPrediction3<-sapply(svmPred3, FUN=function(x) if (x>threshold) 1 else 0)
	svmPrediction4<-sapply(svmPred4, FUN=function(x) if (x>threshold) 1 else 0)

#Calculate the accuracies of the models using the "caret" package: confusionMatrix function
	svmAcc1 <- confusionMatrix( factor(svmPrediction1), factor(testClass) )
	svmAcc2 <- confusionMatrix( factor(svmPrediction2), factor(testClass) )
	svmAcc3 <- confusionMatrix( factor(svmPrediction3), factor(testClass) )
	svmAcc4 <- confusionMatrix( factor(svmPrediction4), factor(testClass) )
	
#3. Naïve Bayesian	
#Use the training data to build a naïve Bayesian model using the "e1071" package: naiveBayes function
	nbModel <- naiveBayes(formula=Class, data=trainingData,na.action = na.omit)
#Use the model to find the prediction on the test data
	nbPred <- predict(nbModel, testData,type = "raw", threshold = 0.001,na.action = na.omit)
# use a threshold value and anything above that, you can assign to class=0 others to class=1
	nbThreshold=0.5
	nbPrediction<-sapply(nbPred[,1], FUN=function(x) if (x>nbThreshold) 0 else 1)
#Calculate the accuracy of the model using the "caret" package: confusionMatrix function
	nbAcc <- confusionMatrix( factor(nbPrediction), factor(testClass) )
	
	cat("NB Accuracy = ",nbAcc$overall[1],"\n")
	
#4. kNN	
#Use the training data to build a K-Nearest Neighbor model using the "class" package: knn function
#Repeat for k-values of 3,5,7,9, and 11
	knnModel1 <- knn(trainingData,testData,as.factor(trainingClass),k=3)
	knnModel2 <- knn(trainingData,testData,as.factor(trainingClass),k=5)
	knnModel3 <- knn(trainingData,testData,as.factor(trainingClass),k=7)
	knnModel4 <- knn(trainingData,testData,as.factor(trainingClass),k=9)
	knnModel5 <- knn(trainingData,testData,as.factor(trainingClass),k=11)
#Calculate the accuracies of the models using the "caret" package: confusionMatrix function
	knnAcc1 <- confusionMatrix( factor(knnModel1), factor(testClass) )
	knnAcc2 <- confusionMatrix( factor(knnModel1), factor(testClass) )
	knnAcc3 <- confusionMatrix( factor(knnModel3), factor(testClass) )
	knnAcc4 <- confusionMatrix( factor(knnModel4), factor(testClass) )
	knnAcc5 <- confusionMatrix( factor(knnModel5), factor(testClass) )
	
	cat("knn Accuracy = ",knnAcc3$overall[1],"\n")
	
#5. Logistic Regression	
#Use the training data to build a Logistic Regression model using the glm function
	glmModel <- glm(Class,data=trainingData,family=binomial())
#Use the models to find the predictions on the test data
	glmPred <- predict(glmModel, newdata=testData, type="response")
# use a threshold value and anything above that, you can assign to class=1 others to class=0
	glmThreshold=0.5
	glmPrediction<-sapply(glmPred, FUN=function(x) if (x>glmThreshold) 1 else 0)
#Calculate the accuracies of the models using the "caret" package: confusionMatrix function
	glmAcc <- confusionMatrix( factor(glmPrediction), factor(testClass) )
	
	cat("LR Accuracy = ",glmAcc$overall[1],"\n")
	
#6. Neural Network	
#Use the training data to build a Neural Network model using the nnet function
	nnModel <- nnet(formula=Class, data=trainingData, size = 20, rang = 0.1, decay = 5e-4, maxit = 200)
#Use the models to find the predictions on the test data
	nnPred <- predict(nnModel, testData)
# use a threshold value and anything above that, you can assign to class=1 others to class=0
	nnThreshold=0.5
	nnPrediction<-sapply(nnPred, FUN=function(x) if (x>nnThreshold) 1 else 0)
#Calculate the accuracies of the models using the "caret" package: confusionMatrix function
	nnAcc <- confusionMatrix( factor(nnPrediction), factor(testClass) )
	
	cat("NN Accuracy = ",nnAcc$overall[1],"\n")
	
#7. Bagging	
#Use the training data to build a Bagging model using the bagging function
	#bagModel <- glmboost(Class, data = trainingData,center=TRUE)
	bagModel <- glmboost(Class, data = trainingData,center=TRUE)
#Use the models to find the predictions on the test data
	bagPrediction<-predict(bagModel,testData)
# use a threshold value and anything above that, you can assign to class=1 others to class=0
	bagThreshold=0.5
	bagPrediction<-sapply(bagPrediction, FUN=function(x) if (x>bagThreshold) 1 else 0)
#Calculate the accuracies of the models using the "caret" package: confusionMatrix function
	bagAcc <- confusionMatrix( factor(bagPrediction), factor(testClass) )
	
	cat("Bag Accuracy = ",bagAcc$overall[1],"\n")
	
#8. Random Forest	
#Use the training data to build a Random Forest model using the randomForest function
	rfModel <- randomForest(Class, data=trainingData, ntree=500, importance=TRUE, proximity=TRUE)
#Use the models to find the predictions on the test data
	rfPred <- predict(rfModel, testData)
# use a threshold value and anything above that, you can assign to class=1 others to class=0
	rfThreshold=0.5
	rfPrediction<-sapply(rfPred, FUN=function(x) if (x>rfThreshold) 1 else 0)
#Calculate the accuracies of the models using the "caret" package: confusionMatrix function
	rfAcc <- confusionMatrix( factor(rfPrediction), factor(testClass) )
	
	cat("RF Accuracy = ",rfAcc$overall[1],"\n")
	
#9. Boosting	
#Use the training data to build a boosting model using the ada function
	boostModel <- ada(Class, data = trainingData, iter=50, nu=1, type="discrete")
#Use the models to find the predictions on the test data
	boostPrediction<-predict(boostModel,testData)
#Calculate the accuracies of the models using the "caret" package: confusionMatrix function
	boostAcc <- confusionMatrix( factor(boostPrediction), factor(testClass) )
	
	cat("Boost Accuracy = ",boostAcc$overall[1],"\n")
	
	#Report the Output in a Table
	repDisplayAns<-c("Table Containing Accuracies for Experiment")
	print(repDisplayAns)
	displayAns <- matrix(c("dt","svm","nb","knn","lr","nn","bag","rf","boost",dtAcc,
					svmAcc1$overall[1],nbAcc$overall[1],knnAcc3$overall[1],glmAcc$overall[1],
					nnAcc$overall[1],bagAcc$overall[1],rfAcc$overall[1],boostAcc$overall[1]),
			ncol=2,byrow=FALSE)
	colnames(displayAns) <- c("Method","Accuracy")
	rownames(displayAns) <- NULL
	print(displayAns)
	
	#Store accuracies
	ans1 <- c(ans1,dtAcc)
	ans2 <- c(ans2,svmAcc1$overall[1])
	ans3 <- c(ans3,nbAcc$overall[1])
	ans4 <- c(ans4,knnAcc3$overall[1])
	ans5 <- c(ans5,glmAcc$overall[1])
	ans6 <- c(ans6,nnAcc$overall[1])
	ans7 <- c(ans7,bagAcc$overall[1])
	ans8 <- c(ans8,rfAcc$overall[1])
	ans9 <- c(ans9,boostAcc$overall[1])
}

#Report the Output in a Table
repDisplayAns<-c("Table Containing Average Accuracies for 10 Experiment2")
print(repDisplayAns)
displayAns <- matrix(c("dt","svm","nb","knn","lr","nn","bag","rf","boost",sum(ans1)/10,
				sum(ans2)/10,sum(ans3)/10,sum(ans4)/10,sum(ans5)/10,sum(ans6)/10,
				sum(ans7)/10,sum(ans8)/10,sum(ans9)/10),
		ncol=2,byrow=FALSE)
colnames(displayAns) <- c("Method","Avg Accuracy")
rownames(displayAns) <- NULL
print(displayAns)
