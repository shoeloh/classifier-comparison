# ABOUT  
In this project I compare the performance of various machine learning classifiers. The code takes a dataset URL and properties from the command line and analyzes it using various classifiers.  

Below is the list of the classifiers implemented:  
1. Decision	Tree  
2. Support Vector Machines  
3. Naïve Bayesian  
4. kNN  
5. Logistic Regression  
6. Neural Network  
7. Bagging  
8. Random Forest  
9. Boosting  

# DATASETS  
A list of datasets used is shown below. For each dataset, I create ten different samples each having a ratio of 90:10 for training and testing. Each classifier will be trained on the training part of the samples and tested on the test part.  

1. Credit default dataset that is available at:  
https://gist.github.com/Bart6114/8675941  
Class variable is attribute: default10yr (Position 6 in the columns)  

2. Graduate school admission dataset that can be obtained from:  
http://www.ats.ucla.edu/stat/data/binary.csv  
Class variable is attribute: admit (Position 1 in the columns)  

3. Wisconsin Prognostic Breast Cancer (WPBC) dataset:  
https://archive.ics.uci.edu/ml/machine-­‐learning-­‐databases/breast-­‐cancer-­‐wisconsin/wpbc.names  
The class attribute is: Outcome (Position 2 in the columns). Also note that the data has no header field.  

4. Wisconsin Diagnostic Breast Cancer (WDBC) dataset:  
https://archive.ics.uci.edu/ml/machine-­‐learning-­‐databases/breast-­‐cancer-­‐wisconsin/wdbc.names  
The class attribute is: Diagnosis (Position 2 in the columns). Also note that the data has no header field.  

5. Ionosphere dataset:  
https://archive.ics.uci.edu/ml/machine-­‐learning-­‐databases/ionosphere/ionosphere.names  
The class attribute has Position 35 in the columns. Also note that the data has no header field. 

# COMPILING, INSTALLATION AND RUNNING  
Program file is classfier-comparison.R.  

## Compiling:  

No compiling of R … its an interpreted language.  

## How to run the code and a genric run command statement along with an example test cases below the general statement.  

Assumes the following packages are installed:  
library(e1071)  
library(caret)  
library(nnet)  
library(randomForest)  
library(rpart)  
library(ada)  
library(mlbench)  
library(class)  
library(mboost)  

If not run:  
install.packages("e1071", dependencies=TRUE)  
install.packages("caret", dependencies=TRUE)  
etc.  

Run the code using the script: testScript.sh  

Generic run command: ./testScript.sh  

Example using directory structure as unpacked:  
$ ./testScript.sh  

## RESULTS  

Results are shown in the Report.pdf file  

Results are also sent to the Console.  

## LICENSE  
[MIT License](https://github.com/shoeloh/classifier-comparison/blob/master/LICENSE)      

