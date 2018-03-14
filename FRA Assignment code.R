##################################################################################
# Group Assignent for FRA
# Author - PGPBABIH - Group #5
##################################################################################
# Clean up and Garbage collection 
rm(list=ls(all=TRUE))
gc(reset=T)

# Get the required packages for code
library(mlr)
library(readxl)
library(ggplot2)
library(dplyr)
library(psych)
library(DMwR)

#Sset the working directory to the path that contains all the data files:
setwd('C:\\Users\\krpande\\Documents\\Rajesh\\GLIM\\FRM\\off campus assignment')

# Load the  Data
#training.xlsx
training <- data.frame(read_excel("training.xlsx", 
                       sheet = "training", col_types = c("numeric", 
                                                         "numeric", "numeric", "numeric", 
                                                         "numeric", "numeric")))
#test.xlsx
test <- data.frame(read_excel("test.xlsx", 
                                  sheet = "test", col_types = c("numeric", 
                                                                    "numeric", "numeric", "numeric", 
                                                                    "numeric", "numeric")))
# About the data
head(training)
#SeriousDlqin2yrs (Y/N)-	Person experienced 90 days past due delinquency or worse 
#RevolvingUtilizationOfUnsecuredLines (%) -	Total balance on credit cards and personal lines of credit except real estate and no installment debt like car loans divided by the sum of credit limits
#DebtRatio(%) -	Monthly debt payments, alimony,living costs divided by monthy gross income
#NumberOfOpenCreditLinesAndLoans -	Number of Open loans (installment like car loan or mortgage) and Lines of credit (e.g. credit cards)
#NumberOfDependents - 	Number of dependents in family excluding themselves (spouse, children etc.)

# Remove the unwanted variable - sequence number 
training$Casenum <- NULL
test$Casenum <- NULL

###############Data Processing and visualization #################################
# check missing values and update statistically
sapply(training,function(x) sum(is.na(x)))

# There are missing values for variable 'Number of dependents'. Let us replace it with Mode, the most frequect observation.
# Mode function
Mode = function(x){ 
  ta = table(x)
  tam = max(ta)
  if (all(ta == tam))
    mod = NA
  else
    if(is.numeric(x))
      mod = as.numeric(names(ta)[ta == tam])
  else
    mod = names(ta)[ta == tam]
  return(mod)
}

training$NumberOfDependents[is.na(training$NumberOfDependents)] <- Mode(training$NumberOfDependents)
test$NumberOfDependents[is.na(test$NumberOfDependents)] <- Mode(test$NumberOfDependents)

# Statiscal Summary of all the variables and graphical representation.
summarizeColumns(training)
multi.hist(training)

# From these outputs, we can make the following inferences:
#1. In the data, we have 6 variables, out of which SeriousDlqin2yrs is the dependent variable and rest are independent variables.
#2. Training data has 5000 observations. Test data has 1000 observations.
#3. In trainng and test data, no variable has missing values (already fixed).
#4. NumberOfOpenCreditLinesAndLoans and RevolvingUtilizationOfUnsecuredLines has outlier values, which should be treated.

################### Analyse each the variables#####################################
#Training dataset
# Seriousdlqin2yrs
summary(training$SeriousDlqin2yrs)
qplot(data = training, x = SeriousDlqin2yrs)

count(training, vars = SeriousDlqin2yrs)
#Clearly looks like imbalanced data, with '1' in minority class.
# We will try SMOTE techniques to create a balance dataset.

#RevolvingUtilizationOfUnsecuredLines
summary(training$RevolvingUtilizationOfUnsecuredLines)
qplot(data = training, x = RevolvingUtilizationOfUnsecuredLines)
ggplot(training, aes(y=RevolvingUtilizationOfUnsecuredLines, x = 1)) + geom_boxplot()
# Check the outliers
boxplot.stats(training$RevolvingUtilizationOfUnsecuredLines)$out
#Since it is a percentage - anything over 1 should not be considered.
#Looking at the boxplot stats, remove anything over 1.36.
training = filter(training, RevolvingUtilizationOfUnsecuredLines < 1.36)
#new boxplot
ggplot(training, aes(y=RevolvingUtilizationOfUnsecuredLines, x = 1)) + geom_boxplot()

#DebtRatio
summary(training$DebtRatio)
qplot(data = training, x = DebtRatio)
ggplot(training, aes(y=DebtRatio, x = 1)) + geom_boxplot()
hist(training$DebtRatio,  breaks = 100,main = "Debt Ratio Chart",xlab = "DebtRatio")
# Looks good no update required

#NumberOfOpenCreditLinesAndLoans
summary(training$NumberOfOpenCreditLinesAndLoans)
qplot(data = training, x = NumberOfOpenCreditLinesAndLoans)
ggplot(training, aes(y=NumberOfOpenCreditLinesAndLoans, x = 1)) + geom_boxplot() 
# Check the outliers
boxplot.stats(training$NumberOfOpenCreditLinesAndLoans)$out
#Since, more the number of open loans more are chances of default. Let's filter outliers as not required by us.
training = filter(training, NumberOfOpenCreditLinesAndLoans < 21)
#new boxplot
ggplot(training, aes(y=NumberOfOpenCreditLinesAndLoans, x = 1)) + geom_boxplot() 
hist(training$NumberOfOpenCreditLinesAndLoans,  breaks = 100,main = "Open Loans Chart",xlab = "Number of open loans")

#NumberOfDependents
summary(training$NumberOfDependents)
qplot(data = training, x = NumberOfDependents)
ggplot(training, aes(y=NumberOfDependents, x = 1)) + geom_boxplot() 
hist(training$NumberOfDependents,  breaks = 100,main = "Dependents Chart",xlab = "Number of Dependents")
# Looks good no update required

#Test dataset
# Seriousdlqin2yrs
summary(test$SeriousDlqin2yrs)
qplot(data = test, x = SeriousDlqin2yrs)

count(test, vars = SeriousDlqin2yrs)
#Clearly looks like imbalanced data, with '1' in minority class.
# We will use SMOTE on training dataset and apply the model to Test dataset

#RevolvingUtilizationOfUnsecuredLines
summary(test$RevolvingUtilizationOfUnsecuredLines)
qplot(data = test, x = RevolvingUtilizationOfUnsecuredLines)
ggplot(test, aes(y=RevolvingUtilizationOfUnsecuredLines, x = 1)) + geom_boxplot()
# Check the outliers
boxplot.stats(test$RevolvingUtilizationOfUnsecuredLines)$out
#Since it is a percentage - anything over 1 should not be considered.
#Looking at the boxplot stats, remove anything over 1.85
test = filter(test, RevolvingUtilizationOfUnsecuredLines < 1.85)
#new boxplot
ggplot(test, aes(y=RevolvingUtilizationOfUnsecuredLines, x = 1)) + geom_boxplot()

#DebtRatio
summary(test$DebtRatio)
qplot(data = test, x = DebtRatio)
ggplot(test, aes(y=DebtRatio, x = 1)) + geom_boxplot()
hist(test$DebtRatio,  breaks = 100,main = "Debt Ratio Chart",xlab = "DebtRatio")
# Looks good no update required

#NumberOfOpenCreditLinesAndLoans
summary(test$NumberOfOpenCreditLinesAndLoans)
qplot(data = test, x = NumberOfOpenCreditLinesAndLoans)
ggplot(test, aes(y=NumberOfOpenCreditLinesAndLoans, x = 1)) + geom_boxplot() 
# Check the outliers
boxplot.stats(test$NumberOfOpenCreditLinesAndLoans)$out
#Since, more the number of open loans more are chances of default. Let's filter outliers as not required by us.
test = filter(test, NumberOfOpenCreditLinesAndLoans < 21)
#new boxplot
ggplot(test, aes(y=NumberOfOpenCreditLinesAndLoans, x = 1)) + geom_boxplot() 
hist(test$NumberOfOpenCreditLinesAndLoans,  breaks = 100,main = "Open Loans Chart",xlab = "Number of open loans")

#NumberOfDependents
summary(test$NumberOfDependents)
qplot(data = test, x = NumberOfDependents)
ggplot(test, aes(y=NumberOfDependents, x = 1)) + geom_boxplot() 
hist(test$NumberOfDependents,  breaks = 100,main = "Dependents Chart",xlab = "Number of Dependents")
# Looks good no update required

######################## SMOTE ###################################################
# Before calling the smote, we need to ensure that predictor variable is a factor.
training$SeriousDlqin2yrs = as.factor(training$SeriousDlqin2yrs)
TrainSMOTE <- SMOTE(SeriousDlqin2yrs ~., training, perc.over = 400,k = 5, perc.under = 300)
summarizeColumns(TrainSMOTE)
table(TrainSMOTE$SeriousDlqin2yrs)
# The result dataset look more blanced now.

##################### Build Models ###############################################
# Logistic Regression
logit_loan=glm(SeriousDlqin2yrs ~ ., data=TrainSMOTE, family=binomial) 

## Check the model parameters 
summary(logit_loan) 

# Calculate the predicted probability and accuracy.Make predictions using the predict function
# type = "response" returns the probabilities for each data
predictTestProbability = predict(logit_loan, newdata= test, type = "response")
predictTestOutcome <- predictTestProbability > 0.5
head(predictTestOutcome)

# Build confusion matrix. Compare the actual decisons with predictions
confusion_matrix <- table(test$SeriousDlqin2yrs, predictTestOutcome)
confusion_matrix
#Accuracy
Loggit_Accuracy = (750+31) /(750+31+157+30)
Loggit_Accuracy 
# Specivity
Loggit_specivity = (31/(30+31))
Loggit_specivity

# Mean squared Error
Loggit_MSE <- sum((predictTestProbability - test$SeriousDlqin2yrs)^2)/nrow(test)
Loggit_MSE
# Visualize the Logistic Regression Model performance 
# Let us plot the ROC curve and calculate the AUC for performance measurements
# Note : The Roc is curve generated by plotting the true positive rate(TPR) against the false 
# positive rate (FPR) at various threshold settings while the AUC is the area under the ROC curve.
# A model with good predictive ability should have AUC close to 1 (ideal)
library(ROCR)
perf <- prediction(predictTestProbability, test$SeriousDlqin2yrs)
PerfMeas <- performance(perf, measure = "tpr", x.measure = "fpr")
plot(PerfMeas)
# Area under the curve
AUC <- performance(perf, measure = "auc") 
AUC <- AUC@y.values[[1]]
print(paste('Area Under the Curve of Model is : ', AUC))

##########################Other Models#############################################
#CART Model
library(rpart)
library(rpart.plot)

Tree = rpart(SeriousDlqin2yrs ~ ., data = TrainSMOTE,  method = 'class', model = FALSE)
prp(Tree)

#The Variable which the Tree Splits upon in the first level is ‘RevolvingUtilizationOfUnsecuredLines’, followed by ‘DebtRatio’ and 'NumberOfDependents', indicating these are the most important variables.
#Model predicion on testing data set
PredictCART = predict(Tree, newdata = test, type = 'class')
table(test$SeriousDlqin2yrs, PredictCART)
(762+ 30)/nrow(test) #Accuracy ~ 82%
30/(31+30)       #Sensitivity ~ 49%

#Compute the AUC of CART model
predictTestCART = predict(Tree, newdata = test)
predictTestCART = predictTestCART[,2]
ROCRCART = prediction(predictTestCART, test$SeriousDlqin2yrs)
as.numeric(performance(ROCRCART, 'auc')@y.values)

#Random Forest Model
library(randomForest)

rfTrain <- randomForest(SeriousDlqin2yrs ~ ., data = TrainSMOTE, 
                        ntree=20,  nodesize = 5,
                        importance=TRUE)
print(rfTrain)
plot(rfTrain, main="")
legend("topright", c("OOB", "0", "1"), text.col=1:6, lty=1:3, col=1:3)
title(main="Error Rates Random Forest Train")

rfTrain$err.rate

#Make predictions
predictRF = predict(rfTrain, newdata=test)
table(test$SeriousDlqin2yrs, predictRF)

#Accuray an Specivity
(821+17)/nrow(test)        #Accuracy ~ 87%
17/ (17+44)                #Sensitivity ~ 28%

# Between CARt and Random forest, the accuracy is better in Random forest than the CART Model.
#Understanding Important Variables in Random Forest Model
#One way of understanding this is to look at the number of times, 
#aggregated over all of the trees in the random forest model,
#that a certain variable is selected for a split.
#This can be done using the following code:

#Method 1
vu = varUsed(rfTrain, count=TRUE)
vusorted = sort(vu, decreasing = FALSE, index.return = TRUE)
dotchart(vusorted$x, names(rfTrain$forest$xlevels[vusorted$ix]))
#We can see that DebtRatio variables is used maximum times followed by other variables.

#Method 2
#A different metric we can look at is related to “impurity”, 
#which measures how homogenous each bucket or leaf of the tree is. 
#Compute below metric
varImpPlot(rfTrain)

#ANN Artificial Neuarl Net#############################
library(neuralnet)
TrainSMOTE$SeriousDlqin2yrs <- as.numeric(TrainSMOTE$SeriousDlqin2yrs)
nn1 <- neuralnet(formula = SeriousDlqin2yrs ~  RevolvingUtilizationOfUnsecuredLines +  DebtRatio  + NumberOfOpenCreditLinesAndLoans +  NumberOfDependents, 
                 data = TrainSMOTE, 
                 hidden = 3,
                 err.fct = "sse",
                 linear.output = FALSE,
                 lifesign = "full",
                 lifesign.step = 10,
                 threshold = 0.1,
                 stepmax = 2000
)

# Graphical representation of the model with the weights on each connection
plot (nn1)

# Predict the ANN model
Predict_nn <- neuralnet::compute(nn1,test[,2:5])

# Mean Squared Error
NeurlNet_MSE <- sum((test$SeriousDlqin2yrs - Predict_nn$net.result)^2)/nrow(test)
NeurlNet_MSE

#Compare the MSEs of Logistic and neural net
print(paste(NeurlNet_MSE,Loggit_MSE))
# Clearly Logistic is better than Neural Net in terms of MSE value.






############ to be worked upon



misClassTable = data.frame(Target = test$SeriousDlqin2yrs, Prediction = Predict_nn$net.result[[1]] )
misClassTable$Classification = ifelse(misClassTable$Prediction>0.143,1,0)
with(misClassTable, table(Target, Classification))

## We can use the confusionMatrix function of the caret package 
##install.packages("caret")
library(caret)
confusionMatrix(misClassTable$Target, misClassTable$Classification)
