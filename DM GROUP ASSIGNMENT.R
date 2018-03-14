## Author: Group 10 Assignment
## 
## Let us first set the working directory path

##setwd ("C:/GLIM/DM/Residency 6 - CART/datafile")
##getwd()

## Let us import the data that we intend to use for modeling

library(readr)
options(repos="https://cran.rstudio.com" )

HRAD <- read_csv("C:\\Users\\krpande\\Documents\\Rajesh\\GLIM\\DM\\Residency 6 - CART  RF\\datafile\\HR_Employee_Attrition_Data.csv")
HR_Emp_Attr_Data <- data.frame(HRAD)
View(HR_Emp_Attr_Data)
str(HR_Emp_Attr_Data)
head(HR_Emp_Attr_Data)

## Changing Attrition values to 0 and 1
HR_Emp_Attr_Data <- within(HR_Emp_Attr_Data,Attrition[Attrition == 'Yes'] <- 1)
HR_Emp_Attr_Data <- within(HR_Emp_Attr_Data,Attrition[Attrition == 'No'] <- 0)
HR_Emp_Attr_Data$Attrition <- as.integer(HR_Emp_Attr_Data$Attrition)
head(HR_Emp_Attr_Data)

## Converting categorical variables as factors 
HR_Emp_Attr_Data$Attrition <- as.factor(HR_Emp_Attr_Data$Attrition)
HR_Emp_Attr_Data$BusinessTravel <- as.factor(HR_Emp_Attr_Data$BusinessTravel)
HR_Emp_Attr_Data$Department <- as.factor(HR_Emp_Attr_Data$Department)
HR_Emp_Attr_Data$Education = as.factor(HR_Emp_Attr_Data$Education)
HR_Emp_Attr_Data$EducationField = as.factor(HR_Emp_Attr_Data$EducationField)
HR_Emp_Attr_Data$EnvironmentSatisfaction = as.factor(HR_Emp_Attr_Data$EnvironmentSatisfaction)
HR_Emp_Attr_Data$Gender = as.factor(HR_Emp_Attr_Data$Gender)
HR_Emp_Attr_Data$JobInvolvement = as.factor(HR_Emp_Attr_Data$JobInvolvement)
HR_Emp_Attr_Data$JobLevel = as.factor(HR_Emp_Attr_Data$JobLevel)
HR_Emp_Attr_Data$JobRole = as.factor(HR_Emp_Attr_Data$JobRole)
HR_Emp_Attr_Data$JobSatisfaction = as.factor(HR_Emp_Attr_Data$JobSatisfaction)
HR_Emp_Attr_Data$MaritalStatus = as.factor(HR_Emp_Attr_Data$MaritalStatus)
HR_Emp_Attr_Data$Over18 = as.factor(HR_Emp_Attr_Data$Over18)
HR_Emp_Attr_Data$OverTime = as.factor(HR_Emp_Attr_Data$OverTime)
HR_Emp_Attr_Data$PerformanceRating = as.factor(HR_Emp_Attr_Data$PerformanceRating)
HR_Emp_Attr_Data$RelationshipSatisfaction = as.factor(HR_Emp_Attr_Data$RelationshipSatisfaction)
HR_Emp_Attr_Data$StockOptionLevel = as.factor(HR_Emp_Attr_Data$StockOptionLevel)
HR_Emp_Attr_Data$WorkLifeBalance = as.factor(HR_Emp_Attr_Data$WorkLifeBalance)

## Missing Values

table(is.na(HR_Emp_Attr_Data))

##There are no missing values in the data set.

##Visualisation of Independent Variables

##For numerical variables, we will use histograms; whereas for Categorical Values, we will use Bar Charts or Frequency Counts.

##Distribution of Categorical Variables

par(mfrow=c(4,2))
par(mar = rep(2, 4))
barplot(table(HR_Emp_Attr_Data$Attrition), main = 'Attrition Distribution')
barplot(table(HR_Emp_Attr_Data$BusinessTravel), main = 'Business Travel')
barplot(table(HR_Emp_Attr_Data$Department), main = 'Department')
barplot(table(HR_Emp_Attr_Data$Education), main = 'Education')
barplot(table(HR_Emp_Attr_Data$EducationField), main = 'EducationField')
barplot(table(HR_Emp_Attr_Data$EnvironmentSatisfaction), main = 'Environment Satisfaction')
barplot(table(HR_Emp_Attr_Data$Gender), main = 'Gender')
barplot(table(HR_Emp_Attr_Data$JobInvolvement), main = 'Job Involvement  ')
barplot(table(HR_Emp_Attr_Data$JobLevel), main = 'Job Level')
barplot(table(HR_Emp_Attr_Data$JobRole), main = 'Job Role')
barplot(table(HR_Emp_Attr_Data$JobSatisfaction), main ='Job Satisfaction')
barplot(table(HR_Emp_Attr_Data$Over18), main ='Over 18')
barplot(table(HR_Emp_Attr_Data$OverTime), main = 'Over Time')
barplot(table(HR_Emp_Attr_Data$PerformanceRating), main ='Performance Rating')
barplot(table(HR_Emp_Attr_Data$RelationshipSatisfaction), main ='Relationship Satisfaction')
barplot(table(HR_Emp_Attr_Data$StockOptionLevel), main = 'Stock Option Level')
barplot(table(HR_Emp_Attr_Data$WorkLifeBalance), main = 'Work Life Balance')

##Distribution of Continuous Variables
library(psych)
par(mfrow=c(4,4))

cont_var_data <- HR_Emp_Attr_Data[,c(1,4,6,13,19:21,24,29:30,32:35)]
str(cont_var_data)
multi.hist(cont_var_data)

##Building Predictive Models

#Baseline Accuracy
table(HR_Emp_Attr_Data$Attrition)

#474 out of total 2940 observations left the job. 
#So the baseline accuracy is 2466/2940 = 84%, without building any model. 
#But this naive way would classify all employee churned as non-churn, which defeats the purpose of creating a response plan to reduce employee attrition.

##Dividing the dataset

#Before doing any modeling, let’s divide our data set into training and testing data set to evaluate the performance of our model.
#Dividing the data set into train and test = 30:70 ratio
set.seed(200)
indexes = sample(1:nrow(HR_Emp_Attr_Data), size=900)
train = HR_Emp_Attr_Data[indexes,]
test = HR_Emp_Attr_Data[-indexes,]

#################CART Model####################
#Build a classification tree for this model. Using the same training set, fit a CART model, and plot the tree.
#CART Model
library(rpart)
library(rpart.plot)

Tree = rpart(Attrition ~ ., data = train,  method = 'class', model = FALSE)
prp(Tree)

#The Variable which the Tree Splits upon in the first level is ‘OverTime’, followed by ‘Joblevel’ and 'Age', indicating these are the most important variables.
#Accuracy of the model on testing data set

PredictCART = predict(Tree, newdata = test, type = 'class')
table(test$Attrition, PredictCART)

(1622+ 106)/nrow(test) #Accuracy ~ 85%

106/(106+234)       #Sensitivity ~ 31%

#############AUC of the model################
library(ROCR)
predictTestCART = predict(Tree, newdata = test)
predictTestCART = predictTestCART[,2]

#Compute the AUC:
ROCRCART = prediction(predictTestCART, test$Attrition)
as.numeric(performance(ROCRCART, 'auc')@y.values)

#################Random Forest Model ###############################
##Build a Random Forest Model.
library(randomForest)

set.seed(100)
rfTrain <- randomForest(as.factor(Attrition) ~ ., data = train, 
                        ntree=50, mtry = 20, nodesize = 10,
                        importance=TRUE)

print(rfTrain)

plot(rfTrain, main="")
legend("topright", c("OOB", "0", "1"), text.col=1:6, lty=1:3, col=1:3)
title(main="Error Rates Random Forest Train")

rfTrain$err.rate

################## For test data
rfTest <- randomForest(as.factor(Attrition) ~ ., data = test, 
                       ntree=50, mtry = 20, nodesize = 10,
                       importance=TRUE)

print(rfTest)
plot(rfTest, main="")
legend("topright", c("OOB", "0", "1"), text.col=1:6, lty=1:3, col=1:3)
title(main="Error Rates Random Forest Test")

rfTest$err.rate

#################### Make predictions
predictRF = predict(rfTrain, newdata=test)
table(test$Attrition, predictRF)

(1671+131)/nrow(test)        #Accuracy ~ 88%

131/ (131+209)                #Sensitivity ~ 38%

#The accuracy is better than the CART Model.

#Let us see some Important Variables in Random Forest Model
#One way of understanding this is to look at the number of times, 
#aggregated over all of the trees in the random forest model,
#that a certain variable is selected for a split.
#This can be done using the following code:

#Method 1
vu = varUsed(rfTrain, count=TRUE)
vusorted = sort(vu, decreasing = FALSE, index.return = TRUE)
dotchart(vusorted$x, names(rfTrain$forest$xlevels[vusorted$ix]))

#We can see that Age variables is used maximum times followed by 
#YearsAtCompany and MonthlyIncome compared to other variables.

#Method 2
#A different metric we can look at is related to “impurity”, 
#which measures how homogenous each bucket or leaf of the tree is. 
#Compute below metric
varImpPlot(rfTrain)

#####################Interpretation##########################
#We see that even though CART model beats the Baseline method,
#it underperforms the Random Forest.However, as is the case here, the CART model is 
#often much simpler to describe and understand.