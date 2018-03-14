## Title : Data Mining Assignment 2
## Author: GLIM Group 1
#################################################################################################################################
## Let us import the HR Attrition dataset that we intend to use for NN ##

library(readr)
options(repos="https://cran.rstudio.com" )
Data <- read_csv("C:/GLIM/DM/GROUP ASSIGNMENT/HR_Employee_Attrition_Data.csv")
#View(HR_Employee_Attrition_Data)
HR_EAD <- data.frame(Data)
#View(HR_EAD)
str(HR_EAD)
#################################################################################################################################
## Missing Values Validation ##
#First we need to check that no datapoint is missing, 
#otherwise we need to fix the dataset.

table(is.na(HR_EAD))

#No missing data found, so we are all good.
#################################################################################################################################
## Data Preprocessing to fit the Neural Network.##
#Changing Attrition values to 0 and 1
HR_EAD <- within(HR_EAD,Attrition[Attrition == 'Yes'] <- 1)
HR_EAD <- within(HR_EAD,Attrition[Attrition == 'No'] <- 0)
HR_EAD$Attrition <- as.integer(HR_EAD$Attrition)
# Converting other categorical column values to numerical 
library(car)
HR_EAD$BusinessTravel<- recode(HR_EAD$BusinessTravel,"'Travel_Frequently'= 3;'Travel_Rarely'= 2;else=1")
HR_EAD$Department <- recode(HR_EAD$Department,"'Sales'= 1;'Research & Development'= 2;else=3")
HR_EAD$EducationField <- recode(HR_EAD$EducationField,"'Human Resources'= 1;'Life Sciences'= 2;'Marketing' =3;'Medical'=4;'Technical Degree' =5;else=6")
HR_EAD$Gender <- recode(HR_EAD$Gender,"'Male' =1;'Female' =2")
HR_EAD$JobRole <- recode(HR_EAD$JobRole,"'Healthcare Representative'= 1;'Human Resources'= 2;'Laboratory Technician' =3;'Manager'=4;'Manufacturing Director' =5;'Research Director'=6;'Research Scientist' = 7;'Sales Executive'=8;else=9")
HR_EAD$MaritalStatus <- recode(HR_EAD$MaritalStatus,"'Single' =1;'Married' =2;else =0")
HR_EAD$Over18 <- recode(HR_EAD$Over18,"'Y' =1;else =0")
HR_EAD$OverTime <- recode(HR_EAD$OverTime,"'Yes' =1;else =0")
head(HR_EAD)
#################################################################################################################################
##Dividing the dataset

#Before doing any modeling, let’s divide our data set into training and testing data set 
#We proceed by randomly splitting the data into a train and a test set - 30:70 ratio
set.seed(500)
indexes = sample(1:nrow(HR_EAD),round(0.30*nrow(HR_EAD)))
train = HR_EAD[indexes,]
test = HR_EAD[-indexes,]
#################################################################################################################################
## Installing the Neural Net package; 
## If already installed do not run the below step
##install.packages("neuralnet")
library(neuralnet)
#################################################################################################################################

##Build the neural network. The fields used in expression are explained.
#formula: a symbolic description of the model to be fitted.
#data   : a data frame containing the variables specified in the formula.
#hidden : a vector of integers specifying the number of hidden neurons (vertices) in each layer.
#err.fct:	a differentiable function that is used for the calculation of the error. Alternatively, 
#         the strings 'sse' and 'ce' which stand for the sum of squared errors and the cross-entropy can be used.
#linear.output:	logical. If act.fct should not be applied to the output neurons set linear output to TRUE, otherwise to FALSE.
#threshold: is a numeric value specifying the threshold for the partial derivatives of the 
#           error function as stopping criteria.
#stepmax	: the maximum steps for the training of the neural network. Reaching this maximum leads 
#           to a stop of the neural network's training process.
# 
# There is no fixed rule as to how many layers and neurons to use although there are several more 
# or less accepted rules of thumb. Usually, if at all necessary, one hidden layer is enough for a 
# vast numbers of applications. As far as the number of neurons is concerned, it should be between 
# the input layer size and the output layer size, usually 2/3 of the input size. 
# We are going to use 2 hidden layers with this configuration: 34:10:6:1. The input layer has 34 inputs, 
# the two hidden layers have 10 and 6 neurons and the output layer has, of course, a single output 
# since we are doing regression.
#################################################################################################################################
nn1 <- neuralnet( formula = Attrition ~ Age + BusinessTravel + DailyRate + Department +	DistanceFromHome +	Education +	EducationField+ EmployeeCount +	EmployeeNumber + EnvironmentSatisfaction +
                              Gender	+ HourlyRate +	JobInvolvement + JobLevel + JobRole +	JobSatisfaction	+ MaritalStatus	+ MonthlyIncome	+ MonthlyRate	+ NumCompaniesWorked +	Over18	+
                              OverTime + PercentSalaryHike + PerformanceRating + RelationshipSatisfaction +	StandardHours +	StockOptionLevel + TotalWorkingYears + TrainingTimesLastYear +
                              WorkLifeBalance	+ YearsAtCompany + YearsInCurrentRole	+ YearsSinceLastPromotion + YearsWithCurrManager, 
                       data = train, 
                       hidden=c(10,6), 
                       err.fct = 'sse',
                       linear.output = FALSE,
                       threshold=0.01,
                       stepmax = 500
                       )
#Key Results to consider
nn1$result.matrix[1:3,]

#Plot the neural network
plot(nn1)

#Quantile results
quantile(nn1$net.result[[1]], c(0,1,5,10,25,50,75,90,95,99,100)/100)
#The gradation in predicted probabilities is not much.
# Let us scale the variables and see if it has impact on output
#################################################################################################################################

## Build the neural net model by scaling the variables
x <- train
y <- scale(x)
y <- cbind(train[2], y)

# Dropping the variables EmployeeCount,Over18 andStandardHours as they can't be scaled.
nn.trainscaled <- cbind(y[1:9],y[11:22],y[24:27],y[29:36])

nn2 <- neuralnet( formula = Attrition ~ Age + BusinessTravel + DailyRate + Department +	DistanceFromHome +	Education +	EducationField+ EmployeeNumber + EnvironmentSatisfaction +
                    Gender	+ HourlyRate +	JobInvolvement + JobLevel + JobRole +	JobSatisfaction	+ MaritalStatus	+ MonthlyIncome	+ MonthlyRate	+ NumCompaniesWorked +	
                    OverTime + PercentSalaryHike + PerformanceRating + RelationshipSatisfaction +	StockOptionLevel + TotalWorkingYears + TrainingTimesLastYear +
                    WorkLifeBalance	+ YearsAtCompany + YearsInCurrentRole	+ YearsSinceLastPromotion + YearsWithCurrManager, 
                  data = nn.trainscaled, 
                  hidden=c(10,6), 
                  err.fct = 'sse',
                  linear.output = FALSE,
                  threshold=0.01,
                  stepmax = 500)

#Key Results to consider
nn2$result.matrix[1:3,]

#Plot the neural network
plot(nn2)

#Quantile results
quantile(nn2$net.result[[1]], c(0,1,5,10,25,50,75,90,95,99,100)/100)

## Classification Accuracy and Confusion Matrix 
misClassTable = data.frame(Attrition = nn.trainscaled$Attrition, Predict.Score = nn2$net.result[[1]] )
misClassTable$Predict.class = ifelse(misClassTable$Predict.Score>0.9,1,0)
with(misClassTable, table(Attrition, Predict.class))

library(caret)
confusionMatrix(misClassTable$Attrition, misClassTable$Predict.class)

#################################################################################################################################
## SSE Error Computation
sum((misClassTable$Attrition - misClassTable$Predict.Score)^2)/2
################################################################################################################################### 

#deciling code
decile <- function(x){
  deciles <- vector(length=10)
  for (i in seq(0.1,1,.1)){
    deciles[i*10] <- quantile(x, i, na.rm=T)
  }
  return (
    ifelse(x<deciles[1], 1,
           ifelse(x<deciles[2], 2,
                  ifelse(x<deciles[3], 3,
                         ifelse(x<deciles[4], 4,
                                ifelse(x<deciles[5], 5,
                                       ifelse(x<deciles[6], 6,
                                              ifelse(x<deciles[7], 7,
                                                     ifelse(x<deciles[8], 8,
                                                            ifelse(x<deciles[9], 9, 10
                                                            ))))))))))
}

## deciling
misClassTable$deciles <- decile(misClassTable$Predict.Score)

#################################################################################################################################
## Ranking code
##install.packages("data.table")
library(data.table)
tmp_DT = data.table(misClassTable)
rank <- tmp_DT[, list(
cnt = length(Attrition), 
cnt_resp = sum(Attrition), 
cnt_non_resp = sum(Attrition == 0)) , 
by=deciles][order(-deciles)]
rank$rrate <- round (rank$cnt_resp / rank$cnt,2);
rank$cum_resp <- cumsum(rank$cnt_resp)
rank$cum_non_resp <- cumsum(rank$cnt_non_resp)
rank$cum_rel_resp <- round(rank$cum_resp / sum(rank$cnt_resp),2);
rank$cum_rel_non_resp <- round(rank$cum_non_resp / sum(rank$cnt_non_resp),2);
rank$ks <- abs(rank$cum_rel_resp - rank$cum_rel_non_resp);
#
library(scales)
rank$rrate <- percent(rank$rrate)
rank$cum_rel_resp <- percent(rank$cum_rel_resp)
rank$cum_rel_non_resp <- percent(rank$cum_rel_non_resp)

View(rank)
#################################################################################################################################
## Scoring another dataset using the Neural Net Model Object
## To score we will use the compute function

x1 <- subset(test, 
            select = c("Age","DailyRate","JobRole",
                       "EmployeeNumber", "YearsAtCompany","MonthlyIncome",
                       "EducationField","TotalWorkingYears","HourlyRate","MonthlyRate","DistanceFromHome",
                       "YearsSinceLastPromotion","OverTime","NumCompaniesWorked","JobInvolvement","EnvironmentSatisfaction",
                       "PercentSalaryHike","YearsWithCurrManager", "StockOptionLevel")
)
x1 <- test
y1 <- scale(x1)
y1 <- cbind(test[2], y1)

nn.testscaled <- cbind(y1[2],y1[4:9],y1[11:22],y1[24:27],y1[29:36])
                  
compute.output = compute(nn2, nn.testscaled)

test$Predict.Score = compute.output$net.result

#Quantile results
quantile(test$Predict.Score, c(0,1,5,10,25,50,75,90,95,99,100)/100)

# Plot results
plot(test$Attrition,test$Predict.Score,col='red',main='Real vs predicted NN',pch=18,cex=0.7)
legend('bottomright',legend='NN',pch=18,col='red', bty='n')

#By visually inspecting the plot we can see that the predictions made by the neural network 
#are (in general) more concentrated around either 0 or 1.  
#################################################################################################################################
## Building Neural Net Moodel using caret package
train1 <- cbind(train[1],train[3:8],train[10:21],train[23:26],train[28:35])

nn_trainplot = predict(
  preProcess(train1[,-2], method="range"),
  train1[,-2]
)

featurePlot(nn_trainplot[, lapply(nn_trainplot, class) %in% c("numeric", "integer")], 
            as.factor(train$Attrition), "box")


