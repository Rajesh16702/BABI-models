## Author: Rajesh Jakhotia
## Company Name: K2 Analytics Finishing School Pvt. Ltd
## Email : ar.jakhotia@k2analytics.co.in
## Website : k2analytics.co.in


## Let us first set the working directory path

setwd ("D:/K2Analytics/Datafile")
getwd()

## Let us import the data that we intend to use for modeling

CTDF <- read.table("CF_TREE_SAMPLE.csv",
        sep = ",", header = T)
nrow(CTDF)
sum(CTDF$Target)
View(CTDF)

## Target Rate may also be called as:
## Response Rate in Marketing 
## Default Rate in Risk
## Churn Rate in Attrition
Target_Rate = sum(CTDF$Target) / nrow(CTDF)
Target_Rate
## Let us quickly understand the structure of our data
str(CTDF)
CTDF$Target = as.factor(CTDF$Target)

summary(CTDF)


tbl <- table(CTDF$Occupation, CTDF$Target)
tbl
chisq.test(tbl)
## Creating Development and Validation Sample
## CTDF$random <- runif(nrow(CTDF), 0, 1);
## CTDF <- CTDF[order(CTDF$random),]
## CTDF.dev <- CTDF[which(CTDF$random <= 0.7),]
## CTDF.val <- CTDF[which(CTDF$random > 0.7),]
## c(nrow(CTDF.dev), nrow(CTDF.val))

 
 


## Installing the CHAID package; If already installed do not run the below step
##install.packages("partykit")
##install.packages("CHAID", repos="http://R-Forge.R-project.org")

library(CHAID)
ctrl <- chaid_control(minbucket = 100, minsplit = 10, alpha2=.05, alpha4 = .05)

with(CTDF, table(Holding_Period, Target))

chaid.tree <- chaid(Target ~ as.factor(Holding_Period), data = CTDF, control = ctrl)
print(chaid.tree)

chaid.tree <-chaid(Target~Occupation+Gender+AGE_BKT,data=CTDF, control = ctrl)
print(chaid.tree)
plot(chaid.tree)

##chaid.tree <-chaid(Target~Occupation+Gender+AGE_BKT,data=CTDF, control = ctrl)


CTDF.dev <- read.table("DEV_SAMPLE.csv", sep = ",", header = T)
CTDF.holdout <- read.table("HOLDOUT_SAMPLE.csv", sep = ",", header = T)
c(nrow(CTDF.dev), nrow(CTDF.holdout))

str(CTDF.dev)

View(CTDF.dev)

## installing rpart package for CART
## install.packages("rpart")
## install.packages("rpart.plot")


## loading the library
library(rpart)
library(rpart.plot)


## setting the control paramter inputs for rpart
r.ctrl = rpart.control(minsplit=100, minbucket = 10, cp = 0, xval = 10)


## calling the rpart function to build the tree
##m1 <- rpart(formula = Target ~ ., data = CTDF.dev[which(CTDF.dev$Holding_Period>10),-1], method = "class", control = r.ctrl)
m1 <- rpart(formula = Target ~ ., data = CTDF.dev[,-1], method = "class", control = r.ctrl)
m1

sum(CTDF.dev$Target)/14000
## install.packages("rattle")
## install.packages("RcolorBrewer")
library(rattle)
library(RColorBrewer)
fancyRpartPlot(m1)


## to find how the tree performs
printcp(m1)
plotcp(m1)

##rattle()
## Pruning Code
ptree<- prune(m1, cp= 0.0041,"CP")
printcp(ptree)
fancyRpartPlot(ptree, uniform=TRUE,  main="Pruned Classification Tree")


## Let's use rattle to see various model evaluation measures
rattle()


## Scoring syntax
CTDF.dev$predict.class <- predict(ptree, CTDF.dev, type="class")
CTDF.dev$predict.score <- predict(ptree, CTDF.dev)


View(CTDF.dev)
##head(CTDF.dev$predict.score[,2])


## deciling code
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
CTDF.dev$deciles <- decile(CTDF.dev$predict.score[,2])


## Ranking code
library(data.table)
tmp_DT = data.table(CTDF.dev)
rank <- tmp_DT[, list(
  cnt = length(Target), 
  cnt_resp = sum(Target), 
  cnt_non_resp = sum(Target == 0)) , 
  by=deciles][order(-deciles)]
rank$rrate <- round(rank$cnt_resp * 100 / rank$cnt,2);
rank$cum_resp <- cumsum(rank$cnt_resp)
rank$cum_non_resp <- cumsum(rank$cnt_non_resp)
rank$cum_perct_resp <- round(rank$cum_resp * 100 / sum(rank$cnt_resp),2);
rank$cum_perct_non_resp <- round(rank$cum_non_resp * 100 / sum(rank$cnt_non_resp),2);
rank$ks <- abs(rank$cum_perct_resp - rank$cum_perct_non_resp);
View(rank)



library(ROCR)
pred <- prediction(CTDF.dev$predict.score[,2], CTDF.dev$Target)
perf <- performance(pred, "tpr", "fpr")
plot(perf)
KS <- max(attr(perf, 'y.values')[[1]]-attr(perf, 'x.values')[[1]])
auc <- performance(pred,"auc"); 
auc <- as.numeric(auc@y.values)

library(ineq)
gini = ineq(CTDF.dev$predict.score[,2], type="Gini")

with(CTDF.dev, table(Target, predict.class))
auc
KS
gini

## Syntax to get the node path
tree.path <- path.rpart(m1, node = c(12590))



## Scoring Holdout sample
CTDF.holdout$predict.class <- predict(ptree, CTDF.holdout, type="class")
CTDF.holdout$predict.score <- predict(m1, CTDF.holdout)

with(CTDF.holdout, table(Target, predict.class))

## Building the model using Random Forest

## importing the data
RFDF.dev <- read.table("datafile/DEV_SAMPLE.csv", sep = ",", header = T)
RFDF.holdout <- read.table("datafile/HOLDOUT_SAMPLE.csv", sep = ",", header = T)
c(nrow(RFDF.dev), nrow(RFDF.holdout))

##install.packages("randomForest")
library(randomForest)
?randomForest
## Calling syntax to build the Random Forest
RF <- randomForest(as.factor(Target) ~ ., data = RFDF.dev[,-1], 
                   ntree=500, mtry = 3, nodesize = 10,
                   importance=TRUE)
print(RF)

plot(RF, main="")
legend("topright", c("OOB", "0", "1"), text.col=1:6, lty=1:3, col=1:3)
title(main="Error Rates Random Forest RFDF.dev")


RF$err.rate
##class(randomForest::importance(RF))
## List the importance of the variables.
##impVar <- round(randomForest::importance(RF), 2)
##impVar[order(impVar[,3], decreasing=TRUE),]

## Tuning Random Forest
tRF <- tuneRF(x = RFDF.dev[,-c(1,2)], 
              y=as.factor(RFDF.dev$Target),
              mtryStart = 3, 
              ntreeTry=100, 
              stepFactor = 1.5, 
              improve = 0.001, 
              trace=TRUE, 
              plot = TRUE,
              doBest = TRUE,
              nodesize = 140, 
              importance=TRUE
              )



## Scoring syntax
RFDF.dev$predict.class <- predict(tRF, RFDF.dev, type="class")
RFDF.dev$predict.score <- predict(tRF, RFDF.dev, type="prob")
head(RFDF.dev)

## deciling
RFDF.dev$deciles <- decile(RFDF.dev$predict.score[,2])


## Ranking code
library(data.table)
tmp_DT = data.table(RFDF.dev)
rank <- tmp_DT[, list(
  cnt = length(Target), 
  cnt_resp = sum(Target), 
  cnt_non_resp = sum(Target == 0)) , 
  by=deciles][order(-deciles)]
rank$rrate <- round(rank$cnt_resp * 100 / rank$cnt,2);
rank$cum_resp <- cumsum(rank$cnt_resp)
rank$cum_non_resp <- cumsum(rank$cnt_non_resp)
rank$cum_rel_resp <- round(rank$cum_resp / sum(rank$cnt_resp),2);
rank$cum_rel_non_resp <- round(rank$cum_non_resp / sum(rank$cnt_non_resp),2);
rank$ks <- abs(rank$cum_rel_resp - rank$cum_rel_non_resp);
View(rank)



library(ROCR)
pred <- prediction(RFDF.dev$predict.score[,2], RFDF.dev$Target)
perf <- performance(pred, "tpr", "fpr")
plot(perf)
KS <- max(attr(perf, 'y.values')[[1]]-attr(perf, 'x.values')[[1]])
auc <- performance(pred,"auc"); 
auc <- as.numeric(auc@y.values)

library(ineq)
gini = ineq(RFDF.dev$predict.score[,2], type="Gini")

with(RFDF.dev, table(Target, predict.class))
auc
KS
gini


## Scoring syntax
RFDF.holdout$predict.class <- predict(tRF, RFDF.holdout, type="class")
RFDF.holdout$predict.score <- predict(tRF, RFDF.holdout, type="prob")
with(RFDF.holdout, table(Target, predict.class))

RFDF.holdout$deciles <- decile(RFDF.holdout$predict.score[,2])
tmp_DT = data.table(RFDF.holdout)
rank <- tmp_DT[, list(
  cnt = length(Target), 
  cnt_resp = sum(Target), 
  cnt_non_resp = sum(Target == 0)) , 
  by=deciles][order(-deciles)]
rank$rrate <- round(rank$cnt_resp * 100 / rank$cnt,2);
rank$cum_resp <- cumsum(rank$cnt_resp)
rank$cum_non_resp <- cumsum(rank$cnt_non_resp)
rank$cum_rel_resp <- round(rank$cum_resp / sum(rank$cnt_resp),2);
rank$cum_rel_non_resp <- round(rank$cum_non_resp / sum(rank$cnt_non_resp),2);
rank$ks <- abs(rank$cum_rel_resp - rank$cum_rel_non_resp);
View(rank)



## C50
##install.packages("C50")
library(C50)
C50DF.dev <- read.table("datafile/DEV_SAMPLE.csv", sep = ",", header = T)
C50model <- C5.0(x = C50DF.dev[,-c(1,2)], 
                 y=as.factor(C50DF.dev$Target))
summary(C50model)
plot(C50model)

predict.class <- predict(C50model, C50DF.dev, type="class")
table(C50DF.dev$Target, predict.class)

