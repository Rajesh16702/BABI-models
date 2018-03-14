##SMOTE Techniques - Synthetic Minority Oversampling Technique To Handle Class Imbalancy In Binary Classification.
### techniques 
### 1. Randomly select a minority point
### 2. We identify the K- nearest neighbours
### 3. randomly selcect one from the KNN
### 4. Randomly drop a point in between the 2 points.

install.packages("unbalanced")
#use Racing to select the best technique for an unbalanced dataset
library(unbalanced)
data(ubIonosphere)
#configure sampling parameters
ubConf <- list(type="ubUnder", percOver=200, percUnder=200, k=2, perc=50, method="percPos", w=NULL)
#load the classification algorithm that you intend to use inside the Race
#see 'mlr' package for supported algorithms
library(randomForest)
#use only 5 trees
results <- ubRacing(Class ~., ubIonosphere, "randomForest", positive=1, ubConf=ubConf, ntree=5)
# try with 500 trees
# results <- ubRacing(Class ~., ubIonosphere, "randomForest", positive=1, ubConf=ubConf, ntree=500)
# let's try with a different algorithm
# library(e1071)
# results <- ubRacing(Class ~., ubIonosphere, "svm", positive=1, ubConf=ubConf)
# library(rpart)
# results <- ubRacing(Class ~., ubIonosphere, "rpart", positive=1, ubConf=ubConf)
