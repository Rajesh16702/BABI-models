#install.packages("h5")
#rhdf5 package works really well, although it is not in CRAN.
#Install it from Bioconductor
source("http://bioconductor.org/biocLite.R")
biocLite("rhdf5")
library(rhdf5)
library(ggplot2) 
library(readr) 
library(corrplot)
library(xgboost)

# set working directory
setwd("C:/GLIM/BDA/Group Assignment")
# close any open connections
H5close()
# set the file name for use.
fname <-  "train.h5"

# Adapted from http://pandas.pydata.org/pandas-docs/stable/io.html#io-external-compatibility 
read_hdf <- function(h5path, dataframe_name=NULL) {
  h5File <- H5Fopen(h5path, flags="H5F_ACC_RDONLY")
  listing <- h5ls(h5File)
  
  if (is.null(dataframe_name)) {
    dataframe_name <- listing$name[1]
  }
  
  group_name <- paste0("/", dataframe_name)
  
  # Filter to just the requested dataframe:
  listing <- listing[listing$group == group_name,]
  
  # Find all data nodes, values are stored in *_values and corresponding column
  # titles in *_items
  data_nodes <- grep("_values$", listing$name)
  name_nodes <- grep("_items$", listing$name)
  data_paths = paste(listing$group[data_nodes], listing$name[data_nodes], sep = "/")
  name_paths = paste(listing$group[name_nodes], listing$name[name_nodes], sep = "/")
  columns = list()
  for (idx in seq(data_paths)) {
    # NOTE: matrices returned by h5read have to be transposed to to obtain
    # required Fortran order!
    data <- data.frame(t(h5read(h5File, data_paths[idx])))
    names <- t(h5read(h5File, name_paths[idx]))
    entry <- data.frame(data)
    colnames(entry) <- names
    columns <- append(columns, entry)
  }
  
  data <- data.frame(columns)
  
  # If "axis0" is specified, we can return the dataframe columns in the original order:
  if ("axis0" %in% listing$name) {
    orig_col_order <- h5read(h5File, paste0(group_name, "/axis0"))
    data <- data[orig_col_order]
  }
  
  H5Fclose(h5File)
  return(data)
}

dataset <- read_hdf("train.h5")
class(dataset)
dataset <- as.data.frame(dataset)
labs <- dataset$y




indexes = sample(1:nrow(dataset), size=0.25*nrow(dataset))

# Split data
test = dataset[indexes,]
head(test)  # 427,689 x 111
train = dataset[-indexes,]
head(train) # 1,283,067 x 111
train <- as.data.frame(train)
rmv<- c("id","timestamp","y")

trainlabs <- train$y
dim(trainlabs)
dat <- train[ , !(colnames(train) %in% rmv)]
class(dat)
dim(dat) #1,283,067 x 108

testlabs <- test$y
testdat <- test[ , !(colnames(test) %in% rmv)]
dim(testdat) #427,689 x 108

Traindata <- xgb.DMatrix(data = as.matrix(dat), missing = NA, label=as.numeric(trainlabs))
Testdata <- xgb.DMatrix(data = as.matrix(testdat), missing = NA, label=as.numeric(testlabs))

watchlist <- list(train=Traindata, test=Testdata)
set.seed(35)
bst <- xgb.train(data=Traindata,
                 max.depth=3,
                 eta=1, 
                 subsample=0.6,
                 colsample_bytree=0.9,
                 min_child_weight = 500,
                 nthread = 6, 
                 nround=50, 
                 watchlist=watchlist, 
                 objective = "reg:linear", 
                 early_stopping_rounds = 5)

bestTree <- bst$best_ntreelimit
pre <- predict(bst,as.matrix(testdat), missing=NA, ntreelimit =bestTree )
head(pre)
