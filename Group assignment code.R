#install.packages("h5")
#rhdf5 package works really well, although it is not in CRAN.
#Install it from Bioconductor

source("http://bioconductor.org/biocLite.R")
biocLite("rhdf5")

#And to use it: 
library(rhdf5)

# set working directory
setwd("C:/GLIM/BDA/Group Assignment")
# close any open connections
H5close()
# set the file name for use.
fname <-  "train.h5"

#List the objects within the file to find the data group you want to read:
h5 <- h5ls(fname)
h5

# read attributes of file
h5readAttributes(fname,"train")

h5readAttributes(fname,"train/axis0")

h5readAttributes(file = "train.h5", 
                 name = "train/axis1")

h5readAttributes(file = "train.h5", 
                 name = "train/block0_items")

h5readAttributes(file = "train.h5", 
                 name = "train/block0_values")

h5readAttributes(file = "train.h5", 
                 name = "train/block1_items")

h5readAttributes(file = "train.h5", 
                 name = "train/block1_values")

# let's grab some data from the H5 file
#testSubset1 <- h5read(file = "train.h5", 
#                     name = "train")

testSubset1 <- h5read(file = "train.h5", 
                      name = "train/axis0",
                      index=NULL,stride =NULL)

testSubset2 <- h5read(file = "train.h5", 
                      name = "train/axis1",
                      index=NULL,stride =NULL)

testSubset3 <- h5read(file = "train.h5", 
                      name = "train/block0_items",
                      index=NULL,stride =NULL)

testSubset4 <- h5read(file = "train.h5", 
                      name = "train/block0_values",
                      index=NULL,stride =NULL)

testSubset5 <- h5read(file = "train.h5", 
                      name = "train/block1_items",
                      index=NULL,stride =NULL)

testSubset6 <- h5read(file = "train.h5", 
                      name = "train/block1_values",
                      index=NULL,stride =NULL)
H5close()



#Read the HDF5 data:
mydata1 <- h5read("train.h5","/train/axis0")
mydata2 <- h5read("train.h5","/train/axis1")
mydata3 <- h5read("train.h5","/train/block0_items")
mydata4 <- h5read("train.h5","/train/block0_values")
mydata5 <- h5read("train.h5","/train/block1_items")
mydata6 <- h5read("train.h5","/train/block1_values")

#And inspect the structure:
str(mydata1)
str(mydata2)
str(mydata3)
str(mydata4)
str(mydata5)
str(mydata6)

H5close()


library(rhdf5)
files <- list.files(pattern = ".h5", full.names = TRUE)
attribute <- "/train"
out.list <- lapply(files, h5read, attribute)
