

library(kohonen)
library(ggplot2)
library(sp)
library(maptools)
library(reshape2)
library(rgeos)
library(Hmisc)
library(MASS)
library("gplots")

# Read in data
# header FALSE stops the first row from being taken as a header
dogtrain <- read.csv('./training.csv', header = FALSE)
dogtest <- read.csv('./testing.csv', header = FALSE)

# First column is a filename for the pictures so its not really needed
# Feel free to shorten the dataframe names if you wish
# Drop first column 
dogTrainD = subset(dogtrain, select = -c(V1))
dogTestD = subset(dogtest, select = -c(V1))

# Column 2 (new column 1) holds categorical data corresponding to dog breeds
# Eg 1 is poodle (its not really poodle thats an example)
# Change new first column name to target
colnames(dogTrainD)[1] <- "target"
colnames(dogTestD)[1] <- "tar"

# The meadian is 60, 
#this shows most of the dog breeds are 60 for train data
# The meadian is arounf 50-60, 
#this shows most of the dog breeds are around 50 - 60 for test data
median(dogTrainD$target) # median = 60.5
median(dogTestD$tar) # medain = 54

boxplot(dogTrainD$target, dogTestD$tar,main="Dog Boxplot",
        ylab="Number of Dog Breeds",
        xlab = " train set    &   test set",
        col="orange")

#to further support the boxplot created histograms to show the bog breed data  
hist(dogTrainD$target, main = "Histogram of Training Dog data", xlab="Dog train",labels=T)
hist(dogTestD$tar,main = "Histogram of Testing Dog data", xlab="Dog test",labels=T)


