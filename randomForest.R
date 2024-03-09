#import library
library(randomForest)
library(caret)
library(pROC)
library(e1071)
library(prediction)

# Read in data
# header FALSE stops the first row from being taken as a header
dogtrain <- read.csv('./training.csv', header = FALSE)
dogtest <- read.csv('./testing.csv', header = FALSE)

### Data pre-processing ###

# First column is a filename for the pictures so its not really needed
# Drop first column
dogTrainD = subset(dogtrain, select = -c(V1))
dogTestD = subset(dogtest, select = -c(V1))

# Column 2 (new column 1) holds categorical data corresponding to dog breeds
# Change new first column name to target, which is what we are predicting
colnames(dogTrainD)[1] <- "target"
colnames(dogTestD)[1] <- "target"


# Check for null values
print("Position of missing values -")
which(is.na(dogTrainD))

# Count total missing values 
print("Count of total missing values - ")
sum(is.na(dogTrainD))

# Check for null values
print("Position of missing values -")
which(is.na(dogTestD))

# Count total missing values 
print("Count of total missing values - ")
sum(is.na(dogTestD))

#no missing values
#############################

# fit a random forest model to the training set
rf.dog.train = randomForest(as.factor(target)~., data = dogTrainD)
rf.dog.train
# fit model onto testing set
ref.test.pred = predict(rf.dog.train, dogTestD, type='class')

# produce confusion matrix and compute accuracy
confusionRF = with(dogTestD, table(target, ref.test.pred))
confusionRF
sum(diag(confusionRF))/sum(confusionRF)*100

# show ROC curve on column 2 as example
ROC_rf <- multiclass.roc(dogTestD$target, as.numeric(ref.test.pred))
rs <- ROC_rf[['rocs']]
# this is one of the ROC curves for one of the prediction
#it shows the sensitivity and specificity
# plot.roc(rs) to view all the predictions made using column 2
plot.roc(rs[[1]])

# tune using algorithm provided in the random Forest library
set.seed(7)
rf.dog.tune = randomForest(as.factor(target)~., data = dogTrainD, mtry=10, ntree=750)
rf.dog.tune
rf.pred.tune = predict(rf.dog.tune, dogTestD,type='class')

# produce confusion matrix and compute accuracy of the tuned model
# drop in accuracy, auto tuning takes too long due to size of data
confusionRF_tuned = with(dogTestD, table(rf.pred.tune, target))
confusionRF_tuned
sum(diag(confusionRF_tuned))/sum(confusionRF_tuned)*100

# tune using cross validation
# tuning took too lone, because of the size of the data
# no output even after a night of tuning
control <- trainControl(method="repeatedcv", number=5, repeats=3, search="random")
mtry <- sqrt(ncol(dogTrainD[,2:120]))
rf_random <- train(as.factor(target)~., data=dogTrainD, method="rf",  metric='Accuracy', tuneLength=15, trControl=control)
print(rf_random)
