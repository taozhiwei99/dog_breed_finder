library(e1071)
library(prediction)
library(ROCR)
library(ISLR)

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

# Fit training data into the svm model
svmfit = svm(target~., data=dogTrainD, kernel="radial", type = "C-classification")

# Predict values based on training data
predict(svmfit, decision.values = TRUE)

# Predict values using test data
svm.pred = predict(svmfit, dogTestD[,-1])

# Generate the confusion matrix of SVM model
confusionSVM = with(dogTestD, table(svm.pred, target))
#confusionSVM

# Calculate accuracy rate
sum(diag(confusionSVM))/sum(confusionSVM)*100

# Improve SVM model through adjusting hyperparameters
svmTuned = svm(target ~ ., data = dogTrainD, type = "C-classification", kernel = "radial", cost=10, gamma = 0.001)

# Predict values using test data
svmTuned.pred = predict(svmTuned, dogTestD[,-1])

# Produce confusion matrix using tuned SVM model
confusionTunedSVM = with(dogTestD, table(svmTuned.pred, target))
confusionTunedSVM

# Calculate accuracy of the tuned model
sum(diag(confusionTunedSVM))/sum(confusionTunedSVM)*100  

summary((tune.svm((target==1) ~ ., data = dogTrainD,
                  kernel = "radial",cost = 10^c(-2:2),
                  gamma = 10^c(-4:1), type='C-classification')), (svm2 = svm(I(target == 1)~ ., data = dogTrainD, type = "C")))

# Fit model in SVM
svm.fit = svm((target==1)~., data=dogTrainD, type='C-classification', gamma=0.001, cost=100, kernel='radial')

# Predict the values on test set[SVM]
svm.fit.pred = predict(svm.fit, dogTestD[,-1], decision.values =TRUE)
svm.fit.pred
-attr(svm.fit.pred, "decision.values")

# Make prediction using SVM
confusionSVM = prediction(-attr(svm.fit.pred, "decision.values"), dogTestD$target == 1)

# Create rocs curve based on prediction
rocsSVM <- performance(confusionSVM, "tpr", "fpr")

# Plot the graph
plot(rocsSVM, col=1)
abline(0, 1, lty = 3)

# Add the legend to the graph
legend(0.6, c('svm'), 1:2)

library(writexl)

write_xlsx(confusionSVM, 'D:\\Uni stuff\\INFO411 - Data Mining and Knowledge Discovery\\svmROC.xlsx')