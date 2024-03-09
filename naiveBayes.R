#Group project 20

#import all packages/libraries
library(partykit)
library(rpart)
library(prediction)
library(ROCR)
library(e1071)
library(pROC)
library(caret)

#step1 setting up data
#header set to false, because the csv first row are not headers
trainingCSV <-read.csv("training.csv", header = FALSE)
testingCSV <-read.csv("testing.csv", header = FALSE)

#removing column 1, which is the dog image names
dogTrain <- trainingCSV[, -1]
dogTest <- testingCSV[, -1]


#renaming column v2, to dog breed
colnames(dogTrain)[1] <- "dogBreed"
colnames(dogTest)[1] <- "dogBreed"

#factor the dog breeds
dogTrain$dogBreed = as.factor(dogTrain$dogBreed)
dogTest$dogBreed = as.factor(dogTest$dogBreed)

#fitting a naive bayes model with trianing data
nb = naiveBayes(dogBreed~., data = dogTrain)
#show the naive bayes model
nb
head(nb$tables)
#predict the values on test set
nb.pred = predict(nb, dogTest[,-1])
nb.pred
nb.predraw = predict(nb, dogTest[,-1], type = "raw")
nb.predraw
#default class predict
nb.predclass = predict(nb, dogTest[,-1], type = "class")
nb.predclass

#produce the confusion matrix for the model
confusionNB = with(dogTest, table(dogBreed, nb.pred))
confusionNB
summary(confusionNB)

#calculate the accuracy rate
sum(diag(confusionNB)) / sum (confusionNB) * 100

#create the ROC curve based on the prediction
roc_nb <- multiclass.roc(dogTest$dogBreed, as.numeric(nb.predclass))

#plot the curve
rs <- roc_nb[["rocs"]]
plot.roc(rs[[1000]])

auc_nb <- auc(roc_nb)
auc_nb

tuneGrid <- expand.grid(laplace = c(0, 1, 2),
                        usekernel = c(FALSE, TRUE),
                        adjust = c(FALSE, TRUE))

nb_tuned <- train(dogBreed ~ ., 
                  data = dogTrain, 
                  method = "naive_bayes", 
                  tuneGrid = tuneGrid, 
                  trControl = trainControl(method = "cv", number = 10))


nb_tuned
nb.pred.tuned = predict(nb_tuned, dogTest[,-1])
nb.pred.tuned

#produce the confusion matrix for the model
confusionNB_tuned = with(dogTest, table(dogBreed, nb.pred.tuned))
confusionNB_tuned
summary(confusionNB)

#calculate the accuracy rate
sum(diag(confusionNB_tuned)) / sum (confusionNB_tuned) * 100


nb.predclass_tuned = predict(nb_tuned, dogTest[,-1], type = "prob")
nb.predclass_tuned

nb.predraw_tuned = predict(nb_tuned, dogTest[,-1], type = "raw")
nb.predraw_tuned
roc_nb_tuned <- multiclass.roc(dogTest$dogBreed, nb.predclass_tuned)
roc_nb_tuned

#create the ROC curve based on the prediction
roc_nb_tuned2 <- multiclass.roc(dogTest$dogBreed, as.numeric(nb.predraw_tuned))
roc_nb_tuned2
rs_tuned <- roc_nb_tuned2[["rocs"]]
plot.roc(rs_tuned[[1000]])


auc_nb_tuned <- auc(roc_nb_tuned2)
auc_nb_tuned
