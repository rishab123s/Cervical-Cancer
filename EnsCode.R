#Installing packaging 
install.packages("caret")
install.packages("NeuralNetTools")
install.packages("pROC")
#Loading required libraries
library(caret)
library(NeuralNetTools)
require(pROC)
#Reading data from the working directory
CC<- read.csv("risk.csv", header=TRUE)
#setting seed for reproducibility
set.seed(2)
#Shuffling the data
n <- nrow(CC)
shuffled_data <- CC[sample(n),]
#Splitting the data into 70% - 30%
Cervical_train <- 1:round(0.7 * n)
training <- shuffled_data[Cervical_train, ]
Cervical_test<- (round(0.7 * n) + 1):n
testing <- shuffled_data[Cervical_test, ]
#training five models namely Random forest, GBM,J48,Neural net, KNN
Randomf <- train(as.factor(Biopsy) ~., data=training, method='rf')
v<- varImp(RandomF, scale = FALSE)
plot(v)
Gbmm <- train(as.factor(Biopsy) ~., data=training, method='gbm')
j48d<- train(as.factor(Biopsy) ~., data=training, method='J48')
nnetm <- train(as.factor(Biopsy) ~., data=training, method='nnet')
print(nnetm)
plotnet(nnetm)
plot(nnetm)
knnm <- train(as.factor(Biopsy) ~., data=training, method='kknn')
plot(knnm)
print(knnm)
#Predicting model with testing values
r1 <- predict(Randomf, newdata=testing)
r2 <- predict(Gbmm, newdata=testing)
r3 <- predict(j48d, newdata=testing)
r4 <- predict(nnetm, newdata=testing)
r5 <- predict(knnm, newdata=testing)
#Creating data frame from predictor table
Dframe <- data.frame(r1, r2,r3,r4,r5,  Biopsy = testing$Biopsy)
#ensembling using extreme gradient boosting
ens <- train(as.factor(Biopsy) ~ ., data = Dframe, method = "xgbLinear")
print(ens)
#Predictor model for ensemble
r6 <- predict(ens, newdata = testing)
#Plotting ROC curve for ensemble model
preprobs <- predict(ens,testing,type='prob')
roc<- multiclass.roc(testing$Biopsy,preprobs[,2])
print(roc)
auc(roc)
plot(roc(testing$Biopsy,preprobs[,2]))

#Creating confusion matrix for diifernet models
cmatrix1 <- confusionMatrix(r1, testing$Biopsy,positive = "1") 
cmatrix1
cmatrix2 <- confusionMatrix(r2, testing$Biopsy,positive = "1") 
cmatrix2
cmatrix3 <- confusionMatrix(r3, testing$Biopsy,positive = "1") 
cmatrix3
cmatrix4 <- confusionMatrix(r4, testing$Biopsy,positive = "1") 
cmatrix4
cmatrix5 <- confusionMatrix(r5, testing$Biopsy,positive = "1") 
cmatrix5
#Creating confusion matrix for ensemble model
cmatrix6 <- confusionMatrix(r6, ens$Biopsy,positive = "1")
cmatrix6


