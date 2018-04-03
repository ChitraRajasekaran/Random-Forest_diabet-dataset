#Random Forest Algorithm
head(diabet)
#split the dataset into training and testing set
set.seed(4)
id<-sample(2,nrows(diabet),prob = c(0.7,0.3), replace = TRUE)
training<-diabet[id==1,]
testing<-diabet[id==2,]


#convert the target variable into the factor form since the raget variable is "YES/NO' and other variables are in numeric format
diabet$type <- as.factor(diabet$type)
class(diabet$type)
training$type<- as.factor(training$type)


#optimised value of m (random variables that i will be choosing)
bestmtry<- tuneRF(training, training$type, stepFactor = 1.2, improve = 0.01, trace = T, plot = T)

library(randomForest)
RFModel<- randomForest(type~.,data = training)
RFModel

importance(RFModel)
varImpPlot(RFModel)
#test our model
RFPred<-predict(RFModel, newdata = testing, type = "class")
RFPred

# validate our model futher
library(caret)
confusionMatrix(table(RFPred,testing$type))

#Accuracy of the model is 76.74%