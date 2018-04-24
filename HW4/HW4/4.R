rm (list=ls())

#install.packages("kernlab")
library(kernlab)
library(ggplot2)
library(randomForest)

spam_data = data(spam)
spam_data = spam
dim(spam_data)
set.seed(12345)
trainIndex <- sample(1:nrow(spam_data), .69*nrow(spam_data))
spam_train <- spam_data[trainIndex,]
spam_test <- spam_data[-trainIndex,]


modelName = c()
test_error_vector = c()
train_error_vector = c()

for(i in seq(1,15,2)){
  rf.fit = randomForest(type~.,data = spam_train, ntree =100, mtry = i)
  
  rf.fit.pred_test <- predict(rf.fit, newdata = spam_test,type='class')
  rf.fit.pred_train <- predict(rf.fit, newdata = spam_train, type='class')
  
  rf_test_err <- mean(rf.fit.pred_test != spam_test$type)
  rf_train_err <- mean(rf.fit.pred_train != spam_train$type)
  
  modelName = c(modelName,i)
  test_error_vector = c(test_error_vector,rf_test_err)
  train_error_vector = c(train_error_vector,rf_train_err)
}

errorDF = data.frame(Model_Name = modelName,Training_Error = train_error_vector,Test_Error = test_error_vector)

plot(errorDF$Model_Name,errorDF$Test_Error,type='o')



collection = rep(0, 100)
for(i in seq(1,15,2)){
  rf.fit = randomForest(type~., data  = spam_train, ntree = 100, mtry = i)
  rf.predictions = predict(rf.fit, spam_test, type = "class")
  collection = cbind(collection, rf.fit$err.rate[,c(1)])
}

#out of bag errors.

collection_plot = data.frame(collection)[,-c(1)]
names(collection_plot) = c("mtry_2", "mtry_4","mtry_6","mtry_8","mtry_10")
collection_plot$NTrees = seq(1, 100)

#plotting

ggplot(collection_plot, aes(NTrees)) + 
  geom_line(aes(y = mtry_2, colour = "mtry_2")) + geom_point(aes(y = mtry_2), size = 0.3) + 
  geom_line(aes(y = mtry_4, colour = "mtry_4")) + geom_point(aes(y = mtry_4), size = 0.3) + 
  geom_line(aes(y = mtry_6, colour = "mtry_6")) + geom_point(aes(y = mtry_6), size = 0.3) + 
  geom_line(aes(y = mtry_8, colour = "mtry_8")) + geom_point(aes(y = mtry_8), size = 0.3) + 
  geom_line(aes(y = mtry_10, colour = "mtry_10")) + geom_point(aes(y = mtry_10), size = 0.3) + 
  
  ggtitle("Out of Bag Errors.") + xlab("No of features") + ylab("OOB Error") +
  theme(plot.title = element_text(hjust = 0.5))