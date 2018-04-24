rm(list=ls())

library("MASS")

data = read.table("C:/Users/harik/Desktop/STA/HW3/DiabetesAndrews36_1.txt")
data = data[5:10]
data

names(data) = c("glucose.area", "insulin.area", "SSPG", "relative.weight", "fasting.plasma.glucose", "classnumber")

data$classnumber = as.factor(data$classnumber)


# pairwise scatterplots.

library(ggplot2)
library(GGally)
ggpairs(data, columns = 1:5)
ggpairs(data, columns = 1:5, ggplot2::aes(colour=classnumber))

####################
### LDA and QDA  ###
####################

### Creating a training and test set ###

train <- sample(1:nrow(data), nrow(data)*0.7)
data_train <- data[train, ]
data_test <- data[-train, ]


#LDA model

lda.fit <- lda(classnumber ~ ., data = data_train)
lda.pred.train <- predict(lda.fit, newdata = data_train[,1:5])
lda.pred.test <- predict(lda.fit, newdata = data_test[,1:5])
y_hat_test_lda <- as.numeric(lda.pred.test$class)
y_hat_test_lda

test_err_lda <- sum(abs(y_hat_test_lda - as.numeric(data_test$classnumber)))/length(data_test$classnumber)
test_err_lda

squared_test_err_lda <- sum(abs(y_hat_test_lda - as.numeric(data_test$classnumber))^2)
squared_test_err_lda


#QDA model

qda.fit <- qda(classnumber ~ ., data = data_train)
qda.pred.train <- predict(qda.fit, newdata = data_train[,1:5])
qda.pred.test <- predict(qda.fit, newdata = data_test[,1:5])
y_hat_test_qda <- as.numeric(qda.pred.test$class)
y_hat_test_qda

test_err_qda <- sum(abs(y_hat_test_qda - as.numeric(data_test$classnumber)))/length(data_test$classnumber)
test_err_qda

squared_test_err_qda <- sum(abs(y_hat_test_qda - as.numeric(data_test$classnumber))^2)
squared_test_err_qda

######################

new_individual = data.frame(0.98, 122, 544, 186, 184)
names(new_individual) = names(data_train)[1:5]


#lda prediction. 
predict(lda.fit, new_individual)$class

#qda prediction.
predict(qda.fit, new_individual)$class
