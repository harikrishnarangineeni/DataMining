rm (list=ls())

library(ISLR)
#install.packages("e1071")
library(e1071)

data(OJ)
attach(OJ)

test_indis <- sample(1:nrow(OJ), 0.35*nrow(OJ))
test_oj <- OJ[test_indis,]
train_oj <- OJ[-test_indis,]

test_error_linear <- c()
train_error_linear <- c()

####  a  ####

#######  SVM with a Linear Kernel  ########

for (i in c(0.01,0.1, 1, 5, 10)){
  
  linear_oj_svm <- tune(svm, Purchase ~ .,data = train_oj, kernel = "linear",ranges = list(cost = i))
  
  best_model_oj <- linear_oj_svm$best.model
  best_model_oj
  
  
  ### predict test data ###
  
  y_hat <- predict(best_model_oj, newdata = test_oj)
  y_true <- test_oj$Purchase
  
  test_err <- length(which(y_true != y_hat))/length(y_true)
  test_error_linear<-c(test_error_linear,test_err)
  
  y_hat <- predict(best_model_oj, newdata = train_oj)
  y_true <- train_oj$Purchase
  
  train_err <- length(which(y_true != y_hat))/length(y_true)
  train_error_linear<-c(train_error_linear,train_err)
  
}

test_error_linear
train_error_linear

### Plotting ###

upper.lr = max(test_error_linear, train_error_linear)
lower.lr = min(test_error_linear, train_error_linear)

x11()
plot(train_error_linear, type = "o", lty = 2, col = "black", ylim = c(lower.lr -1, upper.lr +1) , xlab = "cost", ylab = "error", main = "Test and Training Errors")
lines(test_error_linear, type = "o", lty = 1, col = "red")
legend("topright", c("training", "test"), lty = c(2,1), col = c("black","red"))


##########  SVM with a Radial Kernel  ############

train_error_radial <- c()
test_error_radial<- c()

for (i in c(0.01,0.1, 1, 5, 10)){
  radial_oj_svm <- tune(svm, Purchase ~ .,data = train_oj, kernel = "radial",ranges = list(cost = i))
  
  best_model_oj <- radial_oj_svm$best.model
  best_model_oj
  
  ### predict test data ###
  
  y_hat <- predict(best_model_oj, newdata = test_oj)
  y_true <- test_oj$Purchase
  
  test_err <- length(which(y_true != y_hat))/length(y_true)
  test_error_radial<-c(test_error_radial,test_err)
  
  y_hat <- predict(best_model_oj, newdata = train_oj)
  y_true <- train_oj$Purchase
  
  train_err <- length(which(y_true != y_hat))/length(y_true)
  train_error_radial<-c(train_error_radial,train_err)
  
}

test_error_radial
train_error_radial

### Plotting ###

upper.lr = max(test_error_radial, train_error_radial)
lower.lr = min(test_error_radial, train_error_radial)

x11()
plot(train_error_radial, type = "o", lty = 2, col = "blue", ylim = c(lower.lr -1, upper.lr +1) , xlab = "cost", ylab = "error", main = "Test and Training Errors for Radial Kernel")
lines(test_error_radial, type = "o", lty = 1, col = "red")
legend("topright", c("training", "test"), lty = c(2,1), col = c("blue","red"))


##########  SVM with a Polynimal Kernel  ############


train_error_poly <- c()
test_error_poly<- c()

for (i in c(0.01,0.1, 1, 5, 10)){
  oj.poly.svm <- tune(svm, Purchase ~ .,data = train_oj, degree = 2, kernel = "polynomial",ranges = list(cost = i))
  
  best_model_oj <- oj.poly.svm$best.model
  best_model_oj
  
  ### predict test data ###
  
  y_hat <- predict(best_model_oj, newdata = test_oj)
  y_true <- test_oj$Purchase
  
  test_err <- length(which(y_true != y_hat))/length(y_true)
  test_error_poly<-c(test_error_poly,test_err)
  
  y_hat <- predict(best_model_oj, newdata = train_oj)
  y_true <- train_oj$Purchase
  
  train_err <- length(which(y_true != y_hat))/length(y_true)
  train_error_poly<-c(train_error_poly,train_err)
  
}

test_error_poly
train_error_poly

### Plotting ###

upper.lr = max(test_error_poly, train_error_poly)
lower.lr = min(test_error_poly, train_error_poly)

x11()
plot(train_error_poly, type = "o", lty = 2, col = "orange", ylim = c(lower.lr -1, upper.lr +1) , xlab = "cost", ylab = "error", main = "Test and Training Errors for polynomial Kernel")
lines(test_error_poly, type = "o", lty = 1, col = "blue")
legend("topright", c("training", "test"), lty = c(2,1), col = c("orange","blue"))
