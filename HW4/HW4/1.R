rm(list = ls())

library(ISLR)
library(MASS)
library(class)
library(ElemStatLearn)
library(neuralnet)
library(Metrics)
library(lasso2)
library(boot)
library(bootstrap)
library(leaps)
library(stats)

###########################
####   data loading   #####
###########################

data("prostate")
names(prostate)
prostate = prostate[,-c(10)]

#############################################
########### hold out method #################
#############################################

set.seed(1234)
train = sample(1:nrow(prostate), 0.75*nrow(prostate))
Y.train = prostate$lpsa[train]
Y.test = prostate$lpsa[-train]
X.train = prostate[train,]
X.test = prostate[-train,]

fit <- lm(lpsa ~ ., data = X.train)
pred.test <- predict(fit, newdata = X.test)
pred.train <- predict(fit, newdata = X.train)

test.error <- (1/length(Y.test))*sum((pred.test - Y.test)^2)
train.error <- (1/length(Y.train))*sum((pred.train - Y.train)^2)
test.error
train.error
AIC(fit)
BIC(fit)

#############################################################
## holdout method over a spectrum  of complexity paramters ##
#############################################################

fit1 <- regsubsets(lpsa~., data = X.train, method = "exhaustive", nvmax = 8)
my_summary <- summary(fit1)
names(my_summary)
my_summary$cp
my_summary$bic
which.min(my_summary$cp) #cp: 3 
which.min(my_summary$bic) #bic: 3 

## selection based on holdout method ##

select = summary(fit1)$outmat
train.error.values <- c()
test.error.values <- c()
for (i in 1:8){
  temp = which(select[i,] == "*")
  temp = temp + 1
  1
  red.training = X.train[, c(9,temp)]
  red.testing = X.test[,c(9,temp)]
  
  red.fit = lm(lpsa~., data = red.training)
  
  pred.train = predict(red.fit, newdata = red.training)
  pred.test = predict(red.fit, newdata = red.testing)
  
  test.error = (1/length(Y.test))*sum((pred.test - Y.test)^2)
  train.error = (1/length(Y.train))*sum((pred.train - Y.train)^2)
  
  train.error.values = c(train.error.values, train.error)
  test.error.values = c(test.error.values, test.error)
  
}

#train.error.values
#test.error.values

###############################################################
## bootstrap predition error for the best models of size "k" ##
###############################################################

beta.fit <- function(X,Y){
  lsfit(X,Y)	
}

beta.predict <- function(fit, X){
  cbind(1,X)%*%fit$coef	
}

sq.error <- function(Y,Yhat){
  (Y-Yhat)^2
}


X = prostate[,]
Y = prostate[,9]

error_store = c()
for (i in 1:8){
  temp = which(select[i,] == "*")
  
  res = bootpred(X[,temp], Y, nboot = 50, theta.fit = beta.fit, theta.predict = beta.predict, err.meas = sq.error) 
  error_store = c(error_store, res[[3]])
  
}

error_store

upper = max(train.error.values, test.error.values)
lower = min(train.error.values, test.error.values)
x11()
plot(train.error.values, type = "o", lty = 2, col = "blue", ylim = c(lower -1, upper +1) , xlab = "k", ylab = "error", main = "Model Selection")
lines(test.error.values, type = "o", lty = 1, col = "red")
lines(error_store, type = "o", lty = 3, col = "green")
legend("topright", c("train", "test", "bootstrap .632"), lty = c(2,1), col = c("blue", "red", "green"))

#######################################################
######     10-fold CV for model selection     #########
#######################################################

set.seed (101)

predict.regsubsets = function(object, newdata, id, ...) {
  form = as.formula(object$call[[2]])
  mat = model.matrix(form, newdata)
  coefi = coef(object, id = id)
  mat[, names(coefi)] %*% coefi
}

k=10
set.seed (1234567)
folds=sample (1:k,nrow(prostate),replace =TRUE)

cv.errors = matrix(NA,10,8)
for (j in 1:k) {
  best.fit = regsubsets(lpsa~ ., data = prostate[folds != j,], nvmax = 8)
  for (i in 1:8) {
    pred = predict(best.fit, prostate[folds == j, ], id = i)
    cv.errors[j, i] = mean((prostate$lpsa[folds == j] - pred)^2)
  }
}
rmse.cv = sqrt(apply(cv.errors, 2, mean))
rmse.cv
which.min(rmse.cv) # 7-variable model


###############################################
#####  5-fold CV for model selection  #########
###############################################

set.seed (102)

predict.regsubsets = function(object, newdata, id, ...) {
  form = as.formula(object$call[[2]])
  mat = model.matrix(form, newdata)
  coefi = coef(object, id = id)
  mat[, names(coefi)] %*% coefi
}

k=5
set.seed (123456)
folds=sample (1:k,nrow(prostate),replace =TRUE)

cv.errors = matrix(NA,5,8)
for (j in 1:k) {
  best.fit = regsubsets(lpsa~ ., data = prostate[folds != j,], nvmax = 8)
  for (i in 1:8) {
    pred = predict(best.fit, prostate[folds == j, ], id = i)
    cv.errors[j, i] = mean((prostate$lpsa[folds == j] - pred)^2)
  }
}
rmse.cv = sqrt(apply(cv.errors, 2, mean))
rmse.cv
which.min(rmse.cv) # 3-variable model
