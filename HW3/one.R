rm(list=ls())

library("ISLR")
library("MASS")
library("class")
data(Boston)
names(Boston)
dim(Boston)

###Adding a new column to Boston dataset that indicates crime rate above Median ###

crimabvmedian <- rep(0, length(Boston$crim))
crimabvmedian[Boston$crim > median(Boston$crim)] = 1
Boston <- data.frame(Boston, crimabvmedian)
summary(Boston)

### Creating a training and test set ###

set.seed(12345)
train <- sample(1:nrow(Boston), nrow(Boston)*.75, rep=FALSE)
Boston_train <- Boston[train, ]
Boston_test <- Boston[-train, ]
crimabvmedian_test <- crimabvmedian[-train]


### Logistic Regression (without subset)###

glm.fit <- glm(crimabvmedian ~ . - crimabvmedian - crim, data = Boston, family = "binomial")
summary(glm.fit)
names(glm.fit)

glm.probs.train <- predict(glm.fit, newdata = Boston_train, type="response")
Y_hat_train <- round(glm.probs.train)

glm.probs.test <- predict(glm.fit, newdata = Boston_test, type="response")
Y_hat_test <- round(glm.probs.test)

table(Y_hat_test,Boston_test$crimabvmedian)

test_err_LR <- sum(abs(Y_hat_test-Boston_test$crimabvmedian))/length(Boston_test$crimabvmedian)
test_err_LR

###  subset using different plots ###

Boston.train.new <- Boston_train
Boston.train.new$crimabvmedian <- factor(Boston.train.new$crimabvmedian)
library(GGally)
library(ggplot2)
ggpairs(Boston.train.new, ggplot2::aes(colour=crimabvmedian))
pairs(Boston_train)

## finding correlation ##
cor(Boston[,-1]) ### The correlation of INDUS,NOX,RAD,MEDV,AGE,TAX,PTRATIO looks good ###
### So, taking these 7 predictors into account ###


### Logistic Regression (on subset predictors)###


glm.fit1 <- glm(crimabvmedian ~ nox+rad+medv+age+tax+indus+ptratio, data = Boston, family = "binomial")
summary(glm.fit1)

glm.probs.train1 <- predict(glm.fit1, newdata = Boston_train, type="response")
Y_hat_train1 <- round(glm.probs.train1)

glm.probs.test1 <- predict(glm.fit1, newdata = Boston_test, type="response")
Y_hat_test1 <- round(glm.probs.test1)

table(Y_hat_test1,Boston_test$crimabvmedian)

test_err_LR1 <- sum(abs(Y_hat_test1-Boston_test$crimabvmedian))/length(Boston_test$crimabvmedian)
test_err_LR1

### LDA ###

lda.fit <- lda(crimabvmedian ~ lstat+rm+zn+nox+dis+rad+ptratio+black+medv+age+chas+indus+tax, data = Boston)

lda.pred.train <- predict(lda.fit, newdata = Boston_train)
lda.pred.test <- predict(lda.fit, newdata = Boston_test)
y_hat_test_lda <- as.numeric(lda.pred.test$class)-1
y_hat_test_lda

table(lda.pred.test$class, Boston_test$crimabvmedian)

test_err_lda <- sum(abs(y_hat_test_lda - Boston_test$crimabvmedian))/length(Boston_test$crimabvmedian)
test_err_lda

### subset LDA ###

cor(Boston[,-1]) ### The correlation of INDUS,NOX,RAD,MEDV,AGE,TAX,PTRATIO looks good ###
### So, taking only nox,rad,medv,age,tax predictors into account(subset) ###

lda.fit1 <- lda(crimabvmedian ~ nox+rad+medv+age+tax, data = Boston)

lda.pred.train1 <- predict(lda.fit1, newdata = Boston_train)
lda.pred.test1 <- predict(lda.fit1, newdata = Boston_test)
y_hat_test_lda1 <- as.numeric(lda.pred.test1$class)-1
y_hat_test_lda1

table(lda.pred.test1$class, Boston_test$crimabvmedian)

test_err_lda1 <- sum(abs(y_hat_test_lda1 - Boston_test$crimabvmedian))/length(Boston_test$crimabvmedian)
test_err_lda1


### KNN ###

set.seed(123)

Boston_train_knn = Boston_train[,c("nox","rad","medv","age","tax","ptratio","indus")]
Boston_test_knn =  Boston_test[,c("nox","rad","medv","age","tax","ptratio","indus")]

knn.pred1 = knn(Boston_train_knn,Boston_test_knn,Boston_train$crimabvmedian,k=1)
table(knn.pred1,Boston_test$crimabvmedian)
y_hat_test_knn1 <- as.numeric(knn.pred1)-1
test_err_knn1 <- sum(abs(y_hat_test_knn1 - Boston_test$crimabvmedian))/length(Boston_test$crimabvmedian)
test_err_knn1


knn.pred2 = knn(Boston_train_knn,Boston_test_knn,Boston_train$crimabvmedian,k=2)
table(knn.pred2,Boston_test$crimabvmedian)
y_hat_test_knn2 <- as.numeric(knn.pred2)-1
test_err_knn2 <- sum(abs(y_hat_test_knn2 - Boston_test$crimabvmedian))/length(Boston_test$crimabvmedian)
test_err_knn2


knn.pred3 = knn(Boston_train_knn,Boston_test_knn,Boston_train$crimabvmedian,k=3)
table(knn.pred3,Boston_test$crimabvmedian)
y_hat_test_knn3 <- as.numeric(knn.pred3)-1
test_err_knn3 <- sum(abs(y_hat_test_knn3 - Boston_test$crimabvmedian))/length(Boston_test$crimabvmedian)
test_err_knn3


knn.pred4 = knn(Boston_train_knn,Boston_test_knn,Boston_train$crimabvmedian,k=4)
table(knn.pred4,Boston_test$crimabvmedian)
y_hat_test_knn4 <- as.numeric(knn.pred4)-1
test_err_knn4 <- sum(abs(y_hat_test_knn4 - Boston_test$crimabvmedian))/length(Boston_test$crimabvmedian)
test_err_knn4


knn.pred5 = knn(Boston_train_knn,Boston_test_knn,Boston_train$crimabvmedian,k=5)
table(knn.pred5,Boston_test$crimabvmedian)
y_hat_test_knn5 <- as.numeric(knn.pred5)-1
test_err_knn5 <- sum(abs(y_hat_test_knn5 - Boston_test$crimabvmedian))/length(Boston_test$crimabvmedian)
test_err_knn5



