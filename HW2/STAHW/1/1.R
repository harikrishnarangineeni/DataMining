setwd("/Users/harik/Desktop/STAHW/1")
install.packages("ISLR")
library(ISLR)
data(College)
names(College)
dim(College)
##############################################################################
#######################      a        ########################################
##############################################################################

set.seed(20)
train = sample(1:dim(College)[1], dim(College)[1] / 2)
test=-train
c.trainingdata <- College[train,]
c.testingdata <- College[test,]
dim(c.trainingdata)
dim(c.testingdata)


fit <- lm(Apps~.,data=c.trainingdata)
pred<- predict(fit, c.testingdata)


ls_error <- mean((c.testingdata$Apps - pred)^2)
ls_error

##############################################################################
#######################      b        ########################################
##############################################################################

library(glmnet)
data("College")
train.matrixmod = model.matrix(Apps~., data=c.trainingdata)
test.matrixmod = model.matrix(Apps~., data=c.testingdata)


cv.out<- cv.glmnet(train.matrixmod, c.trainingdata$Apps, alpha = 0) #best lambda
names(cv.out)
bestlam <- cv.out$lambda.min
bestlam


ridge.mod = glmnet(train.matrixmod, c.trainingdata$Apps, alpha = 0)
ridge.pred <- predict(ridge.mod, s = bestlam, newx = test.matrixmod)


ridge_error <- mean((c.testingdata[, "Apps"] - ridge.pred)^2)
ridge_error

##############################################################################
#######################      d        ########################################
##############################################################################

cv.out<- cv.glmnet(train.matrixmod, c.trainingdata$Apps, alpha = 1) #best lambda
names(cv.out)
bestlam <- cv.out$lambda.min
bestlam


lasso.mod = glmnet(train.matrixmod, c.trainingdata$Apps, alpha = 1)
lasso.pred <- predict(lasso.mod, s = bestlam, newx = test.matrixmod)


lasso_error <- mean((c.testingdata[, "Apps"] - lasso.pred)^2)
lasso_error


coeff_obs <- predict(lasso.mod, s=bestlam, type="coefficients")
coeff_obs

##############################################################################
#######################      e        ########################################
##############################################################################

library(pls)
pcr.fit = pcr(Apps ~. , data =c.trainingdata , scale = TRUE, validation = "CV")
summary(pcr.fit) #best k value

pcr.pred.test = predict(pcr.fit, c.testingdata, ncomp=10)


pcr_error <- mean((c.testingdata[, "Apps"] - data.frame(pcr.pred.test))^2)
pcr_error

##############################################################################
#######################      f        ########################################
##############################################################################

pls.fit = plsr(Apps ~., data = c.trainingdata, scale = TRUE, validation = "CV")
summary(pls.fit) #best k value 


pls.pred.test = predict(pls.fit, c.testingdata, ncomp=10)


pls_error <- mean((c.testingdata[, "Apps"] - data.frame(pls.pred.test))^2)
pls_error

##############################################################################
#######################      g- write up       ###############################
##############################################################################

