setwd("/Users/harik/Desktop/STAHW/2")
rm(list = ls())
traindata <- read.table("ticdata2000.txt")
testdata <- read.table("ticeval2000.txt")

traindata
testdata
names(traindata)

##############################################################################
#######################     OLS        #######################################
##############################################################################

fit<- lm(V86~.,traindata)
predictions = predict(fit, testdata[,1:85])
predictions ##observe the values of predictions

## defining a class to decide the predictions between 0 and 1 classifier
class_decider = function(value){
  if(value < 0){
    return(0)
  }else{
    distance_to_0 = value
    distance_to_1 = 1-value
    if(distance_to_0 < distance_to_1){
      return(0)
    }else{
      return(1)
    }
  }
}

for(i in 1:length(predictions)){
  predictions[i] = class_decider(predictions[i])
}
table(predictions)


Y_target = read.table("tictgts2000.txt")
table(Y_target$V1)


ols_error <- sum((Y_target$V1 - predictions)^2)/length(Y_target$V1)
ols_error

##############################################################################
#######################      Forward selection       #########################
##############################################################################

library(leaps)
regfit.fwd <- regsubsets(V86~.,data=traindata, nvmax = 85, method = "forward")
sum.insu.fwd<- summary(regfit.fwd)
sum.insu
sum.insu$outmat


new_test <- cbind(rep(1,length(testdata[,1])),testdata)
colnames(new_test) <- c("(Intercept)",colnames(testdata))
coef(regfit.fwd,id=85)
head(new_test)


test.error.fwd = rep(NA,85)
for(i in 1:85){
  coeffi.fwd = coef(regfit.fwd,id=i)
  pred_test_fwd = as.matrix(new_test[,names(coeffi.fwd)])%*%coeffi.fwd
  test.error.fwd[i] = (1/length(Y_target$V1))*sum((Y_target$V1 - pred_test_fwd)^2)

}


min(test.error.fwd)
which(sum.insu.fwd$cp == min(sum.insu.fwd$cp))


##############################################################################
#######################      Backward selection        #######################
##############################################################################

regfit.bwd <- regsubsets(V86~.,data=traindata, nvmax = 85, method = "backward")
sum.insu.bwd<- summary(regfit.bwd)


test.error.bwd = rep(NA,85)
for(i in 1:85){
  coeffi.bwd = coef(regfit.bwd,id=i)
  pred_test_bwd = as.matrix(new_test[,names(coeffi.bwd)])%*%coeffi.bwd
  test.error.bwd[i] = (1/length(Y_target$V1))*sum((Y_target$V1 - pred_test_bwd)^2)
  
}


min(test.error.bwd)
which(sum.insu.bwd$cp == min(sum.insu.bwd$cp))

##############################################################################
#######################     LASSO        #####################################
##############################################################################


train.mat.mod<- as.matrix(traindata)
test.mat.mod<- as.matrix(testdata)

cv.out.lasso<- cv.glmnet(train.mat.mod[,1:85], traindata$V86, alpha = 1)
names(cv.out.lasso)
bestlam.lasso <- cv.out.lasso$lambda.min
bestlam.lasso


lasso.mod.insu = glmnet(train.mat.mod[,1:85], traindata$V86, alpha = 1)
lasso.pred.insu <- predict(lasso.mod.insu, s = bestlam.lasso, newx = test.mat.mod)

for(i in 1:length(lasso.pred.insu)){
  lasso.pred.insu[i] = class_decider(lasso.pred.insu[i])
}


lasso_error_insu <- mean((Y_target$V1 - lasso.pred.insu)^2)
lasso_error_insu

##############################################################################
#######################      RIDGE        ####################################
##############################################################################

cv.out.ridge<- cv.glmnet(train.mat.mod[,1:85], traindata$V86, alpha = 0)
plot(cv.out.ridge)
names(cv.out.ridge)
bestlam.ridge <- cv.out.ridge$lambda.min
bestlam.ridge

ridge.mod.insu = glmnet(train.mat.mod[,1:85], traindata$V86, alpha = 0)
ridge.pred.insu <- predict(ridge.mod.insu, s = bestlam.ridge, newx = test.mat.mod)

for(i in 1:length(ridge.pred.insu)){
  ridge.pred.insu[i] = class_decider(ridge.pred.insu[i])
}

ridge_error_insu <- mean((Y_target$V1 - ridge.pred.insu)^2)
ridge_error_insu
