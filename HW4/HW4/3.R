rm(list=ls())

#install.packages("data.table")
#install.packages("geneplotter")
#library(data.table)
library(rpart)
library(gbm)
library(randomForest)
library(ISLR)

set.seed(12345)


car_evaluation<-read.csv("C:\\Users\\harik\\Desktop\\STAHW\\car_evaluation.csv")
car_evaluation=car_evaluation[,-c(7)]
dim(car_evaluation)
names(car_evaluation)=c('buying','maint','doors','persons','lug_boot','safety')

head(car_evaluation)

car_evaluation$safety<-as.character(car_evaluation$safety)

for (i in 1:nrow(car_evaluation))
{
  if(car_evaluation$safety[i]=='high')
    car_evaluation$safety[i]=0
  else
    car_evaluation$safety[i]=1
}

car_evaluation$safety<-as.numeric(car_evaluation$safety)
table(car_evaluation$safety)

test_indis<-sample(1:nrow(car_evaluation),.25*nrow(car_evaluation))
test<-car_evaluation[test_indis,]
training<-car_evaluation[-test_indis,]

y_true<-as.numeric(test$safety)

########################################
####     Grow a single tree       #####
########################################

model.control<-rpart.control(minsplit = 5,xval = 10,cp=0)
fit<-rpart(safety~.,data=training,method="class",control = model.control)

x11()
plot(fit)
text(fit,use.n=TRUE,cex=.5)

min_cp=which.min(fit$cptable[,4])
min_cp

x11()
plot(fit$cptable[,4],main="Cp for model selection",ylab="cv error")


### Compute test error for single tree ###

my_pred<-predict(pruned_fit,newdata=test,type="class")
y_hat<-as.numeric(my_pred)-1
misclass_tree<-sum(abs(y_true-y_hat))/length(y_hat)
misclass_tree


#####################################
#####    Random Forests  ######
####################################

rf.fit<-randomForest(safety~.,data=training,ntree=1000)

x11()
varImpPlot(rf.fit)
importance(rf.fit)

y_hat<-predict(rf.fit,newdata=test,type="response")
y_hat<-as.numeric(my_pred)-1
misclass_rf<-sum(abs(y_true-y_hat))/length(y_hat)
misclass_rf


#######################################
     ######    Bagging      #######
#######################################

bag.fit<-randomForest(safety~.,data=training,ntree=1000,mtry=20)

x11()
varImpPlot(bag.fit)
importance(bag.fit)

y_hat<-predict(bag.fit,newdata=test,type="response")
y_hat<-as.numeric(my_pred)-1
misclass_bg<-sum(abs(y_true-y_hat))/length(y_hat)
misclass_bg



######################################
  #####  Boosting    ######
######################################

boost.train<-training;
boost.test<-test;


boost.fit<-gbm(safety~.,data=boost.train,n.trees=1000,shrinkage=.1,interaction.depth = 3,distribution = "adaboost")
boost.fit2<-gbm(safety~.,data=boost.train,n.trees=1000,shrinkage=.6,interaction.depth = 3,distribution = "adaboost")
names(boost.fit)

y_hat<-predict(boost.fit,newdata=boost.test,n.trees=1000,type="response")
misclass_boost.1<-sum(abs(y_hat-y_true))/length(y_true)
misclass_boost.1

y_hat.6<-predict(boost.fit2,newdata=boost.test,n.trees=1000,type="response")
misclass_boost.6<-sum(abs(y_hat.6-y_true))/length(y_true)
misclass_boost.6

##############################
###  logistic regression #####
##############################


logistic<-glm(safety ~ .-safety, data = training,family = "gaussian")
predict_log<-predict(logistic,newdata = test)

y_hat_lm<-predict_log
head(predict_log)

y_hat_lm[y_hat_lm > 0.5] <- 1
y_hat_lm[y_hat_lm < 0.5] <- 0

table(y_hat_lm)

misclass_lm<-sum(abs(y_hat-y_true))/length(y_true)
misclass_lm
