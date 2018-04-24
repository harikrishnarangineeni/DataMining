setwd("/Users/harik/Desktop/STAHW/3")
set.seed(123)
n=1000
p=20
?rnorm


x=matrix(rnorm(n*p),n,p)
beta_coeff<- rnorm(p)
beta_coeff[3] = 0
beta_coeff[4] = 0
beta_coeff[7] = 0
beta_coeff[11] =0
beta_coeff[14] = 0
beta_coeff[17] = 0
epsilon_error <- rnorm(p)

y<- x%*%beta_coeff + epsilon_error

############################################################################################################

##Split data set into training set containing 100 observations and a test set containing 900 observations.##
############################################################################################################

data_model <- data.frame(x,y)
train_data=sample(1:dim(data_model)[1],dim(data_model)[1]/10)

train_data_mod <-data_model[train_data,]
dim(train_data_mod)
names(train_data_mod)
head(train_data_mod)


test_data_mod= data_model[-train_data,]
dim(test_data_mod)

##############################################################################
#######################      Forward selection       #########################
##############################################################################

?regsubsets
library(leaps)
typeof(train_matx)

regfit.forward <- regsubsets(y~., train_data_mod, nvmax = 20,method = "forward")

###referred from the lecture slides###
new_test_model <- cbind(rep(1,length(test_data_mod[,1])),test_data_mod)
colnames(new_test_model) <- c("(Intercept)",colnames(test_data_mod))
head(new_test_model)

new_train_model <- cbind(rep(1,length(train_data_mod[,1])),train_data_mod)
colnames(new_train_model) <- c("(Intercept)",colnames(train_data_mod))
head(new_train_model)


test.error.forward = rep(NA,20)
for(i in 1:20){
  coeffi.forward = coef(regfit.forward,id=i)
  pred_test_forward = as.matrix(new_test_model[,names(coeffi.forward)])%*%coeffi.forward
  test.error.forward[i] = mean((test_data_mod$y - pred_test_forward)^2)
  
}
min(test.error.forward)
which.min(test.error.forward)

train.error.forward = rep(NA,20)
for(i in 1:20){
  coeffi.forward = coef(regfit.forward,id=i)
  pred_train_forward = as.matrix(new_train_model[,names(coeffi.forward)])%*%coeffi.forward
  train.error.forward[i] = mean((train_data_mod$y - pred_train_forward)^2)
  
}
min(train.error.forward)
which.min(train.error.forward)
library(ggplot2)
df <- data.frame(test.error.forward,train.error.forward) 
names(df)=c("testerror","trainerror")
df$n=rownames(df)
df$n =factor(df$n,levels=df$n)
ggplot(df, aes(n,group=1))+geom_line(aes(y=testerror,color="test"))+geom_line(aes(y=trainerror,color="train"))

##############################################################################
#######################      Backward selection        #######################
##############################################################################

regfit.backward <- regsubsets(y~., train_data_mod, nvmax = 20,method = "backward")

new_test_model <- cbind(rep(1,length(test_data_mod[,1])),test_data_mod)
colnames(new_test_model) <- c("(Intercept)",colnames(test_data_mod))
head(new_test_model)

new_train_model <- cbind(rep(1,length(train_data_mod[,1])),train_data_mod)
colnames(new_train_model) <- c("(Intercept)",colnames(train_data_mod))
head(new_train_model)

test.error.backward = rep(NA,20)
for(i in 1:20){
  coeffi.backward = coef(regfit.backward,id=i)
  pred_test_backward = as.matrix(new_test_model[,names(coeffi.backward)])%*%coeffi.backward
  test.error.backward[i] = mean((test_data_mod$y - pred_test_backward)^2)
  
}
min(test.error.backward)
which.min(test.error.backward)

train.error.backward = rep(NA,20)
for(i in 1:20){
  coeffi.backward = coef(regfit.backward,id=i)
  pred_train_backward = as.matrix(new_train_model[,names(coeffi.backward)])%*%coeffi.backward
  train.error.backward[i] = mean((train_data_mod$y - pred_train_backward)^2)
  
}

min(train.error.backward)
which.min(train.error.backward)

df1 <- data.frame(test.error.backward,train.error.backward) 
names(df1)=c("testerror","trainerror")
df1$n1=rownames(df1)
df1$n1 =factor(df1$n1,levels=df1$n1)
ggplot(df1, aes(n1,group=1))+geom_line(aes(y=testerror,color="test"))+geom_line(aes(y=trainerror,color="train"))

###coefficient values
coef(regfit.forward, id=14)
coef(regfit.backward, id=14)
