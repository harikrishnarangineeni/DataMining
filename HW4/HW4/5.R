
#install.packages("neuralnet")
library(ElemStatLearn)
library(neuralnet)

####################
## load spam data ##
####################

data(spam)
spam$spam <- ifelse(spam$spam == "spam",1,0)
spam<- spam[1:4600,]

################################################
## cv error ##
################################################

n<- names(spam)
formulae <- as.formula(paste("spam ~", paste(n[!n %in% "spam"], collapse = " + ")))
crossvalidate <- function(spam,hidden_1=c(5))
{   
  error_cv <- NULL
  k <- 7 # any random value for splitting  
  for(j in 1:k)
  {
    ### splitting data into train and test set ###
    
    
    data.sample <- sample(1:nrow(spam),round(nrow(spam)/2))
    train.data<- spam[data.sample,]
    test.data <- spam[-data.sample,]
    
   
    ## neural network fit ##
    
    
    network <- neuralnet(formulae,data=train.data,hidden=hidden_1,err.fct='ce', linear.output=FALSE,threshold=0.5)
    predict.network <- compute(network,test.data[,1:57])
    
    
    ###### specifying and predict the class #####
   
    
    class <- train.data$spam
    predict <- round(predict.network$net.result)
    
   
    ## the cv error can be defined as follows  ##

    error_cv[j] <- sum((class - predict)/nrow(predict))
    
  }
  return(mean(error_cv))
}

##################################################################
## selecting the hidden neural networks and defining vectors ###
##################################################################

error_train <- NULL
error_test <- NULL
set.seed(100)
for(i in 1:5)
  
###########################################################################
###  fitting the neural network and compute the error with the help of CV ####
############################################################################

{
  network<- neuralnet(formulae,data=spam,hidden=c(i),err.fct='ce', linear.output=FALSE,
                      threshold=0.5)
  error_train[i] <- sum(((round(network$net.result[[1]])-(spam$spam))^2)/nrow(spam))
  error_test[i] <- crossvalidate(spam,hidden=c(i))    
}

##################################################
### finding the error values and plotting them  ##
##################################################

error_train
error_test
plot(error_train,main='Mean vs hidden neurons',xlab="Hidden neurons",ylab='error MSE of train',type='14',col='green',lwd=2)
plot(error_test,main='Mean vs hidden neurons',xlab="Hidden neurons",ylab='error MSE of test',type='l4',col='green',lwd=2)


#########################################
## compute the number of neural network ##
#########################################

which(min(error_train) == error_train)


## Now for the 5 neural networks let us perform ANN with the train and test set split ##

data.sample <- sample(1:nrow(spam),round(nrow(spam)/2))
train.data<- spam[data.sample,]
test.data <- spam[-data.sample,]     
network <- neuralnet(formulae,data=train.data,hidden=5,err.fct='ce', linear.output=FALSE,threshold=0.5)

#####################################
## predict the error value for ANN ###
#####################################

network.compute <- compute(network,test.data[,1:57])
true <- test.data$spam
predi<- round(network.compute$net.result)
error_ann <- abs(sum(true-predi))/length(predi) 
error_ann
table(true,predi)


##################################
## comparison with additive model ##
##################################

install.packages("gam")
library(gam)
model.additive <- gam(formulae,data=train.data,family=binomial)
summary(model.additive)
predict <- predict(model.additive,newdata=test.data)
true_1 <- test.data$spam
pred_1 <- round(predict)
error <- abs(sum(true_1-pred_1))/length(pred_1) 
error 
table(true_1,pred_1)

