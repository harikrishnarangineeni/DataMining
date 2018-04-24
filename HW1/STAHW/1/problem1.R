setwd("/Users/harik/Desktop/STAHW/1")
library(ISLR)
dim(Auto)

sum(is.na(Auto))

hist(Auto$mpg)

#lmodel<-lm(mpg~.-name,data=Auto)
lmodel<-lm(mpg~.-name,data=Auto)
summary(lmodel)
head(Auto)
library(ggplot2)



# is.na command gives the total not available NA values in the data set.

sum(is.na(Auto))




plot(weight~mpg,data=Auto,groups=origin)

##############################
#Histogram 
#############################

plot(Auto,main="Auto data set Scatterplot")

#Histogram of the Auto data set


# ggplot of auto data set
ggplot(data=Auto)+geom_point(mapping=aes(weight,mpg))




plot(~mpg+cylinders+displacement+horsepower+weight+acceleration+year,data=Auto)

mysum<-summary(lmodel)
summary(lmodel)