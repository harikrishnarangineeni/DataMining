setwd("/Users/harik/Desktop/STAHW/2")
library(ISLR)
?Auto
large_model <- lm(mpg~., data=Auto)
summary(large_model)

## a
significant_model <- lm(mpg ~ . -name, data = Auto)
summary(significant_model)
## from the summary of the significant_model:
## displacement, weight, year, origin appear to have a significant relationship to the response.


## b
## coefficient variable for "year" suggest that it is directly proportional to mpg, since it is positive(+0.750773)
## which also states that mpg value increases with each year that passes


## C
## different interactions using * and :
model1 <- lm(mpg ~ .- name+displacement*weight+displacement*year+displacement*origin+displacement:acceleration+displacement:horsepower+displacement:cylinders,data=Auto)
summary(model1)

model2 <- lm(mpg ~ .- name+weight*year+weight*origin+weight:acceleration+weight:horsepower+weight:displacement+weight:cylinders,data=Auto)
summary(model2)

model3 <- lm(mpg ~ .- name+year*origin+year:acceleration+year:weight+year:horsepower+year:displacement+year:cylinders,data=Auto)
summary(model3)

model4 <- lm(mpg ~. -name+displacement:weight+displacement:year+displacement:origin+ weight:year+ weight:origin+ year:origin, data=Auto)
summary(model4)
###
###from the summary of all the four models, it is observed that model4 has significant values for the coefficients
