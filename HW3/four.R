rm(list=ls())

set.seed(1234)
x=rnorm(100)
y=x-2*x^2+rnorm(100)


library(boot)
simulated_data = data.frame(x,y)
set.seed(123)

## first degree polynomial ##

glm.fit.1 = glm(y~x)
cv.glm(simulated_data, glm.fit.1)$delta[1]


## second degree polynomail ##

glm.fit.2 = glm(y~poly(x,2))
cv.glm(simulated_data, glm.fit.2)$delta[1]

## third degree polynomial ##

glm.fit.3 = glm(y~poly(x,3))
cv.glm(simulated_data, glm.fit.3)$delta[1]

##  fourth degree polynomial ##

glm.fit.4 = glm(y~poly(x,4))
cv.glm(simulated_data, glm.fit.4)$delta[1]

## The quadratic polynomial(second degree) had the lowest LOOCV test error rate. 
## And yes I expected that because the true data is of quatratic form..

##  statistical significance ## 

summary(glm.fit.4)
summary(glm.fit.2)


## If we look at the quadratic fit we can see that both the quatratic term and the linear term are significant.
## This is what we expect

