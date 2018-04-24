setwd("/Users/harik/Desktop/STAHW/4")
library(MASS)
Boston
?Boston
dim(Boston)
data(Boston)
##
## a

pairs(Boston,panel=panel.smooth)

##some graphs appear to have a dense plots, which specifies that there may be a relation between them
##for eg: istat vs medv it can be observed they are inversely related. similar for nox vs dis and indus vs dis
##also it can be observed that rm vs medv are directly related. similar for zn vs medv and zn vs rm 

##
## b
model <- lm(crim~., data=Boston)
summary(model)

##  zn, dis,rad,black,medv has a significant relationship to the response.
##  following points can be observed from the graph for crim VS:
##  it is  constant for chas,age and zn
##  it is slightly increasing for indus,nox,rad,tax,ptratio,istat
##  it is slightly decreasing for rm,dis,black,medv

##c
hist(Boston$crim, breaks = 25 , xlab = "Crimerate", ylab="Suburbs",xlim = c(0,50), ylim = c(0,100))
hist(Boston$tax, breaks = 25 , xlab = "Tax rates", ylab="Suburbs")
hist(Boston$ptratio, breaks = 25 , xlab = "Pupil-teacher ratio", ylab="Suburbs")

selectcrime <- subset( Boston, crim > 10)
nrow(selectcrime)/ nrow(Boston)
selectcrime <- subset( Boston, crim > 50)
nrow(selectcrime)/ nrow(Boston)

selecttax <- subset( Boston, tax< 500)
nrow(selecttax)/ nrow(Boston)
selecttax <- subset( Boston, tax> 500)
nrow(selecttax)/ nrow(Boston)

selectpt <- subset( Boston, ptratio> 20)
nrow(selectpt)/ nrow(Boston)

selectpt <- subset( Boston, ptratio< 20)
nrow(selectpt)/ nrow(Boston)

##D
rm_morethan_7 <- subset(Boston,rm>7)
rm_morethan_8 <- subset(Boston,rm>8)
dim(rm_morethan_7)
dim(rm_morethan_8)
summary(rm_morethan_7)
summary(rm_morethan_8)
## There are 64 suburbs with more than 7 rooms per dwelling
## There are 13 suburbs with more than 8 rooms per dwelling