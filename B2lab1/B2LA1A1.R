#############################
####### Computer Lab 1 ######
######### Block 2 ###########
########  732A95  ###########


setwd("/Users/EmilsHem/Documents/732A95/B2lab1")
cube<-read.csv2("cube.csv")

# Assignment 1 

## 1.1 

myspline <- function(Y, X, knots){

#pmax returns all values that are over the value specified in the vector supplied
#all other values the specified break point is returned. so for example in a vector
#with values -10:10 and 5 specified all numbers = and below 5 it will return 5 for
#the other values it will return their specific values. 
#pmax(-10:10,5)

#So when the book specifies h3(X) = (X - \xi_1)+ it basicly says for all values that are 
#positive when the calculation X- \xi_1 is preformed should be kept as their original value
#all other are equal 0, therefor pmax(X - knots[1], 0) will return the correct values. 

#H_1(X) = 1 is added in the model.matrix and is not neccesary. 
#H_2 = X 
#H_3:length(knots) = h3(X) = (X - \xi_i)


H <- cbind(X,sapply(knots, FUN = function(k) pmax(X-k,0)) )
colnames(H) <- c("X",paste0("H",1:length(knots)))
myLM <- lm(Y ~ H)
myPredictedData<-data.frame(cbind(Y,X, predict(myLM)))
colnames(myPredictedData) <- c("Y","X","Predictions")
library(ggplot2)           

p<-ggplot(data = myPredictedData) + geom_point(aes(x = X, y = Y)) + geom_point(aes(x = X, y = Predictions),color = "red")

plot(p)
return(myLM)
}
 

## 1.2     
a<-myspline(Y = cube$y, X = cube$x, knots = c(2,5))
summary(a)

## 1.3 
smoothSpline <- smooth.spline(y = cube$y, x = cube$x)

SSpline<-data.frame(cbind(cube$y,cube$x, fitted(smoothSpline)))
colnames(SSpline) <- c("Y","X","Predictions")

ggplot(data = SSpline) + geom_point(aes(x = X, y = Y)) + geom_line(aes(x = X, y = Predictions),color = "red")

#Hur specificerar man specifika knots.

# Assignment 2




