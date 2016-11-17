#####################
#####################
#######Computer lab 2 
#######732A95
#######Assignment 2
#####################

## 2.1 
setwd("/Users/EmilsHem/Documents/732A95/lab2")
TC<- read.csv2("tecator.csv")

ggplot(data = TC, aes(x=Protein,Moisture)) + geom_point()

## 2.2

# 3.1.1 Maximum likelihood


## 2.3

set.seed(12345)
obs   <- dim(TC)[1]
id    <- sample(1:obs, floor(obs*0.5))
train <- TC[id,]
valid <- TC[-id,]


  dataMSE <- data.frame(trainMSE = 1, testMSE = 1, noPoly = 1)
  i <-1 
  
for (modell in 1:6) {  

  linj<-lm(formula(paste("Moisture ~",paste("I(Protein^",1:modell,")", collapse = " + "))),data = train)
  trainMSE<-mean(linj$residuals^2)  
  testMSE<-mean( (predict(linj,newdata = valid) - valid$Moisture)^2)
  dataMSE[i,] <- c(trainMSE,testMSE,i)
  i <- i + 1
  
} 
  
 aplot<- ggplot(data = dataMSE) + geom_line(aes(x = noPoly ,y = trainMSE, col = "Train")) +
    geom_line(aes(x = noPoly ,y = testMSE, col = "Test")) +
    labs(title="MSE for different polynomial functions",color ="MSE",
         x="Flexibility", y = "MSE")
  plot(aplot)
  
  geom_lin
#######
  
  
#######
  
  
## 2.4 
  
library(MASS)

varSelect<-lm(Fat ~ . , data = TC[,2:102])
stepRes<-stepAIC(varSelect,trace = FALSE)
length(stepRes$coefficients) # It removed 36

## 2.5

install.packages("glmnet")
library(glmnet)
library(reshape2) 
wideWeg <- glmnet(x = as.matrix(TC[,2:101]), y = TC[,102], alpha = 0)
mycoeffs <- coefficients(wideWeg)[-1,]
colnames(mycoeffs) <- wideWeg$lambda

myPlot<-melt(as.matrix(mycoeffs),id = rownames,c("lambda","coef"))

colnames(myPlot) <- c("features","lambda","coef")


ggplot(myPlot,aes(x=log(lambda),y = coef,color = features)) + geom_line(show.legend = FALSE)
ggplot(myPlot,aes(x=lambda,y = coef,color = features)) + geom_line(show.legend = FALSE)

## 2.6

lassoReg <- glmnet(x = as.matrix(TC[,2:101]), y = TC[,102], alpha = 1)
mycoeffs <- coefficients(lassoReg)[-1,]
colnames(mycoeffs) <- lassoReg$lambda

myLasso<-melt(as.matrix(mycoeffs),id = rownames,c("lambda","coef"))

colnames(myLasso) <- c("features","lambda","coef")


ggplot(myLasso,aes(x=log(lambda),y = coef,color = features)) +
  geom_line(show.legend = FALSE) + labs(x = expression(log(lambda)) )

ggplot(myLasso,aes(x=lambda,y = coef,color = features)) + 
  geom_line(show.legend = FALSE) 


## 2.7

cvLASSO<-cv.glmnet(x = as.matrix(TC[,2:101]), y = TC[,102], alpha = 1)

nocoeff<-matrix(coef(cvLASSO))
nocoeff[nocoeff != 0] 
sum(nocoeff!=0) - 1
length(nocoeff[nocoeff != 0]) - 1 #antalet coeff exklusive intercept.
plotLasso<- data.frame(CV=cvLASSO$cvm,lambda= cvLASSO$lambda)

ggplot(data=plotLasso,aes(x = log(lambda), y = CV)) + geom_point()
