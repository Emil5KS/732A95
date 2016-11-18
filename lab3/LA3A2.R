################################################
############## Computer Lab 3 ##################
# # # # Introduction to Machine Learning # # # #
#############  Assignment 2   ##################
# # # # #####                 ########## # # # #

## 2.1 

CS <- read.csv2("lab3/creditscoring.csv")


#Suffle the rows
set.seed(12345)
CS <- CS[sample(nrow(CS)),]

csTrain <- CS[1:(nrow(CS)*0.50),]
csValid <- CS[((nrow(CS)*0.50)+1):floor(nrow(CS)*0.75),]
csTest  <- CS[((nrow(CS)*0.75)+1):nrow(CS), ]

## 2.2

install.packages("tree")
install.packages("partykit")

library(tree)
library(partykit)

#For the deviance 

tDev <- tree(good_bad~ ., data = csTrain, split = "deviance")

predVals<-predict(tDev,newdata = csTrain) 
trainTable<-table(predicted = ifelse(predVals[,1] > predVals[,2], "bad","good"), Train= csTrain$good_bad)
sum(diag(trainTable))/nrow(csTrain)

predValsT<-predict(tDev,newdata = csTest) 
testTable<-table(predicted = ifelse(predValsT[,1] > predValsT[,2], "bad","good"), Test= csTest$good_bad)
sum(diag(testTable))/nrow(csTest)



#For the Gini

tGin <- tree(good_bad~ ., data = csTrain, split = "gini")
summary(tGin)

predValsGin<-predict(tGin,newdata = csTrain) 
giniTab<-table(Predicted =ifelse(predValsGin[,1] > predValsGin[,2], "bad","good"),Test=csTrain$good_bad)
sum(diag(giniTab))/nrow(csTrain)


predValsGinT<-predict(tGin,newdata = csTest) 
giniTabT<-table(Predicted=ifelse(predValsGinT[,1] > predValsGinT[,2], "bad","good"),Test=csTest$good_bad)
sum(diag(giniTabT))/nrow(csTest)


## Deviance seems to be the better one.

## 2.3 



valid23 <- data.frame(trainS=1,testS = 1)
tDev2 <- tree(good_bad~ ., data = csTrain, split = "deviance")
rad <- 1 
  for (i in 2:15) {  
    tDev22         <-      prune.tree(tDev2, best = i)
    valid23[rad,1]<-      deviance(tDev22)
    valid23[rad,2]<-      deviance(predict(tDev22,newdata = csValid,type = "tree"))
    rad <- rad + 1 
}

valid23$best <- 2:15
ggplot(data = valid23,aes(x=best)) + geom_line(aes(y=trainS),col="red") + geom_line(aes(y=testS),col = "blue")

## Borde vara best = 4 som är bäst? 
tBest<-prune.tree(tDev2, best = 4)
summary(tBest)
plot(tBest)

predBest<-predict(tBest,newdata = csTrain) 
tTable<-table(predicted = ifelse(predBest[,1] > predBest[,2], "bad","good"), Train= csTrain$good_bad)
sum(diag(tTable))/nrow(csTrain)

predBT<-predict(tBest,newdata = csTest) 
tBTable<-table(predicted = ifelse(predBT[,1] > predBT[,2], "bad","good"), Test= csTest$good_bad)
sum(diag(tBTable))/nrow(csTest)


## 2.4

install.packages("e1071")
library(e1071)
??e1071

baybay<- naiveBayes(formula = good_bad~., data=csTrain)
bBay<- table(Predicted = predict(baybay,newdata = csTrain), Observed = csTrain$good_bad)
sum(diag(bBay)) / nrow(csTrain)

bBayTest<- table(Predicted = predict(baybay,newdata = csTest), Observed = csTest$good_bad)
sum(diag(bBayTest)) / nrow(csTest)

## 2.5
rawprobs<-predict(baybay,csTrain,type = "raw")

rawBayes<- rawprobs[,2]/rawprobs[,1] #good/bad

lossMat<-matrix(c(0,10,1,0),ncol=2)

bBay
table(Predicted=ifelse(rawBayes > lossMat[2,1]/lossMat[1,2],"Good","Bad" ),Observed=csTrain$good_bad)
#table(Predicted=ifelse(rawBayes > lossMat[1,2]/lossMat[2,1],"Good","Bad" ),Observed=csTrain$good_bad)




