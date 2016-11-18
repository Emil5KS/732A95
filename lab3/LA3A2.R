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








