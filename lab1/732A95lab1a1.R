########### 732A95 #############
####### Computer lab 1 #########


# Asignment 1 

## 1. Import and divide data


setwd("Documents/732A95/lab1")

data <- read.csv2("spambase.csv",sep = ",", header = TRUE, stringsAsFactors = FALSE) # Reading data 

set.seed(12345)

# Dividing data in to a test and traning data set
n     <- dim(data)[1]    
id    <- sample( 1:n, floor(n*0.5) )
train <- data[id,]
test  <- data[-id,]


## 2. K-nearest function 

knearest <- function(data, K, newdata){

train <- data  
test <- newdata 

# i Compute xhat
xhat <- as.matrix( data[,-length(data)] ) 
xhat <- xhat / sqrt(rowSums( xhat^2 )) 

# ii Compute yhat
yhat <- newdata[,-length(newdata)]
yhat <- yhat / sqrt(rowSums(yhat^2))

# iii Compute matrix C as abs(c(Xi,Ymj))
C <-  xhat %*% t(yhat)  

# iv Compute the distance

D <- 1 - C 


### Hit är allt glaskrat. 


myOrders <- apply(D,MARGIN = 2, FUN = order) #Returns orders for each column (the lowest value is the fist value)

myOrders <- matrix(myOrders[1:K,], nrow=K)  #Keeps the K lowest values in the distance matrix
myData<-train[myOrders[1:K,],length(train)]  #Extracts the K number of values of the observed y-variables
myData<- matrix(myData,nrow=K,byrow = FALSE)  #puts the y-observations in a matrix where each column represents a column

myClass<-apply(myData,MARGIN = 2, FUN =function(X) round(mean(X)))  #Majority voting 
myPredict<-apply(myData,MARGIN = 2, FUN =function(X) mean(X))  #Generating predictions for 1.6 


#missclassification rates for training and test 
mcrTest<- 1 - mean(myClass == test[,length(test)])  
mcrTrain <- 1 - mean(myClass == train[,length(train)])  
mcr<-c(Test = mcrTest, Train = mcrTrain)

#Confusion-matrix for the training and test dataset
minRejm           <- data.frame(myClass = myClass) 
minRejm$myTest    <- test[,length(test)]
minRejm$myTrain   <- train[,length(train)]
cmTest <- table(myClass = minRejm$myClass,myTest  = minRejm$myTest) 
cmTrain<- table(myClass = minRejm$myClass,myTrain = minRejm$myTrain) 


returnlist<-list(D = D, myClass = myClass, cmTest = cmTest,cmTrain = cmTrain,mcr=mcr,myPredict=myPredict) 
message(cat("Returning a list with answers, The no. of K used were: ",K)) 
return(returnlist)


}  

##################### END OF FUNCTION 


mytest<-knearest(train, K = 5, test)



mytest$cmTrain
mytest$confMat
mytest$myClass
mytest$mcr
mytest[,1]


## 3.

## 4. 

## 5. 

install.packages("kknn")
library(kknn)

#data<-train
#newdata<-test

kknnearest <- kknn(Spam~., train, test, distance = 1, k=5,  kernel = "rectangular")

kknnearest
  



testKNN <- function(KKNpred,pi){
  a       <- data.frame(pred = as.numeric( KKNpred > pi ))
  a$test  <- test[,length(test)]
  return(table(a))
}

testKNN(predict(kknnearest),0.5)

trainKNN <- function(KKNpred,pi){
  a       <- data.frame(pred = as.numeric( KKNpred > pi ))
  a$test  <- test[,length(test)]
  return(table(a))
}

testKNN(predict(kknnearest),0.5)


## 6. 

rocKalk <- function(predictions,pi = 0.5){
  a       <- data.frame(pred = as.numeric( predictions > pi ))
  a$test  <- test[,length(test)]
  confmat<-table(a)
  TPR<-sum(confmat[2,2])/sum(confmat[,2])
  FPR<-1 - sum(confmat[1,1])/sum(confmat[,1])
  return(data.frame(TPR = TPR, FPR = FPR))
}

 
rocKalk(mytest$myPredict)
##### massa jävla forloopar 
mypi<-seq(0.05,0.95,0.05)
i <- 1
rocMyFunk <- data.frame(matrix(ncol= 2 ,nrow = length(mypi)))
for (n in mypi){ 
rocMyFunk[i,1]<-rocKalk(predictions = mytest$myPredict, pi=n)[[1]] 
rocMyFunk[i,2]<-rocKalk(predictions = mytest$myPredict, pi=n)[[2]] 
i <- i + 1  

  }        
rocMyFunk$X1<- as.numeric(rocMyFunk$X1)
rocMyFunk$X2<- as.numeric(rocMyFunk$X2)
colnames(rocMyFunk) <- c("TPR","FPR")


mypi<-seq(0.05,0.95,0.05)
i <- 1
rocKKNN <- data.frame(matrix(ncol= 2 ,nrow = length(mypi)))
for (n in mypi){ 
  rocKKNN[i,1]<-rocKalk(predictions = predict(kknnearest), pi=n)[[1]] 
  rocKKNN[i,2]<-rocKalk(predictions = predict(kknnearest), pi=n)[[2]] 
  i <- i + 1  
  
}        
rocKKNN$X1<- as.numeric(rocKKNN$X1)
rocKKNN$X2<- as.numeric(rocKKNN$X2)
colnames(rocKKNN) <- c("TPR","FPR")





#rocMyFunk<-data.frame(matrix(as.numeric(unlist(sapply(mypi, FUN=rocKalk,predictions = mytest$myPredict))),ncol=2))

#rocKKNN<-data.frame(matrix(as.numeric(unlist(sapply(mypi, FUN=rocKalk,predictions =  predict(kknnearest)))),ncol=2))
#colnames(rocKKNN) <- c("TPR","FPR")




library(ggplot2)


ggplot() + geom_line(data=rocMyFunk,aes(x = FPR, y = TPR,col="knearest")) + geom_line(data=rocKKNN,aes(x = FPR, y = TPR,col="KKNN"))


rocMyFunk<-data.frame(matrix(unlist(sapply(mypi, FUN=rocKalk,predictions = mytest$myPredict)),ncol=2))
colnames(rocMyFunk) <- c("TPR","FPR")
