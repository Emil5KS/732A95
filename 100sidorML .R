########### 732A95 #############

###############################
####### Computer lab 1 ########
###############################
# Asignment 1 

## 1. Import and divide data
data <- read.csv2("lab1/spambase.csv",sep = ",", header = TRUE, stringsAsFactors = FALSE) # Reading data 

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
  
  #Returns orders for each column (the lowest value is the fist value) 
  #Mao får vi fram villka som är de närmsta grannarna
  myOrders <- apply(D,MARGIN = 2, FUN = order) 
  
  #Keeps the K lowest values in the distance matrix
  #Vi plockar bara de K första grannarna
  myOrders <- matrix(myOrders[1:K,], nrow=K) 
  
  #Extracts the K number of values of the observed y-variables
  #Plockar fram vad de observerade y-värdena är
  myData<-train[myOrders[1:K,],length(train)] 
  
  #puts the y-observations in a matrix where each column represents a column
  #Smetar in dem i en matris
  myData<- matrix(myData,nrow=K,byrow = FALSE)  
  
  #Majority voting
  # tar genomsnittet av de K närmsta grannarna ( 1or och 0or) så vi får en kvot.
  # om kvoten är > än 0.5 blir det en 1 och 0 om den är under 0.5. 
  myClass<-apply(myData,MARGIN = 2, FUN =function(X) round(mean(X))) 
  
  #Generating predictions for 1.6
  #Tar med sannolikheterna också, för ngn av uppgifterna
  myPredict<-apply(myData,MARGIN = 2, FUN =function(X) mean(X))   
  
  
  #missclassification rates for training and test
  # TRUE och FALSE motsvarar 1 och 0 så snittet av alla som stämmer
  #överense kommer då bli mcr.
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

kknnearest <- kknn(formula = Spam~., train = train, test = test, 
                   distance = 1, k=5,  kernel = "rectangular")
#Bestämmer vilken form kernel-området ska ha.
#distance = 1 är när en kvadratisk form runt mittpunkten, 2 är en cirkel
# Vilken kernel som ska användas i kknn. epanechnikov var på tentan vi fick.
kknnearest


# Det här nedan är till för när man vill använda någon annan beslutsregel än 0.5. 
# pi är mao beslutsgränsen
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

### Lab2 

setwd("/Users/EmilsHem/Documents/732A95/lab1")

machines <- data.frame(read.csv2("machines.csv"))
machines<-as.vector(unlist(machines$Length))

# maximum-likelihood av normalfördelningen.
explog <-function(theta,data){
  return(     length(data)*log(theta,base = exp(1)) - (theta*sum(data))    )
  
}

#explog2 <-function(theta){
# 48*log(theta,base = exp(1)) - theta*sum(machines)
#}

#explog3 <-function(theta){
# 48*log(theta,base = exp(1)) - theta*sum(machines[1:6])
#}

## Sapply försöket med explog2
theta <- seq(0.001,10,by=0.01)
thetaML <- sapply( X = theta, FUN = explog, data = machines)

plot(x=theta, y = thetaML, type = "l")

library(ggplot2)
plotData <- data.frame(x=theta,y=thetaML)
p <- ggplot() + geom_line(data=plotData,aes(x=x,y=y,col="All observations")) +
  labs(title="ML-estimation of exponential distrubution",
       x=expression(theta),y="ML-estimate")
plot(p)

a <- p + geom_segment(aes(x = 3, y = -20, xend = theta[thetaML == max(thetaML)], yend = max(thetaML)),
                      arrow = arrow(length = unit(0.5, "cm"))) + 
  annotate("text",x=4,y=-19,label = paste("theta",theta[thetaML == max(thetaML)]))

1/max(thetaML) # Answer to the 2.2
1/sum(machines)

# 2.3.

thetaML3 <- sapply( X = theta, FUN = explog, data = machines[1:6])

plotData$y2 <- thetaML3

a + geom_line(data = plotData, aes(x=x,y=y2,col="6 observations"))
# it's dependent of data, although not to much data as 48 observation was close.

### 2.4

#May god have mercy on us all.

# Posteriorn som jag beräknat ser ut som den gör i blocket
# Eventuellt så behöver log(lambda) (lambda = 10) tas bort då det
# inte beror på theta ( och proportionalliteten för posteriorn 
# tar inte hänsyn till det)

postBayes <- function(data,theta){ 
  lambda <- 10 
  return(length(data) * log(theta) - theta * sum(data) + log(lambda) - (lambda*theta))
  
} 

thetaBayes <- sapply(theta,FUN = postBayes,data = machines)

plot(x=theta,y=thetaBayes,type = "l")

plotDataBayes <- data.frame(x=theta,y=thetaBayes)
B <- ggplot() + geom_line(data=plotDataBayes,aes(x=x,y=y)) +
  labs(title="Bayes-estimation of exponential distrubution",
       x=expression(theta),y="Bayes-estimate")
plot(B)


### 2.5 


maxTheta <- theta[thetaML == max(thetaML)]

runif
explog(theta = maxTheta,data=c(1))
set.seed(12345)

randomExpData<- data.frame(x=rexp(50,rate = maxTheta))
ggplot(data = randomExpData,aes(x=x)) + geom_histogram(bins = 10)
ggplot(data =data.frame(x=machines),aes(x=x)) + geom_histogram(bins = 10)

hist(machines)
hist(randomExpData$x) 
############ For-loop försöket

# sum(as.vector(explog(0.01,machines)))/48

thetas<-c() 
means <-c()
i <- 1 
for (theta in seq(0.001,3,by=0.01)){  
  mymind   <- explog(theta, data = machines)
  thetas[i]<- theta
  means[i] <- sum(mymind)/48
  i <- i + 1 
} 

means <- means[-1]
thetas<- thetas[-1]
plot(x=thetas, y = means, type = "l")

###############################



################################
#######  Computer lab 2  ####### 
################################

## Assignment 1
# 1.1 

myLM <- function(Y, X, Nfolds){
  library(ggplot2)
  # DEFINING THE FUNCTION LINREG, FOR FITTING LINEAR MODELS 
  
  linreg<-function(formula,data){
    formula <- formula(formula)
    des.mat <- model.matrix(formula , data) #Extracts the model matrix
    dep.var <- all.vars(formula)[1]         #Extracts the name of the y-variable 
    dep.var <- as.matrix(data[dep.var])     #Extracts the data of the y-variable 
    # and overwrites it with the data-colum 
    
    #Calculating the beta coeffs. (X' %*% X)^-1 %*% X' %*% y
    beta.hat <- solve( t(des.mat) %*% des.mat )  %*% t(des.mat) %*% dep.var
    
    # Calculating the y-hat  , y_hat = X %*% beta_hat
    y.hat <- des.mat %*% beta.hat  
    
    #Calculating the residuals e= y- y_hat
    res.err <- dep.var - y.hat   
    
    l<-list( beta.hat = beta.hat, y.hat = y.hat, res.err = res.err)
    return(l)
  }
  
  
  #GENERATING ALL POSSIBLE PERMUTATIONS OF MODELS
  
  #Get the colnames for the X-variables
  q<-rep(paste0("X",c(1:5)))
  
  #Merge the data in to one data set and naming the columns
  myData <- cbind(Y,X) 
  colnames(myData)<- c("Y",q)
  
  #Generating all possible combinations 
  myComb<-sapply(c(1:5), FUN = combn, x = q )
  #Creating the vector that will hold all formulas 
  myformula <- c(myComb[[1]])
  
  #Extracting the combinations of 2 and 3 X-variables and adding a + between them
  for (i in 2:3){
    for (j in 1:10){
      myformula[length(myformula)+1]<-(paste(myComb[[i]][,j],collapse = " + "))
    }
  }
  
  #Heres a row that could replace the above for-loop
  #sapply(2:3, FUN = function(i) sapply(1:10, FUN = function(j) paste(myComb[[i]][,j], collapse = " + " ) ) )
  
  
  #Extracting the combinations of 4 and 5 X-variables and adding a + between them
  #This is basicly a loop for the 4 combinations 
  myformula <-c(myformula ,
                sapply(1:5, FUN =function(X)
                  paste(myComb[[4]][,X],collapse = " + " ) 
                ),  paste(myComb[[5]],collapse = " + ")  
  )
  
  myformula<- paste("Y","~",myformula)
  
  #### SPLITTING AND SUBSETING DATA IN TO K FOLDS
  
  #calculatin no. rows
  noobs<-dim(myData)[1]
  K <- Nfolds
  
  #Use sample to randomly draw the indexes of the dataset
  #and reorder the data with them in a random manner
  set.seed(12345)
  myData<-myData[sample(noobs),] 
  
  #Create K equal indexes that are added to the data.
  
  myData$index <- cut(1:noobs,breaks=K,labels = FALSE)
  
  
  #init a counting vector "o" useed to loop in to the data.frame "linearModels"
  #and a combination index "dataKombs" used for subseting the different datasets
  #used for fitting models
  o <- 1 
  linearModels<-data.frame(CV=1,model="text",nofeats=1,stringsAsFactors = FALSE)
  dataKombs <- combn(1:K,K-1)
  for (m in 1:length(myformula)){   
    for (l in (1:K)){  
      
      #the data of the K-folds used for the model estimation    
      data<-subset(myData, myData$index %in% dataKombs[,l] )
      
      
      #the fold that was left out in the model estimation
      predmatrix<-model.matrix(formula(myformula[m]), 
                               subset(myData, !(myData$index %in% dataKombs[,l] ))) 
      
      
      
      #Calculating the CV score for each model. sum((Y - Y(hat))^2)
      CV<-sum(
        (  
          #this is the observed Y for the left out fold.
          subset(myData, !(myData$index %in% dataKombs[,l] ))[,1] -
            #predmatrix description above.
            predmatrix %*%  
            #the estimated beta-hats. 
            linreg(formula = myformula[m], data = data)$beta.hat 
        )^2
      )
      
      #inserting the results in to the linearModels data.frame
      linearModels[o,] <- c(CV,myformula[m],ncol(predmatrix) - 1)
      o <- o + 1 
    }
  }
  #reforming data to numeric again. 
  linearModels[,1] <- as.numeric( linearModels[,1])
  linearModels[,3] <- as.numeric( linearModels[,3])
  
  #The mean for the different models, each model is estimadet K times  
  plotdata<-suppressWarnings(aggregate(linearModels,by = list(linearModels$model),FUN= mean)[,-3])
  
  #renaming a column to ease plotting
  colnames(plotdata)[1] <- "Model"
  
  #plotting
  engr<-ggplot() +
    geom_line(data = aggregate(plotdata,list(plotdata$nofeats),FUN = min)[,c(3,4)],aes(x=nofeats,y=CV)) +
    geom_point(data = plotdata,aes(x=nofeats,y=CV,col = factor(nofeats)) ) +
    labs(title = "CV scores for different no. feats",color = "No. feat")
  
  
  #displays the plot 
  plot(engr)
  
  #Returns the models with the lowest average CV-score 
  return( plotdata[min(plotdata$CV) == plotdata$CV,c(1,3,2)])
  
}  

# 1.2

myLM(Y=swiss[,1],X=swiss[,2:ncol(swiss)],Nfolds=5)






################################


