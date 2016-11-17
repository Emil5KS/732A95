#####################
#####################
#######Computer lab 2 
#######732A95


setwd("/Users/EmilsHem/Documents/732A95/lab2")
read.csv("tecator.csv")
data<-swiss
K<-3

set.seed(12345)


myLM <- function(Y, X, Nfolds){
  
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
  
  q<-rep(paste0("X",c(1:5)))
  
  myData <- cbind(Y,X) 
  colnames(myData)<- c("Y",q)
  
  myComb<-sapply(c(1:5), FUN = combn, x = q )
  myformula <- c(myComb[[1]])
 
  
  for (i in 2:3){
    for (j in 1:10){
      myformula[length(myformula)+1]<-(paste(myComb[[i]][,j],collapse = " + "))
    }
  }
  
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
 #and reorder the data with them. 
 set.seed(12345)
 myData<-myData[sample(noobs),] 
 
 #Create K equal indexes 
 cut(1:noobs,breaks=K,labels = FALSE)
 
 myData$index <- cut(1:noobs,breaks=K,labels = FALSE)
 
 # Create a list with all the K data frames
 #WILL PROBABLY BE NEEDED LATER SO DONT DELETE IT DIPSHIT
 dataFrejm<-list()
 for (i in 1:K){
   dataFrejm[[i]] <- myData[which(myData$index == i),] #-length(data) add if dont want index
 }
 
 o <- 1
 linearModels<-data.frame(CV=1,model="text",nofeats=1,stringsAsFactors = FALSE)
 dataKombs <- combn(1:K,K-1)
 for (m in 1:length(myformula)){   
 for (l in (1:K)){  
    
data<-subset(myData, myData$index %in% dataKombs[,l] )
predmatrix<-model.matrix(formula(myformula[m]), 
             subset(myData, !(myData$index %in% dataKombs[,l] )))  

 CV<-sum((subset(myData, !(myData$index %in% dataKombs[,l] ))[,1]-predmatrix %*% linreg(formula = myformula[m], data = data)$beta.hat)^2)
   linearModels[o,] <- c(CV,myformula[m],ncol(predmatrix) - 1)
   o <- o + 1 
    }
 }
 linearModels[,1] <- as.numeric( linearModels[,1])
 linearModels[,3] <- as.numeric( linearModels[,3])
   
 
 plotdata<-suppressWarnings(aggregate(linearModels,by = list(linearModels$model),FUN= mean)[,-3])
 colnames(plotdata)[1] <- "Model"
 engr<-ggplot() +
   geom_line(data = aggregate(plotdata,list(plotdata$nofeats),FUN = min)[,c(3,4)],aes(x=nofeats,y=CV)) +
   geom_point(data = plotdata,aes(x=nofeats,y=CV,col = factor(nofeats)) ) +
       labs(title = "CV scores for different no. feats",color = "No. feat")
 
 

plot(engr)
 
 return( plotdata[min(plotdata$CV) == plotdata$CV,c(1,3,2)])
   #in hit ska linregfunktionen tillsammans med formulan.
   #Loops, loops everywhere. 
#as.matrix(dataFrejm[[1]][,c(-1,-7)])
   # GENERATING EACH MODEL AND CALCULATING CV
  
  #Borde få ut ett lm-objekt med y~x1 fittat på de två första delarna av settet
}  


myLM(Y=swiss[,1],X=swiss[,2:ncol(swiss)],Nfolds=5)




