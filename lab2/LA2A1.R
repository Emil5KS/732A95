################################
################################
#######  Computer lab 2  ####### 
#######     732A95       #######
#######  Assignment 1    #######

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




