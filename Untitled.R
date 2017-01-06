install.packages("kknn")
install.packages("ggplot2")
install.packages("boot")
install.packages("reshape2")
library(reshape2) #här finns melt
library(boot) #finns cv i den har
library(ggplot2)
library(kknn)

library(kknn) 
library(ggplot2) 
library(gridExtra) 
library(MASS) 
library(glmnet) 
library(reshape2) 
library(tree) 
library(partykit) 
library(e1071) 
library(boot) 
library(grid) 
library(fastICA) 
library(pls) 
library(geosphere) 
library(neuralnet) 
library(mgcv) 
library(mboost) 
library(randomForest) 
library(pamr) 
library(kernlab)

#Laddar in alla paket jag anvandt under ML-kursen
###

paket <- c('kknn','ggplot2','gridExtra','MASS','glmnet',
           'reshape2','tree','partykit','e1071','boot',
           'grid','fastICA','pls','geosphere','neuralnet',
           'mgcv','mboost','randomForest','pamr','kernlab',
           'mixtools','LiblineaR')



if(!all(paket %in% rownames(installed.packages()))){
  
  for(i in (paket[!paket %in% rownames(installed.packages())]) ){
    print(paste("installing package:",i))
    install.packages(i)
  } 
}else("Everything is installed... or is it?")

for (i in paket){
  do.call(library,list(i))
}

###


confmat <- function(model,Y,...){
  
  pred    <- predict(model,...)
  mytable <- table(Observed = Y, Predicted = pred)
  print(mytable)
  return( sum(diag(mytable)) / sum(mytable[,1],mytable[,2]) )
}

loss <- function(class1,class2,lossfunc){
    ifelse( (cl1/cl2) > lossfunc[1,2]/lossfunc[2,1] )
  }

loss<-matrix(c(0,1,10,0),ncol = 2)5873

colnames(loss) <- c("TRUE","FALSE")
rownames(loss) <- c("TRUE", "FALSE")

#Code

```{r, ref.label=knitr::all_labels(),echo=TRUE,eval=FALSE}
```


loss
# Nu i högra hörnet är False negative
# I vänstra hörnet är False positive
# Nu när vi sätter False negative till 10 säger vi att vi ska vara 
# tio gånger så säkra på att observationen är TRUE jämfört med att den är FALSE


dataDiv <- function(data,train,test,validation){    

indexValues <- rep(1, ceiling(nrow(data)*train)) 
indexValues <- c(indexValues, rep(2,floor(nrow(data)*test)))    
indexValues <- c(indexValues, rep(3,nrow(data)*validation))

data$index  <- sample(indexValues,replace = FALSE)

mlist <- list(
  
  train = subset(data,indexValues == 1, select = colnames(data)[1:ncol(data)-1] ),
  test  = subset(data,indexValues == 2, select = colnames(data)[1:ncol(data)-1] ),
  validation  = subset(data,indexValues == 3, select = colnames(data)[1:ncol(data)-1] )
            
) 
return(mlist)
}  

spliti<-dataDiv(data,0.50,0.25,0.25)

train <- spliti$train  
test  <- spliti$test
validation <- spliti$validation


2740*0.34
rep(1,nrow(data)*0.3)

