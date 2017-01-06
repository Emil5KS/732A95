#A txt file, maybe works with R-files but idk.
midat<-readLines(file("/Users/EmilsHem/Documents/732A95/732A95/library"))


# Takes a text file and returns the packages loaded with library and
# returns them in a text-vector.

# Specify install = TRUE and it will automaticly install all
# missing packages and load them.

InstLoad <- function(script,install = FALSE){
  
  
paket<-unique(trimws(script[grep(pattern = "library\\(.+\\)" ,x=midat)]))

paket<-sub(x = paket,pattern = ".+\\(",replacement = "")
paket<-sub(x = paket,pattern = "\\)",replacement = "")


# for making a vector out of it, redundant, but nice to have. 
#paste0("c('",paste(paket,collapse = "','"),"')") 

#paket <- c('kknn','ggplot2','gridExtra','MASS','glmnet','reshape2','tree','partykit','e1071','boot','grid','fastICA','pls','geosphere','neuralnet','mgcv','mboost','randomForest','pamr','kernlab')

if(install == TRUE ){

if(!all(paket %in% rownames(installed.packages()))){
  
  for(i in (paket[!paket %in% rownames(installed.packages())]) ){
    print(paste("installing package:",i))
    install.packages(i)
  } 
}else("Everything is installed... or is it?")

for (i in paket){
  do.call(library,list(i))
}
} else{
  return(paket)
  }
  
  
}


# The function
InstLoad(midat)
