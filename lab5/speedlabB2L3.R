step8lapply <- function(SV){
  #yxm <- c()
  res<-c()
  
  distelement<-lapply(SV,function(m) as.matrix(dist(rbind(spam[m,-ncol(spam)],spam[m,-ncol(spam)]))))
  
  for (m in SV){
    yxm <-  sum(spam[m,"Spam"]* gaussian_k(distelement[[m]][-1,1]))
    res[m] <-  spam[m,"Spam"]*(step4(dataindex = spam[m,-ncol(spam)]) - yxm)
  }
  
  # rez<- lapply(SV,function(m){
  #   distelement <- as.matrix(dist(rbind(spam[m,-ncol(spam)],spam[m,-ncol(spam)]))) 
  #   yxm <-  sum(spam[m,"Spam"]* gaussian_k(distelement[-1,1]))
  #   res <-  spam[m,"Spam"]*(step4speed(dataindex = spam[m,-ncol(spam)]) - yxm)
  #   res
  # })
  return(which.max(res))
}



#  # for speedups
# euclidean_d  <- function(x, xi) {
#   x <- t(as.matrix(x))
#   xi <- as.numeric(xi)
#   sqrt(colSums((x - xi)^2))
#   ## d <- dist(rbind(x, xi))
#   ## d[(length(d) - nrow(x) + 1):length(d)]
# }


step4speed <- function(dataindex){
  
  
  distelement <-lapply(1:M,function(m){
    as.matrix(dist(rbind(dataindex,spam[m,-ncol(spam)])))
  })
  
  yxi <- c()
  for (m in 1:M){ 
    yxi[m]<-  sum(spam[m,"Spam"]* gaussian_k(distelement[[m]][-1,1]))
    #sum(1*spam[sv,"Spam"]* gaussian_k(dist(spam[i,],spam[sv,]))) + b
  }
  return(sum(yxi))
}



# 
# step4 <- function(dataindex){
#   # yxi <- c()
#   b<-0
#   # distelement <-apply(spam[sv,-ncol(spam)],MARGIN = 1, FUN = function(m){
#   #   as.matrix(dist(rbind(dataindex,m)))[-1,1]
#   # })  
#   distelement<-as.matrix(dist(rbind(dataindex,spam[sv,-ncol(spam)])))[-1,1]
#   #i<-1
#   # for (m in sv){
#   #  #distelement <- as.matrix(dist(rbind(dataindex,spam[m,-ncol(spam)])))
#   # yxi<-  c(yxi,
#   # #sum(1*spam[sv,"Spam"]* gaussian_k(dist(spam[i,],spam[sv,]))) + b
#   #  i<- i + 1
#   #  
#   #  
#   # } 
#   return(sum(spam[sv,"Spam"]* gaussian_k(distelement)) + b )
#   #return(sum(yxi))
# }

