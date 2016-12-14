set.seed(1234567890)
spam <- read.csv2("B2lab3/spambase.csv") 
ind <- sample(1:nrow(spam))
spam <- spam[ind,c(1:48,58)]
h <- 1
betai <- -0.5  # Your value here
  Mi <- 20 # Your value here
  Ni <- 500 # number of training points

  if(all(levels(factor(spam$Spam)) == c(-1,1)) | all(levels(factor(spam$Spam)) == c(1,-1)) ){
    
  }else{
    spam$Spam[spam$Spam == 0] <- (-1) 
  }
  
gaussian_k <- function(x, h = 1) { # Gaussian kernel # Your code here
  return(exp(-(x/h)^2)) 
} 


#a_n = C- m_n



SVM <- function(sv,i,M = Mi, beta = betai){  


  #  # for speedups
  # euclidean_d  <- function(x, xi) {
  #   x <- t(as.matrix(x))
  #   xi <- as.numeric(xi)
  #   sqrt(colSums((x - xi)^2))
  #   ## d <- dist(rbind(x, xi))
  #   ## d[(length(d) - nrow(x) + 1):length(d)]
  # }
  
  
  step4 <- function(dataindex){
     yxi <- c()
     for (m in 1:M){ 
      distelement <- dist(cbind(dataindex,spam[m,-ncol(spam)]))
       
     yxi[m]<-  spam[m,"Spam"]* gaussian_k(distelement)
     #sum(1*spam[sv,"Spam"]* gaussian_k(dist(spam[i,],spam[sv,]))) + b
    
     }
     return(sum(yxi))
  }
  
  # step4speed <- function(dataindex){
  #   yxi <- c()
  #   for (m in 1:M){ 
  #     distelement <-  euclidean_d(x=dataindex,xi=spam[m,-ncol(spam)])
  #     
  #     yxi[m]<-  spam[m,"Spam"]* gaussian_k(distelement)
  #     #sum(1*spam[sv,"Spam"]* gaussian_k(dist(spam[i,],spam[sv,]))) + b
  #     
  #   }
  #   return(sum(yxi))
  # }
  # 
  
  
  step8 <- function(SV){
    yxm <- c()
    res<-c()
    for (m in SV) {
      distelement <- dist(cbind(spam[m,-ncol(spam)],spam[m,-ncol(spam)])) 
      yxm[m] <-  spam[m,"Spam"]* gaussian_k(distelement)
      res[m] <-  spam[m,"Spam"]*(step4(dataindex = spam[m,-ncol(spam)]) - yxm[m])
    } 
    return(which.max(res))
  }  
  
  # 
  # step8lapply <- function(SV){
  #   #yxm <- c()
  #   #res<-c()
  #   rez<- lapply(SV,function(m){
  #     distelement <- dist(cbind(spam[m,-ncol(spam)],spam[m,-ncol(spam)])) 
  #     yxm <-  spam[m,"Spam"]* gaussian_k(distelement)
  #     res <-  spam[m,"Spam"]*(step4speed(dataindex = spam[m,-ncol(spam)]) - yxm[m])
  #     res
  #   }) 
  #   return(which.max(rez))
  # }  
  # 
  
  
  b <- 0
  errors <- 1
  errorrate <- vector(length = N)
  errorrate[1] <- 1
  sv <- c(1)
  
  
  
  
  for(i in 2:N) {

    s4<-step4(dataindex = spam[i,-ncol(spam)])
    
    if (spam[i,"Spam"]*s4 <= beta){
    
        sv[i] <- i 
        errors <- errors + 1
        
      if (length(sv) > M  ){
        
        sv <- sv[-step8(SV = sv)]
        
      }
    }
    
  errorrate[i] <- errors / i 
  
    }
  #plot(errorrate[seq(from=1, to=N, by=10)], ylim=c(0.2,0.4), type="o") 
  plot(errorrate)
  length(sv)
  errorrate[N]
  return(errorrate)
}   



system.time()
svm1<-SVM(M = 500, beta = 0)
svm2<-SVM(M = 500, beta = -0.05)
svm3<-SVM(M = 20, beta = 0)
svm4<-SVM(M = 20, beta = -0.05)

#Vet du vad en gotländsk statistiker säger när han är dålig i magen? 
#Jag har gauss i magen.
  