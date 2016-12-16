set.seed(1234567890)
spam <- read.csv2("B2lab3/spambase.csv") 
ind <- sample(1:nrow(spam))
spam <- spam[ind,c(1:48,58)]
h <- 1
betai <- -0.5  # Your value here
  Mi <- 20 # Your value here
  N <- 500 # number of training points

  if(all(levels(factor(spam$Spam)) == c(-1,1)) | all(levels(factor(spam$Spam)) == c(1,-1)) ){
    
  }else{
    spam$Spam[spam$Spam == 0] <- (-1) 
  }
  
gaussian_k <- function(x, h = 1) { # Gaussian kernel 
  return(exp(-(x^2/2*h^2))) 
} 


#a_n = C- m_n



SVM <- function(sv,i,M = Mi, beta = betai){  



  
   step4 <- function(dataindex){
      # yxi <- c()
      b<-0
      # distelement <-apply(spam[sv,-ncol(spam)],MARGIN = 1, FUN = function(m){
      #   as.matrix(dist(rbind(dataindex,m)))[-1,1]
      # })  
     distelement<-as.matrix(dist(rbind(dataindex,spam[sv,-ncol(spam)])))[-1,1]
    #i<-1
      # for (m in sv){
      #  #distelement <- as.matrix(dist(rbind(dataindex,spam[m,-ncol(spam)])))
      # yxi<-  c(yxi,
      # #sum(1*spam[sv,"Spam"]* gaussian_k(dist(spam[i,],spam[sv,]))) + b
      #  i<- i + 1
      #  
      #  
      # } 
     return(sum(spam[sv,"Spam"]* gaussian_k(distelement)) + b )
      #return(sum(yxi))
   }
  
   step8 <- function(SV){
     yxm <- c()
     res<-c()
     for (m in SV) {
       distelement <- as.matrix(dist(rbind(spam[m,-ncol(spam)],spam[m,-ncol(spam)]))) 
       yxm <-  sum(spam[m,"Spam"]* gaussian_k(distelement[-1,1]))
       res <-  c(res,spam[m,"Spam"]*(step4(dataindex = spam[m,-ncol(spam)]) - yxm))
     } 
     return(which.max(res))
   }  
   
  
  # as.matrix(dist(rbind(spam[1,-ncol(spam)],spam[1,-ncol(spam)])))

 
  
  
  b <- 0
  errors <- 1
  errorrate <- vector(length = N)
  errorrate[1] <- 1
  sv <- c(1)
  
  
  s4<-c()
  
  for(i in 2:N) {

    s4<-step4(dataindex = spam[i,-ncol(spam)])
    
    if(spam[i,"Spam"]*s4 < 0){
      errors <- errors + 1
    }
    
    if(spam[i,"Spam"]*s4 < beta){
      sv[length(sv)+1] <- i 
    }
    
    if (length(sv) > M  ){
        sv <- sv[-step8(SV = sv)]
      }
    errorrate[i] <- errors / i 
    }
  plot(errorrate)
  length(sv)
  errorrate[N]
  return(errorrate)

  
}
  #plot(errorrate[seq(from=1, to=N, by=10)], ylim=c(0.2,0.4), type="o") 

#}   



#system.time()

  system.time(svm1<-SVM(M = 500, beta = 0))
svm2<-SVM(M = 500, beta = -0.05)
svm3<-SVM(M = 20, beta = 0)
svm4<-SVM(M = 20, beta = -0.05)


#Vet du vad en gotländsk statistiker säger när han är dålig i magen? 
#Jag har gauss i magen.
  


distelement <-apply(spam[1:500,-ncol(spam)],MARGIN = 1, FUN = function(m){
  as.matrix(dist(rbind(spam[1,-ncol(spam)],m)))
})
as.matrix(dist(rbind(spam[1,-ncol(spam)],spam[6,-ncol(spam)])),nrow = 2,ncol =2)

distelement[2,]
