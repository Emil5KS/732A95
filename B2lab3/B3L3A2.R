set.seed(1234567890)
spam <- read.csv2("B2lab3/spambase.csv") 
ind <- sample(1:nrow(spam))
spam <- spam[ind,c(1:48,58)]
h <- 1
beta <- # Your value here
  M <- # Your value here
  N <- 500 # number of training points

spam$Spam[spam$Spam == 0] <- (-1) 

gaussian_k <- function(x, h) { # Gaussian kernel # Your code here
  return(exp(-(x^2/h))) 
} 


SVM <- function(sv,i){  
  # SVM on point i with support vectors sv
  # Your code here
  if(all(levels(factor(spam$Spam)) == c(-1,1)) | all(levels(factor(spam$Spam)) == c(1,-1)) ){
    
  }else{
    spam$Spam[spam$Spam == 0] <- (-1) 
  }
  # Note that the labels in spambase.csv are 0/1 and SVMs need -1/+1. Then, use 2*label-1
  # to convert from 0/1 to -1/+1
  # Do not include the labels when computing the Euclidean distance between the point i
  # and each of the support vectors. This is the distance to use in the kernel function
  # You can use dist() to compute the Euclidean distance }
  mydist <-as.matrix(dist(spam[,-ncol(spam)]))
  gauss  <- apply(mydist,2,FUN = gaussian_k,h=1)
  errors <- 1
  errorrate <- vector(length = N)
  errorrate[1] <- 1
  sv <- c(1)
  for(i in 2:N) {
    # Your code here
  }
  plot(errorrate[seq(from=1, to=N, by=10)], ylim=c(0.2,0.4), type="o") length(sv)
  errorrate[N]
}   



#Vet du vad en gotl�ndsk statistiker s�ger n�r han har ont i magen? 
#Jag har gauss i magen.
  