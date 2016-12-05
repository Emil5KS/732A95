# # # # # # # # # # # 
# # # # # # # # # # #
# # # 
# # # 
# # # 
# # # # # # # 
# # # # # # # 
# # #
# # # 
# # #
# # # 
# # # # # # # # # # #
# # # # # # # # # # # 





set.seed(1234567890)
max_it <- 100 # max number of EM iterations
min_change <- 0.1 # min change in log likelihood between two consecutive EM iterations
N=1000 # number of training points
D=10 # number of dimensions
x <- matrix(nrow = N, ncol = D) # training data
true_pi <- vector(length = 3) # true mixing coefficients
true_mu <- matrix(nrow = 3, ncol = D) # true conditional distributions
true_pi <- c(1/3, 1/3, 1/3)
true_mu[1,] <- c(0.5,0.6,0.4,0.7,0.3,0.8,0.2,0.9,0.1,1)
true_mu[2,] <- c(0.5,0.4,0.6,0.3,0.7,0.2,0.8,0.1,0.9,0)
true_mu[3,] <- c(0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5)
plot(true_mu[1,], type = "o", col = "blue", ylim = c(0, 1))
points(true_mu[2,], type="o", col="red")
points(true_mu[3,], type="o", col="green")


# Producing the training data
for(n in 1:N) {
  k <- sample(1:3,1,prob=true_pi)
  for(d in 1:D) {
    x[n,d] <- rbinom(1,1,true_mu[k,d])
  }
}

K <- 3 # number of guessed components
z <- matrix(nrow=N, ncol=K) # fractional component assignments
pi <- vector(length = K) # mixing coefficients
mu <- matrix(nrow=K, ncol=D) # conditional distributions
llik <- vector(length = max_it) # log likelihood of the EM iterations


# Initialization of the paramters (in a random manner)
pi <- runif(K,0.49,0.51)
pi <- pi / sum(pi)


for(k in 1:K) {
  mu[k,] <- runif(D,0.49,0.51)
}

pi 
mu


## Här ska algoritmen skrivas 
#ml<- function(){
for(it in 1:max_it) { 
  plot(mu[1,], type="o", col="blue", ylim=c(0, 1))
  points(mu[2,], type="o", col="red")
  points(mu[3,], type="o", col="green")
  #points(mu[4,], type="o", col="yellow")
  #Sys.sleep(0.5)
  # E-step: Computation of the fractional component assignments
  # Your code here
  for (i in 1:nrow(x)){ 
    for (j in 1:nrow(mu)){
    
    z[i,j]<-prod(mu[j,]^(x[i,])*(1-mu[j,])^(1-x[i,])) * pi[j]
    #print(z[i,j])
    }
  }
  #print(dim(z))
  for (l in 1:nrow(z)){ 
    z[l,]<- z[l,]/sum(z[l,])
    
  } 
  #print(dim(z)) 
  part2<- c()
  tempvar <- c()
  #Log likelihood computation.
  for (rad in 1:nrow(x)){ 
    for (klass in 1:nrow(mu)){
  part1 <- x[rad, ] * log( mu[klass, ] ) + (1 - x[rad, ])*log(1 - mu[klass, ])
  part2[klass]<-z[rad, klass]*(log(pi[klass]) + sum(part1))
    }
    tempvar[rad] <- sum(part2) 
  }
  
  llik[it] <- sum(tempvar)
  cat("iteration: ", it, "log likelihood: ", llik[it], "\n")  
  #flush.console()
  # Stop if the lok likelihood has not changed significantly
  # Your code here
  if (it >1){ 
    if(abs(abs(llik[it]) - abs(llik[it-1])) < min_change){ 
  stop("The log-likelihood as not change significantly, returning from loop")
    }
  }
  #M-step: ML parameter estimation from the data and fractional component assignments
  # Your code here
  
  pi <- colSums(z) / 1000 # pi_k-ML
  
  for (class in 1:nrow(mu)){
    for (column in 1:ncol(mu)){ 
      mu[class,column] <- sum( z[,class]*x[,column] )/sum( z[,class] )
    }
  }  
   
}  
  
#} 

pi
mu
plot(llik[1:it], type="o")
debugonce(ml)
ml()





9






for (i in 1:nrow(x)){ 
  for (j in 1:nrow(mu)){
    
    z[i,j]<-prod(mu[j,]^(x[i,])*(1-mu[j,])^(1-x[i,])) *pi[j]
    print(z[i,j])
  } 
}

z[,1]*pi/colSums(z*pi)

colSums(z*pi) / 1000# pi_k-ML

t(x)%*%z*pi / colSums(z) # typ mu_k-ML steget, utan nämnaren

x-mu


mu^(x[i,j])*(1-mu)^(1-x[i,j])
for (k in 1:ncol(mu)){ 
print(mu[,k]^(x[1,1])*(1-mu[,k])^(1-x[1,1]))
}

prod(mu[3,]^(x[1,])*(1-mu[3,])^(1-x[1,]))
dim(mu)
dim(x)








