### Lab2 

setwd("/Users/EmilsHem/Documents/732A95/lab1")

machines <- data.frame(read.csv2("machines.csv"))
machines<-as.vector(unlist(machines$Length))

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

#######