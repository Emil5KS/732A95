   
   #  # # # # # #
  #               #
 # #    732A95   # #
# # # # # # # # # # #
   # Assignment 1 #
# # # # # # # # # # # 
 # #    732A95   # #
  #               #
    # # # # # # # 
      # # # # # 
        # # # 
          #

library(ggplot2)
library(tree)

## 1.1 

State <- read.csv2("lab4/State.csv")
State <- State[order(State$MET),]


a11 <- ggplot(data = State, aes(x = MET,y = EX)) + geom_point()
plot(a11)
## 1.2 

## Har eg. ingen koll alls på det här.
state.tree <- tree(EX ~ MET, data = State, minsize = 8 , split = "deviance")


plot(state.tree)

plot(cv.tree(state.tree))
# three is the optimal tree. 


state.prune <- prune.tree(state.tree, best=3)

plot(state.prune)

a12 <- a11 + geom_point( y = predict(state.prune), col = "red" )
plot(a12)

ggplot(data = data.frame(x = resid(state.prune))) + 
  geom_histogram(aes(x = x), bins = 10)

# a bitt shotty, to many residuals in the right tail, but overall ok. 

## 2.3

library(boot)

tree.fun <- function(data, ind){
  
  data <- data[ind,]
  trio <- tree(EX ~ MET, data = data, minsize = 8 , split = "deviance")
  return( predict(trio, newdata = data) )
  
}

set.seed(12345)
tree.boot <- boot(State, tree.fun, R = 1000)

tree.boot$t

predict(tree.boot)
boot.ci(tree.boot, type = "norm")$normal


State.plot <- data.frame(lower =envelope(tree.boot)$point[2,])
State.plot$upper <-  envelope(tree.boot)$point[1,]
State.plot$predicted<-predict(state.prune)

library(reshape2)

State.plot<-melt(State.plot)
State.plot$row <- rep(State$MET,times = 3)
a11 + geom_line(data =  State.plot, aes(x = row,y = value, col =variable ))


## 2.4

tree.fun.para <- function(data, ind){
  
  data <- data[ind,]
  mean(data$EX)
  trio <- tree(EX ~ MET, data = data, minsize = 8 , split = "deviance")
  preD<-predict(trio,data)
  
  myPredictions<- rnorm(length(preD),mean = mean(preD), sd =  sd(resid(trio)) )
  
  return( myPredictions )
  
}


set.seed(12345)
tree.boot.para <- boot(State, tree.fun.para, R = 1000)

tree.boot$t

boot.ci(tree.boot.para, type = "norm")$normal
#State.plot <- State[c("EX","MET")]
State.plot <- data.frame(lower =envelope(tree.boot.para)$point[2,])
State.plot$upper <-  envelope(tree.boot)$point[1,]
State.plot$predicted<-predict(state.prune)

library(reshape2)

State.plot<-melt(State.plot)
State.plot$row <- rep(State$MET,times = 3)
a11 + geom_line(data =  State.plot, aes(x = row,y = value, col =variable ))


