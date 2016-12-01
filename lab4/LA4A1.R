   
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
text(state.prune)
a12 <- a11 + geom_point( y = predict(state.prune), col = "red" )
plot(a12)

ggplot(data = data.frame(x = resid(state.prune))) + 
  geom_histogram(aes(x = x), bins  = 10)

# a bitt shotty, to many residuals in the right tail, but overall ok. 

## 1.3

library(boot)

tree.fun <- function(data, ind){
  
  data <- data[ind,]
  trio <- tree(EX ~ MET, data = data,
               control = 
                 tree.control( minsize = 8, nobs = nrow(data))
               )
  trio <- prune.tree(trio, best = 3)
  return( predict(trio, newdata = State) )
  
}

set.seed(12345)
tree.boot <- boot(State, tree.fun, R = 1000)



State.plot <- data.frame(lower =envelope(tree.boot)$point[2,])
State.plot$upper <-  envelope(tree.boot)$point[1,]
State.plot$predicted<-predict(state.prune)
State.plot$EX <- State$EX 
State.plot$MET <- State$MET

ggplot(data = State.plot) + 
  geom_point(aes(x = MET, y = EX)) +
  geom_point(aes(x = MET, y = predicted), col = "red") +
  geom_ribbon(aes(x = MET ,ymin = lower, ymax = upper), alpha = 0.2)

#library(reshape2)
#State.plot<-melt(State.plot)
#State.plot$row <- rep(State$MET,times = 3)
#a11 + geom_line(data =  State.plot, aes(x = row,y = value, col =variable ))


## 1.4




myrng <- function(data, model) {
  data1<-data.frame(EX = data$EX, MET = data$MET)
  n<-nrow(data1)
  data1$EX <- rnorm(n,predict(model, newdata=data1),sd(resid(model)))
  return(data1)
}

para.fun <- function(data1){ 
  
  trio <- tree(EX ~ MET, data = data1, 
                 control = 
                   tree.control( minsize = 8, nobs = nrow(data1)))
  trio <- prune.tree(trio, best = 3)
    #predict values for all Area values from the original data
  priceP <- predict(trio,newdata=data1) 
  return(priceP)
} 

tree.boot.para <- boot(State, statistic = para.fun, R=1000, 
            mle=state.prune ,ran.gen = myrng, sim="parametric")

set.seed(12345)
State.plot$lower2 <-  envelope(tree.boot.para)$point[2,]
State.plot$upper2 <-  envelope(tree.boot.para)$point[1,]

ggplot(data = State.plot) + 
  geom_point(aes(x = MET, y = EX)) +
  geom_point(aes(x = MET, y = predicted), col = "red") +
  geom_ribbon(aes(x = MET ,ymin = lower2, ymax = upper2), alpha = 0.2)


## prediction enda som är nytt att predict nu använder State istället för data-variabeln 

para.fun.p <- function(data1){ 
  
  trio <- tree(EX ~ MET, data = data1, 
               control = 
                 tree.control( minsize = 8, nobs = nrow(State)))
  trio <- prune.tree(trio, best = 3)
  #predict values for all Area values from the original data
  priceP <- predict(trio,newdata=State) 
  priceP <- rnorm(n = nrow(data1), mean = priceP, sd = sd(resid(trio)))
  return(priceP)
} 

set.seed(12345)
tree.boot.para.p <- boot(State, statistic = para.fun.p, R=1000, 
                       mle=state.prune ,ran.gen = myrng, sim="parametric")

State.plot$lower3 <-  envelope(tree.boot.para.p)$point[2,]
State.plot$upper3 <-  envelope(tree.boot.para.p)$point[1,]

ggplot(data = State.plot) + 
  geom_point(aes(x = MET, y = EX)) +
  geom_point(aes(x = MET, y = predicted), col = "red") +
  geom_ribbon(aes(x = MET ,ymin = lower3, ymax = upper3), alpha = 0.2,col = "orange",fill ="orange")








#State.plot <- State[c("EX","MET")]
# State.plot <- data.frame(lower =envelope(tree.boot.para)$point[2,])
# State.plot$upper <-  envelope(tree.boot)$point[1,]
# State.plot$predicted<-predict(state.prune)

# library(reshape2)

# State.plot<-melt(State.plot)
# State.plot$row <- rep(State$MET,times = 3)
# a11 + geom_line(data =  State.plot, aes(x = row,y = value, col =variable ))


