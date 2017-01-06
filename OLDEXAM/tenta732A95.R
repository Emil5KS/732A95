
#ass 1 

# 1.1

glass <- read.csv2("OLDEXAM/glass.csv")

set.seed(12345)
glass <- sample(glass, replace = FALSE)

train <- glass[1:107,]
test  <- glass[108:(108+53),]
valid  <- glass[(108+54):214,]

library(tree)


glass.tree <- tree(formula = Al ~ ., data = train, split = "deviance" )  

no.leafs <- data.frame(trainS=1,testS = 1)
rad <- 1 

for (i in 2:9) {  
  pruned.glass    <-  prune.tree(glass.tree, best = i)
  # no.leafs[rad,1] <-  mean( (train$Al - predict(pruned.glass))^2 )
  # no.leafs[rad,2] <-   mean((valid$Al - predict( pruned.glass , newdata = valid, type = "vector"))^2)
  #  
   no.leafs[rad,1] <-  deviance( pruned.glass )
   no.leafs[rad,2] <-  deviance(predict( pruned.glass , newdata = valid, type = "tree"))
  rad <- rad + 1 
}


library(ggplot2)


ggplot(data = no.leafs) + geom_point(aes(x = 1:8, y = trainS),color = "blue") +
  geom_point(aes(x = 1:8, y = testS),color = "red")



library(pls)

myplsr <- plsr(formula = Al ~ ., data = train, validation  = c("CV") )

plot(myplsr)

myplsr$loadings

# 1.3. c) 
myplsr$validation$PRESS
#svar: alla 7 komponenter då lägst PRESS.

# e)
myplsr$coefficients

# f)

mean((test$Al - predict(myplsr, newdata = test))^2)




# Assignment 2 
scars <- mtcars
scars$hp <- scale(scars$hp)
scars$hp <- scale(scars$qsec)

ggplot(data = scars, aes(x=qsec,y=hp, color = am) ) + geom_point()

#No there are overlapping points that i think would be hard to discriminate, and it would 
#be very hard to separate thoes groups with a straight line.
#Yes, both groups seem to have equal variance if you disregard the outlier to the top far right
#Since the LDA assuptions dosn't need to be very strong it dosn't seem something to worry about.

library(MASS)
equalpriors<-lda(am~ hp + qsec,data = scars, prior = c(1,1)/2 )

scars$eqprior<-ifelse(predict(equalpriors,type = "class")$x > 0,0,1)
ggplot(data = scars, aes(x=qsec,y=hp, color = eqprior) ) + geom_point()


equalpriors

proppriors<-lda(am~ hp + qsec,data = scars, prior = c(19,13)/nrow(scars) )

scars$pp<-ifelse(predict(proppriors,type = "class")$x > 0,0,1)
ggplot(data = scars, aes(x=qsec,y=hp, color = pp) ) + geom_point()

proppriors

# The line moved sligthly to the left. But the parameters are unchanged since 
# we only change the proportions which effect... what? 



## 

mykernel<-function(X,Xtest,lambda){
density(X)
  
}


minicars<-subset(mtcars, am == 0,select = c("qsec","hp"))
minicars <- scale(cbind(minicars[,1],minicars[,2]))
dest <- density(minicars,0.2,kernel = c("epanechnikov"))
dest$x

