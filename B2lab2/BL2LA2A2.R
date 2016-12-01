# # # # # # # # #         
 #             # 
  #           # 
   # # # # # # 
   # Block 2 # 
   #  Lab 2  #
   # 732A95  # 
   # # # # # # 
  #           # 
 #             #
# # # # # # # # #            



## Assignment 2

library(tree)
library(mboost)
library(randomForest)

BFR <- read.csv2("B2lab2/bodyfatregression.csv")
set.seed(1234567890)
BFR <- BFR[sample(nrow(BFR), replace = FALSE),]


train <- BFR[1:floor((nrow(BFR)*(2/3))),]
test <- BFR[74:nrow(BFR),]

## 2.1 

#Det här tror vi är rätt enligt formel 2 på ppt 5/19 i slides b2fl1

bfr.SE <- 0
set.seed(1234567890)
for (i in 1:100) {
  samptrain<-train[sample(nrow(train),replace = TRUE),] 
  bfr.tree        <- tree(Bodyfat_percent ~. ,data = samptrain)
  bfr.predictions <- predict(bfr.tree,test)
  bfr.SE[i]       <- sum((bfr.predictions - test$Bodyfat_percent))
}

mean((bfr.SE/100)^2)

### Det här tror vi är fel, men Caroline säger att det är rätt
bfr.SE <- 0
set.seed(1234567890)
for (i in 1:100) {
  samptrain<-train[sample(nrow(train),replace = TRUE),] 
  bfr.tree        <- tree(Bodyfat_percent ~. ,data = samptrain)
  bfr.predictions <- predict(bfr.tree,test)
  bfr.SE[i]       <- mean((bfr.predictions - test$Bodyfat_percent)^2)
}
mean(bfr.SE)
plot(bfr.SE) 


#Vafan gör jag? 

## 2.2 
bfr.SE2 <- c()
set.seed(1234567890)
for (i in 1:100){ 
  BFRre<- BFR[sample(nrow(BFR),replace = TRUE),]
  
  bfr.tree22 <- tree(Bodyfat_percent ~. ,data = BFRre )
  bfr.cv <- cv.tree(bfr.tree22, K = 3)
  best.size <- bfr.cv$size[which.min(bfr.cv$dev)]
  bfr.tree22 <- prune.tree(bfr.tree22, best = best.size)
  
  bfr.SE2[i] <- sum(predict(bfr.tree22, newdata = BFR) - BFR$Bodyfat_percent)
  print(i)
}


mean(  (bfr.SE2/100)^2)

#vilken data predictar jag på? 
summary(bfr.cv)
plot(bfr.cv)
predict(bfr.cv)
