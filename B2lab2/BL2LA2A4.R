# # # # #   Block 2    # # # # # 
    # # #    Lab 2     # # # 
        # Assignemnt 4 # 

library(randomForest)

spam <- read.csv2("B2lab2/spambase.csv")
spam$Spam <- factor(spam$Spam)
set.seed(1234567890)
spam <- spam[sample(nrow(spam)),]
spamTrain <- spam[1:(nrow(spam)*2/3),]
spamTest <- spam[3068:4601,]


#adaTree <- mboost(Spam~., data = spamTrain, family = AdaExp() , baselearner = "btree" )

adaTree <- blackboost(Spam~., data = spamTrain, family = AdaExp(), control = boost_control(mstop = 100))
predvals <- predict(adaTree, newdata = spamTest, type = "class") 
table(Predicted = predvals, Observed = spamTest$Spam)
hatval <- adaTree$baselearner

ada.trees<- sapply(X = seq(10,100,10), FUN = function(y) { 
  set.seed(1234567890)
  adaTree <- blackboost(Spam~., data = spamTrain, family = AdaExp(), control = boost_control(mstop = y))
  predvals <- predict(adaTree, newdata = spamTest, type = "class") 
  return(table(Predicted = predvals, Observed = spamTest$Spam))
  
})


mcrplot <- data.frame(mstop = seq(10,100,10))  
mcrplot$ada.trees<-colSums(ada.trees[c(1,4),])/colSums(ada.trees)  

ggplot(data = mcrplot)+geom_point( aes(x = mstop, y=ada.trees), col = "red")  
#control = boost_control(mstop = 100)

set.seed(1234567890) 
wierdTree <- randomForest(formula = Spam ~., data = spamTrain,  control = boost_control(mstop = 100)) 

RFpredvals <- predict(wierdTree, newdata = spamTest, type = "class") 
table(Predicted = RFpredvals, Observed = spamTest$Spam) 

ten.trees<- sapply(X = seq(10,100,10), FUN = function(y){
  set.seed(1234567890)
wierdTree <- randomForest(formula = Spam ~., data = spamTrain,  control = boost_control(mstop = y))
RFpredvals <- predict(wierdTree, newdata = spamTest, type = "class")
table(Predicted = RFpredvals, Observed = spamTest$Spam)
}) 


mcrplot$ten.trees <- colSums(ten.trees[c(1,4),])/colSums(ten.trees)  

ggplot(data = mcrplot) + 
  geom_point( aes(x = mstop, y=ada.trees), col = "red") +
  geom_point( aes(x = mstop, y=ten.trees), col = "blue")

