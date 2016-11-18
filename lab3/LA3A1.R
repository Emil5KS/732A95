################################################
############## Computer Lab 3 ##################
# # # # Introduction to Machine Learning # # # #
#############  Assignment 1   ##################
################################################

# 1.1 

crabs <- read.csv("lab3/australian-crabs.csv")
library(ggplot2)
p<- ggplot(data = crabs) + geom_point(aes(x = RW, y =CL, col = sex))
plot(p)
attach(crabs)
# 1.2 

LDA<- function(X){

RW <- X[,1]
CL <- X[,2]
sex<- X[,3]
myMu <- aggregate(cbind(RW,CL),by = list(sex), FUN = mean, simplify = TRUE)
myCov <- by(cbind(RW,CL), list(sex), cov, method = "pearson")
myPi <- aggregate(cbind(RW,CL),by = list(sex), FUN =function(x) length(x)/nrow(cbind(RW,CL)), simplify = TRUE)


mySig<- (( myCov[[1]] * myPi[2,2] * length(RW) ) + (myCov[[2]] * myPi[2,3] * length(RW)) ) / 200


woMale <- -0.5 * as.matrix(myMu[2,2:3],ncol = 2) %*% solve(mySig) %*% t(myMu[2,2:3]) + log(myPi[2,3])
woFem <- -0.5 * (as.matrix(myMu[1,2:3],ncol = 2)) %*% solve(mySig) %*% t(myMu[1,2:3]) + log(myPi[1,3])
#woBoth<-sapply(1:2, FUN = function(x) -0.5 * as.matrix(myMu[x,2:3],ncol = 2) %*% solve(mySig) %*% t(myMu[x,2:3]) + myPi[x,2:3])

wM<- solve(mySig) %*% t(myMu[2,2:3])
wF<- solve(mySig) %*% t(myMu[1,2:3])
#wFM<-sapply(1:2, FUN = function(x) solve(mySig) %*% t(myMu[x,2:3]))
#t(cbind(RW,CL)) %*% mySig %*% myMy[2,2:3] -  

a <- (woMale - woFem) 
b <- wM - wF
x <- cbind(X[,1:2])

#w0s<- a 
#w1s <- b[1]
#w2s <- b[2]
myInter <- as.numeric(-a/b[2]) 
mySlope <- as.numeric(-b[1]/b[2])

X$myClass<-t(ifelse((a[1] + t(b) %*% t(x)) > 0 ,levels(X[,3])[2],levels(X[,3])[1]))
colnames(X)[4] <- "Predicted" 
retObj<-list(w0 = c(woMale,woFem),
             w1 = cbind(wM=wM,wF=wF), 
             myClass = X,
             myModel = c(myInter = myInter, mySlope = mySlope))

return(retObj)
}

results <- LDA(crabs[,c(5,6,2)])

## 2.3
#actualdata + desicion boundaries
#p + geom_abline(intercept = results$myModel[1], slope = results$myModel[2], col = "Red")

#predicted classes + desicion boundaries
ggplot(data = results$myClass) + geom_point(aes(x = RW, y =CL, col = Predicted)) +
  geom_abline(intercept = results$myModel[1], slope = results$myModel[2], col = "Red") +
  labs(title = "Predicted values for my own LDA() function")

## 1.4


myLogit<-glm(sex~RW + CL, family = binomial(link='logit') )
summary(myLogit)
plot(myLogit)
myDecLog<-coef(myLogit)[1:2]/-coef(myLogit)[3]

ggplot(data = results$myClass) + geom_point(aes(x = RW, y =CL, col = ifelse(myLogit$fitted.values > 0.5,1,0))) +
  geom_abline(intercept = myDecLog[1], slope = myDecLog[2]) 
+geom_contour(aes(density = mySig))
  

table(ifelse(myLogit$fitted.values > 0.5,"Male","Female"),sex)


round(predict(myLogit),digits = 4)

############





###############################################################Skit nedan
#
#
#

my


woMale
woFem 

b[1,] %*% x[,1] + b[2,]*x[,2]+a[1]



p + geom_abline(intercept = a[1], slope = wM[1,1]) 

p + geom_abline(intercept = 0.645, slope = 2.470) 
+ geom_abline(intercept = a[1], slope = b[,2],col = "red") 

lm(CL~RW,data = crabs)


#wo = a 
#wi = b 
# a1-a2(b1-b2)x

#slope<-data.frame(matrix(,ncol = 1))
plot( (a[1] + (t(b) %*% t(x))),(a[1] + (t(b) %*% t(x))) )

p + geom_line(aes(x= slope,y = slope))
  
  geom_abline(intercept = a[1], slope = )  

(t(b) %*% t(x) ~ t(b) %*% t(x)) 

solve(mySig) %*% b 




Males<-as.matrix(subset(crabs,sex == "Male",c(RW,CL)))

ldaF<-cbind(RW,CL) %*% as.matrix(wF) %*% as.matrix(woFem)
ldaM<-cbind(RW,CL) %*% as.matrix(wM) %*% as.matrix(woMale)

solve(mySig) %*% t(myMu[1,2:3] - myMu[2,2:3])

as.matrix(wM) %*% as.matrix(woMale)  
as.matrix(wF) %*% as.matrix(woFem)


library(MASS)
theAns <- lda(sex ~ RW + CL, data = crabs)
table(ifelse(predict(theAns)$x > 0, 1,0))
table(ifelse(ldaM > 0, 1,0))



p + geom_abline(intercept = 18.03, slope = 5)


 (wF%*%woFem) - wM%*%woMale 
(ldaF > ldaM)



lda(ldaF[,1] ~ldaM[,1])


length(ldaM > ldaF) 

t(cbind(RW,CL)) %*% solve(mySig) %*% (myMu[2,2:3] - myMu[1,2:3])

plotdata<- data.frame(ldaF = ldaF,ldaM = ldaM, sex = sex )
colnames(plotdata) <- c("ldaF", "ldaM","sex")




fitted(lm(ldaF~ldaM,data=plotdata))

ggplot(plotdata,aes(x=ldaF,y=ldaM)) + geom_point(aes(col = sex)) +
  geom_abline(intercept = 107.941 , slope = 2.883)

theAns <- lda(sex ~ RW + CL, data = crabs)
predict(theAns)
$prior
