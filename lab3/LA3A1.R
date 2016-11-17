################################################
############## Computer Lab 3 ##################
# # # # Introduction to Machine Learning # # # #
#############  Assignment 1   ##################
################################################

# 1.1 

setwd("/Users/EmilsHem/Documents/732A95/lab3")
crabs <- read.csv("australian-crabs.csv")

p<- ggplot(data = crabs) + geom_point(aes(x = RW, y =CL, col = sex))
plot(p)

# 1.2 

LDA<- function(){}
attach(crabs)

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


ldaF<-cbind(RW,CL) %*% as.matrix(wF) %*% as.matrix(woFem)
ldaM<-cbind(RW,CL) %*% as.matrix(wM) %*% as.matrix(woMale)


theAns <- lda(sex ~ RW + CL, data = crabs)
predict(theAns)

############ Skit nedan
#
#
#

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
