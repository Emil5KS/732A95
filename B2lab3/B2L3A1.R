#    /$$$$$$$  /$$                     /$$              /$$$$$$       
#   | $$__  $$| $$                    | $$             /$$__  $$      
#   | $$  \ $$| $$  /$$$$$$   /$$$$$$$| $$   /$$      |__/  \ $$      
#   | $$$$$$$ | $$ /$$__  $$ /$$_____/| $$  /$$/        /$$$$$$/      
#   | $$__  $$| $$| $$  \ $$| $$      | $$$$$$/        /$$____/       
#   | $$  \ $$| $$| $$  | $$| $$      | $$_  $$       | $$            
#   | $$$$$$$/| $$|  $$$$$$/|  $$$$$$$| $$ \  $$      | $$$$$$$$      
#   |_______/ |__/ \______/  \_______/|__/  \__/      |________/      
#   
#   
#   
#          /$$        /$$$$$$  /$$$$$$$         /$$$$$$                     
#         | $$       /$$__  $$| $$__  $$       /$$__  $$
#         | $$      | $$  \ $$| $$  \ $$      |__/  \ $$
#         | $$      | $$$$$$$$| $$$$$$$          /$$$$$/
#         | $$      | $$__  $$| $$__  $$        |___  $$
#         | $$      | $$  | $$| $$  \ $$       /$$  \ $$
#         | $$$$$$$$| $$  | $$| $$$$$$$/      |  $$$$$$/
#         |________/|__/  |__/|_______/        \______/
#
# (Fuck yeah)


library(pamr)

data <- read.csv2("B2lab3/data.csv", encoding = "latin1")
data <- data[sample(nrow(data)),]
data$Conference <- as.factor(data$Conference)

train  <- data[1:45, ]
test   <- data[46:64, ]

y <- as.factor(train$Conference)
x <- t(train[-which(colnames(data) == "Conference")])

TRAIN <- list(x = x, y = y,geneid = as.character( 1:nrow(x) ), genenames = rownames(x) ) 

y1 <- as.factor(test$Conference)
x1 <- t(test[-which(colnames(test) == "Conference")])

TEST <- list(x = x1, y = y1,geneid = as.character( 1:nrow(x1) ), genenames = rownames(x1) ) 



# 
# model <- pamr.train(TRAIN, threshold = seq(0,4, 0.1)) 
# 
# pamr.plotcen(model, TRAIN, threshold=1)
# pamr.plotcen(model, TRAIN, threshold=2.5)
# 
# firstmodel <-pamr.listgenes(model,TRAIN,threshold=2.5)
# cat( paste( colnames(data)[as.numeric(firstmodel[,1])], collapse='\n' ) )

cvmodel=pamr.cv(model,TRAIN)
print(cvmodel) #13 1.2        314    4 ,  mao 314 variabler vill vi ha
pamr.plotcv(cvmodel)

modcv <- pamr.train(TRAIN, threshold = 1.2) 

pamr.plotcen(modcv, TRAIN, threshold=1.2)
pamr.plotcen(modcv, TRAIN, threshold=0.9)

crossed<-pamr.listgenes(modcv,TRAIN,threshold=1.2)
cat( paste( colnames(data)[as.numeric(crossed[,1])], collapse='\n' ) )

table(pamr.predict(modcv,TEST$x,threshold = 1.2),TEST$y)


# 
# data0=read.csv2("voice.csv") 
# data=data0 
# data=as.data.frame(scale(data)) 
# data$Quality=as.factor(data0$Quality) 
# 
# rownames(data)=1:nrow(data) 
# x=t(data[,-311])  
# y=data[[311]] 
# mydata=list(x=x,y=as.factor(y), geneid=as.character(1:nrow(x)), genenames=rownames(x))
# model=pamr.train(mydata,threshold=seq(0,4, 0.1))
# pamr.plotcen(model, mydata, threshold=1)
# pamr.plotcen(model, mydata, threshold=2.5)
# 
# a=pamr.listgenes(model,mydata,threshold=2.5)
# cat( paste( colnames(data)[as.numeric(a[,1])], collapse='\n' ) )
# 
# cvmodel=pamr.cv(model,mydata)
# print(cvmodel)
# pamr.plotcv(cvmodel)



## 1.2 


library(glmnet)  
library(reshape2)   

elasticNet<- cv.glmnet(x = as.matrix(train[,-which(colnames(train) == "Conference")]), y = train$Conference, 
       family = "binomial", alpha = 0.5)    
mycoeffs <- coefficients(elasticNet)[-1,] 
cat(paste0(names(mycoeffs[mycoeffs != 0]),"\n")) 

plot(elasticNet)    
 
colnames(mycoeffs) <- elasticNet$lambda  

myElnet<-melt(as.matrix(mycoeffs),id = rownames,c("lambda","coef")) 

colnames(myElnet) <- c("features","lambda","coef") 

ggplot(myElnet,aes(x=log(lambda),y = coef,color = features)) +
  geom_line(show.legend = FALSE) + labs(x = expression(log(lambda)) )


elsNet<- predict(elasticNet, s = elasticNet$lambda.min, newx = as.matrix(test[,-which(colnames(test) == "Conference")] )) 
table(ifelse(elsNet > 0, 1,0),test$Conference)
train[1,1:5]



library(kernlab)
K   <- as.kernelMatrix(crossprod(t(as.matrix(train[,-which(colnames(train) == "Conference")]) )))
res <- kpca(K, kernel = "vanilladot")
barplot(res@eig)
plot(res@rotated[,1], res@rotated[,2], xlab="PC1", ylab="PC2")
res@
predict(res,as.matrix(test[,-4703]))
#Funkar inte.
ksvm() # använd den här istället
filter <- ksvm(Conference~.,data=train,kernel="vanilladot")
colnames(train)[filter@alphaindex[[1]]]

table(predict(filter,test[,-ncol(test)]),test$Conference) 


Conference.t.test<- as.numeric(as.character(data$Conference)) 

data.t.test <- t(apply(data,1,FUN = function(x) as.numeric(as.character(x)))) 
data.t.test <- as.data.frame(data.t.test) 
colnames(data.t.test) <- colnames(data) 


pvalues<-matrix(ncol = 2, nrow = ncol(data))
for (i in 1:ncol(data)){ 
  pvalues[i,]<- c(colnames(data.t.test)[i] ,
                 t.test(data.t.test[,i]~Conference.t.test ,alternative = "two.sided" )$p.value) 
}
pvalues<-as.data.frame(pvalues)
colnames(pvalues) <- c("feature","pvalue")
pvalues$pvalue <- as.numeric(as.character(pvalues$pvalue))

#Skit som inte funkar. 
#apply(data.t.test[-ncol(data.t.test)],1,FUN = function(x) t.test(x ~ Conference.t.test)$p.value )


#t.test(as.numeric(as.character(Conference)) ~ aalborg,data = data )

# getAnywhere(p.adjust())
# hochberg = {
#   i <- lp:1L
#   o <- order(p, decreasing = TRUE)
#   ro <- order(o)
#   pmin(1, cummin((n - i + 1L) * p[o]))[ro]
# }

# BH = {
#   i <- lp:1L
#   o <- order(p, decreasing = TRUE)
#   ro <- order(o)
#   pmin(1, cummin(n/i * p[o]))[ro]
# }
j <- 1:ncol(data)-1
alph <- 0.05
pvalues$feature <- as.factor(pvalues$feature)
pvalues <- pvalues[order(pvalues$pvalue),]
pvalues$reject <- 1:nrow(pvalues)

# for (i in 1:nrow(pvalues)){
#   pvalues$reject[i]<-(ifelse(alph*(i/nrow(pvalues)) > pvalues$pvalue[i] , 0,1))
#   
# }
pvalues$reject<-(ifelse(alph*(1:nrow(pvalues)/nrow(pvalues)) > pvalues$pvalue, 0,1))

nrow(pvalues[pvalues$reject == 0,]) - 1


ggplot(data = pvalues[1:4702,], aes(x = 1:4702,y=pvalue, col = reject)) + geom_point()


cummin(c(3:1, 2:0, 4:2))
