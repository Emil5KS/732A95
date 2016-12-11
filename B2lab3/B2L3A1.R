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
# Fuck yeah



data <- read.csv2("B2lab3/data.csv", encoding = "latin1")
data <- data[sample(nrow(data)),]
data$Conference <- as.factor(data$Conference)


train  <- data[1:45, ]
test   <- data[46:64, ]

library(pamr)

y <- as.factor(train$Conference)
x <- t(train[-which(colnames(data) == "Conference")])

TRAIN <- list(x = x, y = y,geneid = as.character( 1:nrow(x) ), genenames = rownames(x) ) 

model <- pamr.train(TRAIN, threshold = seq(0,4, 0.1)) 

pamr.plotcen(model, TRAIN, threshold=1)
pamr.plotcen(model, TRAIN, threshold=2.5)

firstmodel <-pamr.listgenes(model,TRAIN,threshold=2.5)
cat( paste( colnames(data)[as.numeric(firstmodel[,1])], collapse='\n' ) )

cvmodel=pamr.cv(model,TRAIN)
print(cvmodel) #13 1.2        314    4 ,  mao 314 variabler vill vi ha
pamr.plotcv(cvmodel)

modcv <- pamr.train(TRAIN, threshold = 1.2) 

pamr.plotcen(modcv, TRAIN, threshold=1.2)
pamr.plotcen(modcv, TRAIN, threshold=0.9)

crossed<-pamr.listgenes(modcv,TRAIN,threshold=1.2)
cat( paste( colnames(data)[as.numeric(crossed[,1])], collapse='\n' ) )



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

elasticNet<- cv.glmnet(x = as.matrix(train[,-which(colnames(train) == "Conference")]), y = train$Conference, 
       family = "binomial", alpha = 0.5, lambda = seq(0.1,10,0.1))
  
plot(elasticNet) 
elsNet<- predict(elasticNet, newx = as.matrix(test[,-which(colnames(test) == "Conference")] ))
table(ifelse(elsNet > 0, 1,0),test$Conference)
train[1,1:5]



library(kernlab)
K   <- as.kernelMatrix(crossprod(t(as.matrix(train[,-which(colnames(train) == "Conference")]) )))
res <- kpca(K, kernel = "vanilladot")
barplot(res@eig)
plot(res@rotated[,1], res@rotated[,2], xlab="PC1", ylab="PC2")

predict(res,as.matrix(test[,-4703]))
