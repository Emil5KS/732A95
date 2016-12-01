###################
# #    732A95   # #  
# # # # # # # # # #
# Assignment 1 #    
# # # # # # # # # # 
# #    732A95   # #   
# #             # #
###################

# Assignment 2 

# 2.1 




# myPCA <- function(X, stand = TRUE){
# 
#   if (stand == TRUE) X <- scale(X)
# 
# cov<-cov(X)#kovarianserna av de standardiserade pendelvariablerna
# #sum(diag(DDcov))
# values <-eigen(cov)$values
# vectors<-eigen(cov)$vector#Tar ut första kol ur matrisen med egenvektorer.
# colnames(vectors) <- paste0("PC",1:ncol(vectors))
# return(list(values = values , vectors = vectors))
# }
# 
# plot(Egen,type='l')
# pca.nir<-myPCA(NIR)
# pca.varexplaind<- pca.nir$values/sum(pca.nir$values)


NIR<-read.csv2("lab4/NIRspectra.csv")

pca.nir <- prcomp(NIR[,-127], scale = TRUE) 
 
pca.varexplaind<- pca.nir$sdev^2/sum(pca.nir$sdev^2) 

pca.eigens <-  pca.nir$sdev^2

ggplot() + geom_line(aes(x = 1:length(pca.varexplaind), y = pca.varexplaind))  + 
  labs(title="Scree plot for PCA",x="Principal Component", y = "% variance explained")

#There is a quick drop off in the number of PCA-components  that explain the variance.
#The classic elbow shape is a good way to determine which should be kept, in this case
#only PC1 is needed. But to get to 99 % of the variance explained the PC2 is also selected.

#A closer look at the drop of for the 4 first PCA components. Here the elbow shape is
# more visible as we have less variables. 
ggplot() + geom_line(aes(x = 1:4, y = pca.varexplaind[1:4])) + 
  labs(title="Scree plot for PCA zoomed in",x="Principal Component", y = "% variance explained")
#

eigenvectors <- as.data.frame(pca.nir$rotation[,1:2])
colnames(eigenvectors) <- paste0("pca",1:2)

#component <- data.frame(pca1 = matrix(pca.nir$vectors[1,],ncol = 1), 
 #                       pca2 = matrix(pca.nir$vectors[2,],ncol = 1))

scores<-data.frame(pca.nir$x[,1:2])
scores$PC1
ggplot(data = scores) + geom_point(aes(x = PC1, y = PC2))  + 
  labs(title="Score plot for PCA",x="PC1", y = "PC2")


# ggplot(data = eigenvectors) + geom_point(aes(x = pca1, y = pca2), col = "darkblue") + 
#   labs(x= "PCA 1", y = "PCA 2", title = "")

## 2.2 

pP<-ggplot(data = eigenvectors) + geom_point(aes(x = 1:126, y = pca1), col = "firebrick") + 
  labs(x= "index", y = "PCA 1", title = "")

pP2<-ggplot(data = eigenvectors) + geom_point(aes(x = 1:126, y = pca2), col = "darkgreen") + 
  labs(x= "index", y = "PCA 2", title = "")

gRid<-grid.arrange(pP,pP2,ncol = 2, top ="PCA Trace plots ")
grid.newpage()
grid.draw(gRid)
library(grid)
## 2.3 

library(fastICA)
library(gridExtra)

set.seed(12345)
slowCoop<- fastICA(scale(NIR[,-127]), n.comp = 2, alg.typ = "parallel", fun = "logcosh") 
        
Wprim <- data.frame(slowCoop$K %*% slowCoop$W)
slowCoop$W


pX1 <- ggplot(data = Wprim) + geom_point(aes(x = 1:126, y = X1), col = "firebrick") + 
  labs(x= "index", y = "X1", title = "")

pX2 <- ggplot(data = Wprim) + geom_point(aes(x = 1:126, y = X2), col = "darkgreen") + 
  labs(x= "index", y = "X2", title = "")

gRid<-grid.arrange(pX1,pX2,ncol = 2, top ="ICA Trace plots ")
grid.newpage()
grid.draw(gRid)


S <- data.frame(slowCoop$S[,1:2])
ggplot(data = S) + geom_point(aes(x = X1 , y = X2)) + 
  labs(title="Score plot for ICA")


## 2.4 

library(pls)
set.seed(12345)
mypcr <- pcr(Viscosity~.,data = NIR, scale = TRUE)
set.seed(12345)
cv.mypcr<- crossval(mypcr)

str(mypcr)


plot(MSEP(cv.mypcr))

cvMSEP<- MSEP(cv.mypcr)

msep<-data.frame(t(matrix(cvMSEP$val,nrow = 2)))
msep$nocomp <- cvMSEP$comps
colnames(msep) <- c("CV","adjCV","nocomp")


library(reshape2)
smelt<-melt(msep,id = "nocomp")

ggplot(smelt,aes(x = nocomp, y = value, col=variable)) + geom_line() +
  labs(title="Mean squared error prediction",x="Number of components", y = "MSEP")

sapply(X =1:5, FUN = function (x) (x)^2)
x <- 3
