#############################
####### Computer Lab 1 ######
######### Block 2 ###########
########  732A95  ###########


## 2.1 

Infu<-read.csv2("B2lab1/Influenza.csv")
attach(Infu)
library(gridExtra)

p<- ggplot(data = Infu, aes(x = Time)) 
aM<- p + geom_line(aes(y = Mortality)) 
aI<- p + geom_line(aes(y = Influenza))
#p + geom_line(aes(y = Mortality)) + geom_line(aes(y = Influenza))
plot(arrangeGrob(aM,aI))


## 2.2
library(mgcv)
addM <- gam(Mortality ~ Year + s(Week),data = Infu )

# E(mort|x) = N(Year,sigma) + spline-antaganden(week)

## 2.3 

summary(addM)
aM + geom_line(aes(y = fitted(addM)),col = "red") 
plot(addM)  ## ASK ABOUT SPLINE-PLOT
addM$smooth

## 2.4 


for (spval in c(-100,1,50,10000)){
  plot(aM + geom_line(aes(y = fitted(gam(Mortality ~ Year + s(Week, sp = spval),data = Infu))),col = "red") +
   labs(title = paste("Fits when lambda is:",spval) ))
}

# plotlist <- list()
# i <- 1
# for (spval in c(-100,1,50,10000)){
#   #foraddM<-gam(Mortality ~ Year + s(Week),data = Infu, sp = spval)
#   plotlist[[i]] <- ggplot(data = Infu, aes(x = Time)) +
#     geom_line(aes(y = Mortality)) +
#     geom_line(aes(y = fitted(gam(Mortality ~ Year +
#                                    s(Week, sp = spval),data = Infu))),col = "red") +
#          labs(title = paste("Fits when lambda is:",spval) )
#   i <- i + 1
# }
# 
# library(gridExtra)
# grid.arrange(plotlist,ncol =2)
# plot(plotlist)
# do.call(grid.arrange, c(plotlist, list(ncol =2) ))

## 2.5 

aI + geom_line(aes(y=resid(addM),x = Time),col = "lightblue")


## 2.6

add26<-gam(Mortality ~ s(Week) + s(Year, k = 9) + s(Influenza) ,data = Infu)
summary(add26)
aM + geom_line(aes(y =fitted(add26)), col ="red")
 