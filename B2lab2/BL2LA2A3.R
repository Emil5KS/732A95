# # # # #   Block 2    # # # # # 
# # # # #    Lab 2     # # # # # 
# # # # # Assignemnt 3 # # # # #

library(mboost)

# Assignment 3 

## 3.1 

BFR <- read.csv2("B2lab2/bodyfatregression.csv")
m <- blackboost(Bodyfat_percent ~ Waist_cm + Weight_kg, data = BFR)
mstop(m)
cvf <- cv(model.weights(m), type = "kfold")
cvm <- cvrisk(m, folds = cvf, grid = 1:100)
plot(cvm)

#ser bra ut


## 3.2 

m2 <- blackboost(Bodyfat_percent ~ Waist_cm + Weight_kg, data = train, 
                 control=boost_control(mstop=mstop(cvm)))
#control=boost_control(mstop=mstop(cvm))
mstop(m2)
cvf2 <- cv(model.weights(m2), type = "kfold")
cvm2 <- cvrisk(m2, folds = cvf2, grid = 1:100)
plot(cvm2)

m2.sd <- mean(predict(m2,test) - test$Bodyfat_percent)

summary(m2)
