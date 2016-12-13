#   ___      _______  _______   ___                      
#  |   |    |   _   ||  _    | |   |                     
#  |   |    |  |_|  || |_|   | |   |___                  
#  |   |    |       ||       | |    _  |                 
#  |   |___ |       ||  _   |  |   | | |                 
#  |       ||   _   || |_|   | |   |_| |                 
#  |_______||__| |__||_______| |_______|                 
#   _______  _______  _______  _______  _______  _______ 
#  |       ||       ||       ||   _   ||  _    ||       |
#  |___    ||___    ||____   ||  |_|  || | |   ||   ____|
#      |   | ___|   | ____|  ||       || |_|   ||  |____ 
#      |   ||___    || ______||       ||___    ||_____  |
#      |   | ___|   || |_____ |   _   |    |   | _____| |
#      |___||_______||_______||__| |__|    |___||_______|
#                                                        
#  V.2                                                      


library(neuralnet)
set.seed(1234567890)
Var <- runif(50, 0, 10)
trva <- data.frame(Var, Sin=sin(Var))


tr <- trva[1:25,] # Training
va <- trva[26:50,] # Validation

# # Random initializaiton of the weights in the interval [-1, 1]
winit <- runif(31,-1,1)

MSE.nn <-c()
#MSE.nn.train <-c()
for(i in 1:10) {
  
  nn<- neuralnet(Sin ~ Var,data = tr, hidden = 10, startweights = winit,
                 threshold = i/1000)
  
  pr.nn <- compute(nn,va$Var)
  pr.nn.tr <- compute(nn,tr$Var)
  
  # MSE.nn.train[i] <- sum((tr$Sin - pr.nn.tr$net.result)^2) / nrow(tr)
  MSE.nn[i] <- sum((va$Sin - pr.nn$net.result)^2) /nrow(va)
  
  if (i > 1 && MSE.nn[i] > MSE.nn[i - 1]) break("Gradiant descent has decended")
}


nn <- neuralnet(Sin ~ Var,data = tr, hidden = 10, startweights = winit,
                threshold = 4/1000)
plot(nn)
# Your code here
# Plot of the predictions (black dots) and the data (red dots)
x <- prediction(nn)$rep1[,1] 
y<-prediction(nn)$rep1[,2]
plot(x,y)
points(trva, col = "red")
