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
#                                                        


library(neuralnet)
set.seed(1234567890)
Var <- runif(50, 0, 10)
trva <- data.frame(Var, Sin=sin(Var))


tr <- trva[1:25,] # Training
va <- trva[26:50,] # Validation
# nn<- neuralnet(Sin ~ Var,data = tr, hidden = 10, startweights = runif(31,-1,1),
#              threshold = 1/1000)
# 
# # pr.nn <- compute(nn,va$Var)
# 
# pr.nn.ngt <- pr.nn$net.result*(max(trva$Sin)-min(trva$Sin))+min(trva$Sin)
# test.r <- (va$Sin)*(max(trva$Sin)-min(trva$Sin))+min(trva$Sin)
# 
# MSE.nn <- sum((test.r - pr.nn.ngt)^2)/nrow(va)
# print(MSE.nn)
# 
# print(paste(MSE.lm,MSE.nn))
# 
# 
# plot(nn)
# prediction(nn,va)
# # Random initializaiton of the weights in the interval [-1, 1]
# winit <- # Your code here
  set.seed(1234567890)
  MSE.nn <-c()
  MSE.nn.train <-c()
  for(i in 1:10) {
    
    nn<- neuralnet(Sin ~ Var,data = tr, hidden = 10, startweights = runif(31,-1,1),
                   threshold = i/1000)
    
    pr.nn <- compute(nn,va$Var)
    
    pr.nn.ngt <- pr.nn$net.result*(max(trva$Sin)-min(trva$Sin))+min(trva$Sin)
    test.r <- (va$Sin)*(max(tr$Sin)-min(tr$Sin))+min(tr$Sin)
    
    #MSE.nn.train[i] <- sum((predict(pr.nn) - tr$Sin)^2)/nrow(tr)
    #MSE.nn[i] <- sum((test.r - pr.nn.ngt)^2)/nrow(va)
    MSE.nn[i] <- sum((va$Sin - pr.nn$net.result)^2)/nrow(va)
    
    print(MSE.nn)
    #if (i > 1 && MSE.nn[i] > MSE.nn[i - 1]) stop("Gradiant descent has decended")
  }
  
  
  
plot(nn <- neuralnet())# Your code here
  # Plot of the predictions (black dots) and the data (red dots)
  plot(prediction(nn)$rep1)
  points(trva, col = "red")
