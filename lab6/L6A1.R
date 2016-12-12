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
neuralnet(Sin ~ Var,data = tr, hidden = 10, startweights = runif(nrow(tr)*10+10,-1,1) )

# Random initializaiton of the weights in the interval [-1, 1]
winit <- # Your code here
  for(i in 1:10) {
    nn <- neuralnet() # Your code here
      # Your code here
  }
plot(nn <- neuralnet())# Your code here
  # Plot of the predictions (black dots) and the data (red dots)
  plot(prediction(nn)$rep1)
  points(trva, col = "red")
