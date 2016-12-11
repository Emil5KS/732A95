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

train  <- data[1:45, ]
test   <- data[46:64, ]

library(pamr)





data0=read.csv2("voice.csv")
data=data0
data=as.data.frame(scale(data))
data$Quality=as.factor(data0$Quality)

rownames(data)=1:nrow(data)
x=t(data[,-311])
y=data[[311]]
mydata=list(x=x,y=as.factor(y),geneid=as.character(1:nrow(x)), genenames=rownames(x))
model=pamr.train(mydata,threshold=seq(0,4, 0.1))
pamr.plotcen(model, mydata, threshold=1)
pamr.plotcen(model, mydata, threshold=2.5)

a=pamr.listgenes(model,mydata,threshold=2.5)
cat( paste( colnames(data)[as.numeric(a[,1])], collapse='\n' ) )

cvmodel=pamr.cv(model,mydata)
print(cvmodel)
pamr.plotcv(cvmodel)


  



