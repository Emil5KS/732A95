######  #    #    #####   #  #
#       #    #   #        # # 
####    #    #   #        ##
#       #    #   #        # #
#        ####     #####   #  # 
#windows 1252 for åäö

set.seed(1234567890)
library(geosphere)

stations <- read.csv("lab5/stations.csv",header = TRUE)
temps <- read.csv("lab5/temps50k.csv")
st <- merge(stations,temps,by="station_number")
write.csv(st,"st1.csv")

# (u'52230', (u'55,3836', u'12,8203', u'"2004-05-28"', u'"12:00:00"', u'12'))
h_distance <- # These three values are up to the students
  h_date <-
  h_time <-

a <- 58.4274 # The point to predict (up to the students)
b <- 14.826
date <- "2013-11-04" # The date to predict (up to the students)
times <- c(paste0("0",seq(2,9,2),":00:00"),paste0(seq(10,24,2),":00:00"))
temp <- vector(length=length(times))
# Students code here
plot(temp, type="o")


my_magic_kernel <- function(data ,time, date, longlat = c(59.4446, 13.3374), h_days = 6, h_time = 4, h_distance = 100000){ 
 
### Defining the kernel
  
gk <- function(x, xi){
  #for the days
  if( all(class(x) == "Date")) {
    
    xi <- as.Date(factor(xi),format = "%Y-%m-%d")
    return(exp(-((abs( as.numeric(x - xi) )^2) / (h_days) ))) 
  }
  
  #For the hours
  if(class(x)  == c("difftime") ) {
    
    xi <- strptime(xi,"%H:%M:%S")
    return(exp(-((abs( as.numeric(x) )^2) / (h_time)))) 
  }  
  
  #For long and lat
  return(exp(-((abs( (x - xi))^2) / (h_distance)))) 
}  
  
  
  #Initiatin objects for loop.
  predictions <- data.frame(time=1,temp=1)
  i <- 1 
  
for (timme in times){ 
    
  
      mdate = strftime(paste(date,timme))
      data<-subset(st,  strftime(paste(st$date,st$time)) < mdate)
        
      #Longitude and Latitude distances.  
      dmat <- geosphere::distHaversine(p1 = cbind(data$latitude,data$longitude) , p2 = longlat)
      gkdmat<- gk(dmat,0)
      
      #datum
      datevec <- as.Date(st$date)
      gkdate<-gk(datevec,date)
      
      
      #timme
      
      difftimes<-difftime(strptime(data$time,format="%H:%M:%S"),strptime(timme,format = "%H:%M:%S"),units = "hours")      #timevec <- strptime(data$time,format = "%H:%M:%S")
      gktime<-gk(difftimes,0)
      
      alltemps <- rowSums(cbind(gkdmat,gktime,gkdate)*data$air_temperature)/sum((gkdmat + gkdate + gktime)) 
      predictions[i,] <- c(timme,sum(alltemps))
      i <- i + 1 

}
  
predictions[,1]<- as.factor(predictions[,1])
predictions[,2]<- as.numeric(predictions[,2])
return(predictions) 
  
}

as<-my_magic_kernel(data = st ,time = times, date = "2016-12-24",longlat = c(a,b), h_days = 7, h_time = 2, h_distance = 100000)


sum(rowSums(as))
mean(colSums(as))
debugonce(my_magic_kernel)


as[,1] <- as.factor(as[,1])
as[,2] <- as.numeric(as[,2])
ggplot(data = as, aes(x= time,y=mean))+geom_point()
plot(as[,1],as[,2],type = "o")

# gk <- function(x, xi){
#   
#   if( class(x) == "Date"){
#     xi <- as.Date(factor(xi),format = "%Y-%m-%d")
#     return(exp(-((abs( as.numeric(x - xi) )^2) / (2*sd(x)^2)))) 
#   }
#   
#   return(exp(-((abs( (x - xi))^2) / (2*sd(x)^2)))) 
# }
# 
# 
# 
# # Den första kerneln 
# plot(y = st$latitude,x = st$longitude)
# 
# st[st$station_number == "93220",]
# which(st$station_number == "93220")
# 
# Karlstad<-c(st$latitude[19203],st$longitude[19203])
# 
# dmat <- distHaversine(p1 = cbind(st$latitude,st$longitude) , p2 = Karlstad)
# #gk(dmat,0)
# 
# # sapplyen nedan gör substr(st$time,1,2) borde bara vara att slänga in skiten nu
# standardDate
# timevec <- sapply(sapply(data$time,substr,start = 1, stop = 2),as.numeric)
# 
# 
# 
# 
# strptime(st$time[1],"%H:%M:%S")
# as.Date(st$time[1]) - as.Date(st$time[2])
# 
# ## Tidexperiment 
# 
# datum2<-as.Date(factor(st$date[2]),format = "%Y-%m-%d")
# datum - datum2
# typeof(datum)
# strftime(datum, format = "", tz = "", usetz = FALSE)
# strptime(datum, format = "")
# 
# as.POSIXt(datum2)
# strftime("2004-05-28")  
# 
# paste
# substr("1943-09-06",start=1,stop =9)
# 
# 
# mdate = "1943-09-06"
# mdate = as.Date(mdate) -1

### Att göra
#Hitta en filter funktion som filtrerar ut rätt datum, ie en över det datum vi vill ha.
#Rätta till kernelsummorna. Det ska summera alla kernelvikter och dela på y*alla kernels

## I block 2 får jag fixa min Log likelihood och min CV eventuellt.

# 
# strftime("1943-09-06") < as.Date("1943-09-06") 
# 
# 
# 
# it <- 2



strptime(st$time,format="%H:%M:%S") - strptime("01:00:00",format = "%H:%M:%S")

