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



h_distance <- # These three values are up to the students
  h_date <-
  h_time <-
  a <- 58.4274 # The point to predict (up to the students)
b <- 14.826
date <- "2013-11-04" # The date to predict (up to the students)
times <- c("04:00:00", "06:00:00", ..., "24:00:00")
temp <- vector(length=length(times))
# Students code here
plot(temp, type="o")


gk <- function(x, xi){
  
  if( class(x) == "Date"){
    xi <- as.Date(factor(xi),format = "%Y-%m-%d")
    return(exp(-((abs( as.numeric(x - xi) )^2) / (2*sd(x)^2)))) 
  }
  
  return(exp(-((abs( (x - xi))^2) / (2*sd(x)^2)))) 
}



# Den första kerneln 
plot(y = st$latitude,x = st$longitude)

st[st$station_number == "93220",]
which(st$station_number == "93220")

Karlstad<-c(st$latitude[19203],st$longitude[19203])

dmat <- distHaversine(p1 = cbind(st$latitude,st$longitude) , p2 = Karlstad)
#gk(dmat,0)

# sapplyen nedan gör substr(st$time,1,2) borde bara vara att slänga in skiten nu
standardDate
timevec <- sapply(sapply(data$time,substr,start = 1, stop = 2),as.numeric)

# 



my_magic_kernel <- function(data ,time, date, longlat = c(59.4446, 13.3374) ){ 
  
  gk <- function(x, xi){
    #for the days
    if( all(class(x) == "Date")) {
      
      xi <- as.Date(factor(xi),format = "%Y-%m-%d")
      return(exp(-((abs( as.numeric(x - xi) )^2) / (2*sd(x)^2)))) 
    }
    
    #For the hours
    if(all(class(x)  %in% c("POSIXlt","POSIXt")) ) {
      
      xi <- strptime(xi,"%H:%M:%S")
      return(exp(-((abs( as.numeric(x - xi) )^2) / (2*sd(x)^2)))) 
    }  
    
    #For long and lat
    return(exp(-((abs( (x - xi))^2) / (2*sd(x)^2)))) 
  }  
  
#Longitude and Latitude distances.  
dmat <- geosphere::distHaversine(p1 = cbind(data$latitude,data$longitude) , p2 = longlat)
gkdmat<- gk(dmat,0)

#timme
timevec <- strptime(data$time,format = "%H:%M:%S")
gktime<-gk(timevec,time)
#datum
datevec <- as.Date(st$date)
gkdate<-gk(datevec,date)


alltemps <- cbind(gkdmat,gktime,gktime)*data$air_temperature
return(alltemps) 
  
}

as<-my_magic_kernel(data = st ,time ="01:00:00", date = "2016-03-04", longlat = c(59.4446, 13.3374) )
debugonce(my_magic_kernel)


strptime(st$time[1],"%H:%M:%S")
as.Date(st$time[1]) - as.Date(st$time[2])

## Tidexperiment 

datum2<-as.Date(factor(st$date[2]),format = "%Y-%m-%d")
datum - datum2
typeof(datum)
strftime(datum, format = "", tz = "", usetz = FALSE)
strptime(datum, format = "")

as.POSIXt(datum2)
strftime("2004-05-28")
