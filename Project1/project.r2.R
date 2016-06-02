#####call function for three different TV shows

###TV 1

source("/Users/peterking/Desktop/STAT-4181/Project/tv.R")

#First, check if this tv series is included
OUR_TITLE <- "The Sopranos"

#check if exists
tv.1 <- search_by_title(OUR_TITLE,type="series")
tv.1

#Check if it has ratings
find_by_title(OUR_TITLE, type="episode", season=1, episode=1)$imdbRating

#Run function
sopranos <-getTv(OUR_TITLE,OUR_YEAR = 1999)
#Retrieve ratings
x <- sopranos$x

#coerce to ts object
x <- as.ts(x)
ts.plot(x)
#print acf and pacf of time series x
acf(x,lag=60)
pacf(x, lag=20)

##FIND BEST MODEL
#create matrix to store AIC of models
max.order <- 10
AIC.matrix <- matrix(0,nrow = max.order+1,ncol= max.order+1)

#create loop to store AIC of AR/MA/ARMA models with specified parameters
for(i in 1:(max.order+1)){
  for(j in 1:(max.order+1)){
    currentArima <- arima(x,order=c(i-1,0,j-1))
    AIC.matrix[i,j] <- AIC(currentArima) 
  }
}

#create loop to find best AIC fit
BEST_I <- 0
LOWEST_AIC <- 10^10
for(i in 1:(max.order+1)){
  for(j in 1:(max.order+1)){
    if(AIC.matrix[i,j] < LOWEST_AIC){
      LOWEST_AIC <- AIC.matrix[i,j]
      best.model <- arima(x,order=c(i,0,j))
    }
  }
}

#print acf and pacf of this model
acf(residuals(best.model))
pacf(residuals(best.model))

#plot lattice
library(lattice)
levelplot(AIC.matrix)
  

###TV 2

#First, check if this tv series is included
OUR_TITLE <- "Boardwalk Empire"

#check if exists
tv.2 <- search_by_title(OUR_TITLE,type="series")
tv.2

#Check if it has ratings
find_by_title(OUR_TITLE, type="episode", season=1, episode=1)$imdbRating

#Run function
boardwalk <-getTv(OUR_TITLE,OUR_YEAR = 2010)
#Retrieve ratings
x <- boardwalk$x

#coerce to ts object
x <- as.ts(x)
ts.plot(x)
#print acf and pacf of time series x
acf(x,lag=60)
pacf(x)

##FIND BEST MODEL
#create matrix to store AIC of models
max.order <- 10
AIC.matrix <- matrix(0,nrow = max.order+1,ncol= max.order+1)

#create loop to store AIC of AR/MA/ARMA models with specified parameters
for(i in 1:(max.order+1)){
  for(j in 1:(max.order+1)){
    currentArima <- arima(x,order=c(i-1,0,j-1))
    AIC.matrix[i,j] <- AIC(currentArima) 
  }
}

#create loop to find best AIC fit
BEST_I <- 0
LOWEST_AIC <- 10^10
for(i in 1:(max.order+1)){
  for(j in 1:(max.order+1)){
    if(AIC.matrix[i,j] < LOWEST_AIC){
      LOWEST_AIC <- AIC.matrix[i,j]
      best.model <- arima(x,order=c(i,0,j))
    }
  }
}
  
#print acf and pacf of this model
acf(best.model)
pacf(best.model)

levelplot(AIC.matrix)
AIC.matrix
###TV 3

#First, check if this tv series is included
OUR_TITLE <- "24"

#check if exists
tv.3 <- search_by_title(OUR_TITLE,type="series")
tv.3

#Check if it has ratings
find_by_title(OUR_TITLE, type="episode", season=1, episode=1)$imdbRating

#Run function
twentyfour <-getTv(OUR_TITLE,OUR_YEAR = 2001)
#Retrieve ratings
x <- twentyfour$x

#coerce to ts object
x <- as.ts(x)
ts.plot(x)
#print acf and pacf of time series x
acf(x,lag=60)
pacf(x)

##FIND BEST MODEL
#create matrix to store AIC of models
max.order <- 10
AIC.matrix <- matrix(0,nrow = max.order+1,ncol= max.order+1)

#create loop to store AIC of AR/MA/ARMA models with specified parameters
for(i in 1:(max.order+1)){
  for(j in 1:(max.order+1)){
    currentArima <- arima(x,order=c(i-1,0,j-1))
    AIC.matrix[i,j] <- AIC(currentArima) 
  }
}

#create loop to find best AIC fit
BEST_I <- 0
LOWEST_AIC <- 10^10
for(i in 1:(max.order+1)){
  for(j in 1:(max.order+1)){
    if(AIC.matrix[i,j] < LOWEST_AIC){
      LOWEST_AIC <- AIC.matrix[i,j]
      best.model <- arima(x,order=c(i,0,j))
    }
  }
}
  
#print acf and pacf of this model
acf(best.model)
pacf(best.model)
  
levelplot(AIC.matrix)
  
  
  
  
  




