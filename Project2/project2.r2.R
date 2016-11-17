# call appropriate libraries for the tv.r script
library('dplyr')
library('stringr')
library('pbapply')
library('devtools')

# retrieve tv.r script
source('/Volumes/IMPORTANT/STAT-4181/Project1/proj1-scripts/tv.r')

# First, check if this tv series is included
OUR_TITLE <- "24"

# check if exists
tv.3 <- search_by_title(OUR_TITLE,type="series")
tv.3

# Check if it has ratings
find_by_title(OUR_TITLE, type="episode", season=1, episode=1)$imdbRating

# Run getTv function
twentyfour <-getTv(OUR_TITLE,OUR_YEAR = 2001)

# Retrieve ratings for "24"
x <- twentyfour$x

# print acf and pacf of time series x
par(mfrow=c(1, 2))
acf(x)
pacf(x)

# create matrix to store AIC of ARIMA models
max.order <- 10
AIC.matrix <- list()

# set the maximum 'd' differences of the time series x to be 3
max.d <- 3

# create a loop that fits all 363 models and returns a new matrix for each integrated component 'd'
# this loop takes time series x, extracts the AIC value, and stores the value in a matrix (returns 4 in total)
for(d in 0:max.d){
  AIC.temp.matrix <- matrix(0,nrow = max.order+1,ncol= max.order+1)
  for(i in 1:(max.order+1)){
    for(j in 1:(max.order+1)){
      AIC.temp.matrix[i,j] <- tryCatch(
        {
          #try
          currentArima <- arima(x,order=c(i-1,d,j-1))
          AIC(currentArima)
        },
        error=function(cond){
          errMessage <- paste(i-1,d,j-1,sep=",")
          errMessage <- paste0("Error in fitting ARIMA(",errMessage,"), setting AIC to 10^6")
          message(errMessage)
          return(10^6)
        },
        warning=function(cond){
          errMessage <- paste(i-1,d,j-1,sep=",")
          errMessage <- paste0("Error in fitting ARIMA(",errMessage,"), setting AIC to 10^6")
          message(errMessage)
          return(10^6)
        })
    }
  }
  AIC.matrix[[d+1]] <- AIC.temp.matrix
}

# print each matrix
AIC.matrix[[1]]
AIC.matrix[[2]]
AIC.matrix[[3]]
AIC.matrix[[4]]

# to aid in finding which model parameters return the lowest AIC value, use the levelplot function
par(mfrow=c(2,2))
levelplot(AIC.matrix[[1]])
levelplot(AIC.matrix[[2]]) 
levelplot(AIC.matrix[[3]])
levelplot(AIC.matrix[[4]])

## the 3 best model fits return AIC scores of 16.10, 16.39, and 16.90 respectively ##

# create new 'best.model' vectors, which will store the arima values of the manually entered fitted orders
best.model <- arima(x,order=c(2,1,3))
best.model2 <- arima(x,order=c(0,1,2))
best.model3 <- arima(x,order=c(2,1,4))

# print acf and pacf of the best model with the lowest AIC
par(mfrow=c(1, 2))
acf(best.model$resid)
pacf(best.model$resid)

# print acf and pacf of the second best model with the lowest AIC
par(mfrow=c(1, 2))
acf(best.model2$resid)
pacf(best.model2$resid)

# print acf and pacf of the third best model with the lowest AIC
par(mfrow=c(1, 2))
acf(best.model3$resid)
pacf(best.model3$resid)
