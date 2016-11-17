# call the lattice library for the levelplot function
library(lattice)

# read in Canada datframe
data("Canada")
x <- as.data.frame(Canada)

# print acf and pacf of time series x
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
levelplot(AIC.matrix)

# create new 'best.model' vectors, which will store the arima values of the manually entered fitted orders
best.model <- arima(x,order=c())
best.model2 <- arima(x,order=c())
best.model3 <- arima(x,order=c())

# print acf and pacf of the best model with the lowest AIC
acf(best.model$resid)
pacf(best.model$resid)

