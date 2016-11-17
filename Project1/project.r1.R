# print acf and pacf of time series x
acf(x)
pacf(x)

# create matrix to store AIC of ARMA models
max.order <- 10
AIC.matrix <- matrix(0,nrow = max.order+1,ncol= max.order+1)

# create loop to store AIC of ARMA models with specified parameters
for(i in 1:(max.order+1)){
  for(j in 1:(max.order+1)){
    currentArima <- arima(x,order=c(i-1,0,j-1))
    AIC.matrix[i,j] <- AIC(currentArima) 
  }
}

# create loop to find best AIC fit
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

# print acf and pacf of this model
acf(best.model)
pacf(best.model)

