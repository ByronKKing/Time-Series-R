#Retrieve ratings
x <- twentyfour
#coerce to ts object
x <- as.ts(x)

#fit a least squares model while converting season to a factor and removing intercept
lm1 <- lm(x~factor(season)-1,data=twentyfour)
#get regression output
summary(lm1)

x <- lm1$residuals

#get acf and pacf of residuals
acf(lm1$residuals,lag=60)

#create matrix to store AIC of ARMA models
max.order <- 10
AIC.matrix <- matrix(0,nrow = max.order+1,ncol= max.order+1)

#create loop to store AIC of ARMA models with specified parameters
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
      LOWEST_AIC <- AIC.matrix[i,j]3
      best.model <- arima(lm1,order=c(i,0,j))
    }
  }
}
#plot lattice, row is p column is q
levelplot(AIC.matrix)

#fit gls model
gls1 <- gls(x~factor(season)-1,data=twentyfour,correlation = corARMA(value = coef(best.model)[1:6],p=2,q=4))
summary(gls1)







