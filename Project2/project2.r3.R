#load library
library(vars)

#load Canada dataset and coerce to dataframe
data("Canada")
Canada.df <- as.data.frame(Canada)

#create new time series variables from Canada dataframe
C.e <- ts(Canada.df$e)
C.prod <- ts(Canada.df$prod)
C.rw <- ts(Canada.df$rw)
C.U <- ts(Canada.df$U)

#get differences of all these variables
C.e.diff <- diff(C.e)
C.prod.diff <- diff(C.prod)
C.rw.diff <- diff(C.rw)
C.U.diff <- diff(C.U)

#get acf and pacf for both employment and the employment differences
par(mfrow=c(2,2))
acf(C.e)
pacf(C.e)
acf(C.e.diff)
pacf(C.e.diff)

#get acf and pacf for both labour productivity and the labour productivity differences
par(mfrow=c(2,2))
acf(C.prod)
pacf(C.prod)
acf(C.prod.diff)
pacf(C.prod.diff)

#get acf and pacf for both real wage and the real wage differences
par(mfrow=c(2,2))
acf(C.rw)
pacf(C.rw)
acf(C.rw.diff)
pacf(C.rw.diff)

#get acf and pacf for both unemployment rate and the unemployment rate differences
par(mfrow=c(2,2))
acf(C.U)
pacf(C.U)
acf(C.U.diff)
pacf(C.U.diff)

#plot all 6 ccf combinations for all 3 explanatory variables and response variable
par(mfrow=c(3,2))
ccf(C.prod, C.e)
ccf(C.rw, C.e)
ccf(C.U, C.e)
ccf(C.prod.diff, C.e)
ccf(C.rw.diff, C.e)
ccf(C.U.diff, C.e)

#fit lm model with significant lags
model1 <- lm(C.e~lag(C.prod,0)+lag(C.rw,0)+lag(C.U,1))
summary(model1)

#store the residuals for this model in x
x <- model1$resid

#print acf and pacf of time series x
par(mfrow=c(1, 2))
acf(x)
pacf(x)

#create matrix to store AIC of ARIMA models
max.order <- 10
AIC.matrix <- list()

#set the maximum 'd' differences of the time series x to be 3
max.d <- 3

#create a loop that fits all 363 models and returns a new matrix for each integrated component 'd'
#this loop takes time series x, extracts the AIC value, and stores the value in a matrix (returns 4 in total)
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

#print each matrix
AIC.matrix[[1]]
AIC.matrix[[2]]
AIC.matrix[[3]]
AIC.matrix[[4]]

#to aid in finding which model parameters return the lowest AIC value, use the levelplot function
par(mfrow=c(2,2))
levelplot(AIC.matrix[[1]])
levelplot(AIC.matrix[[2]]) 
levelplot(AIC.matrix[[3]])
levelplot(AIC.matrix[[4]])

#best model is an ARIMA(2,0,0) model, with no integrated component! 
#fit the arima model given these parameters
best.model <- arima(x,order=c(2,0,0))

#print the estimated coefficients of this model
best.model