# Read in csv
bitcoin <- read.csv("~/Time-Series-R/bitcoin-2016-02-21.csv")


# Create time series object and plot

## start time series at July 18th 2010, increment by each day
bitcoin.ts <- ts(bitcoin$Close,start=c(2010,7,18),frequency=1)
plot.ts(bitcoin.ts)


# Autocorrelation/Partial autocorrelation functions

## series exhibits non-stationarity
acf(bitcoin.ts,lag.max = 200)

## pacf indicates seasonality
pacf(bitcoin.ts)


# Create new ts object

## take close differences and divide by length of the ts object - 1
bitcoin.ts2 <- ts(diff(bitcoin$Close)/
                    bitcoin$Close[-length(bitcoin$Close)]
                                                    ,start=c(2010,7,18),frequency=1)
## plot acf
acf(bitcoin.ts2)

# Forecast with nnetar (neural networks time series)

library(forecast)

## vector with first 1000 observations of time series
### this will be the training set
bitcoin.ts2.new <- bitcoin.ts2[1:1000]

## create nnetar model
### returns list with p=15 non-seasonal lags
bitcoin.ts2.new.hw <- nnetar(bitcoin.ts2.new,p = 15)

## forecast 1 day ahead
xhat <- forecast(bitcoin.ts2.new.hw,1)$mean

## loop over test set and obtain residuals

### create empty vector
allResiduals <- c()

### create loop
for(t in 1001:length(bitcoin.ts2)){
  
  allResiduals[t-1000] <- bitcoin.ts2[t]-xhat ## store residual (actual - prediction)
  bitcoin.ts2.new <- bitcoin.ts2[1:t] ## store differenced values in training set
  bitcoin.ts2.new.hw <- nnetar(bitcoin.ts2.new,p = 15) ## build model on data
  xhat <- forecast(bitcoin.ts2.new.hw,1)$mean ## forcast mean one day ahead
  
}

### print MSE and MAE

print(mean(allResiduals^2))
print(mean(abs(allResiduals)))