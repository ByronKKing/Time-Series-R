## Load Data, Plot etc.

bitcoin <- read.csv("C:/Users/ByronKing/Desktop/bitcoin-2016-02-21.csv")
head(bitcoin)
tail(bitcoin)
bitcoin.ts <- ts(bitcoin$Close,start=c(2010,7,18),frequency=1)
plot.ts(bitcoin.ts)
acf(bitcoin.ts,lag.max = 200)

bitcoin.ts2 <- ts(diff(bitcoin$Close)/
                    bitcoin$Close[-length(bitcoin$Close)]
                                                    ,start=c(2010,7,18),frequency=1)
acf(bitcoin.ts2)

## Forecasting with nnetar

library(forecast)

bitcoin.ts2.new <- bitcoin.ts2[1:1000]
bitcoin.ts2.new.hw <- nnetar(bitcoin.ts2.new,p = 15)
xhat <- forecast(bitcoin.ts2.new.hw,1)$mean
allResiduals <- c()

for(t in 1001:length(bitcoin.ts2)){
  if(t %%50 ==0) {print(t)}
  allResiduals[t-1000] <- bitcoin.ts2[t]-xhat
  
  bitcoin.ts2.new <- bitcoin.ts2[1:t]
  bitcoin.ts2.new.hw <- nnetar(bitcoin.ts2.new,p = 15)
  xhat <- forecast(bitcoin.ts2.new.hw,1)$mean
}

print(mean(allResiduals^2))
print(mean(abs(allResiduals)))