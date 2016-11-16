# install packages
install.packages("gtrendsR")
library(gtrendsR)

# user info and password
user <- "stat4181@gmail.com"
psw <- "winteriscoming"
gconnect(user,psw)

# plot the mentions of 'flu' over 7 days
aa <- gtrends(c("flu"),res="7d")
plot(aa)

# retrieve basic info
str(aa)
head(aa$trend)

# coerce to time series object
flu <- ts(aa$trend[,2],start=c(1,1),frequency = 24)
plot(flu)

## Holt Winters ##

# fit Holt Winters exponential smoothing
flu.HW <- HoltWinters(flu)
plot(flu.HW)

# split into train and test sets
length(flu)
flu.train <- window(flu,start=c(1,1),end=c(6,24))
flu.test <- window(flu,start=c(7,1),end=c(7,24))

# plot with fitted lines
plot(flu.train,xlim=c(1,8))
lines(flu.test,col=2,lwd=4)

# fit and predict 2 years ahead using holt winters
flu.HW1 <- HoltWinters(flu.train)
HW1.pred <- predict(flu.HW1,n.ahead = 24)
lines(HW1.pred,col=4,lwd=3)

# plot residuals
HW1.res <- flu.test-c(HW1.pred)
plot(HW1.res)
sum(HW1.res^2) # sum of squared residuals: 490.0992

## AR models ##

# get positions in cycle of training set
flu.train.cycle <- cycle(flu.train)

# build regression model
lm1 <- lm(flu.train~as.factor(flu.train.cycle)-1)
plot(lm1$coefficients)

# plot residuals and retrieve acf of residuals
plot(lm1$residuals,type='l')
acf(lm1$residuals)
ar(lm1$residuals)

# import packages
library(nlme)
library(forecast)

# return best auto arima, regress using gls and compare coefficients
ar.1.auto <- auto.arima(lm1$residuals,d=0)
gls1 <- gls(flu.train~as.factor(flu.train.cycle)-1,
            correlation = corARMA(value = c(0.95,-0.134),p=1,q=1))
plot(gls1$coefficients)
points(lm1$coefficients,col=2)

# plot
plot(flu.train,xlim=c(1,8))
lines(flu.test,col=2,lwd=4)
lines(HW1.pred,col=4,lwd=3)

# extract arima residuals
auto.arima(gls1$residuals,d=0)

# extract coefficients and residuals from gls
gls.pred <- gls1$coefficients
gls.res <- flu.test-gls.pred

# plot residuals
plot(gls.res)
lines(HW1.res,col=2)

# compare sum of squared residuals
sum(HW1.res^2) # 490.0992
sum(gls.res^2) # 710.44

# build arima model and predict two years ahead
gls1.arima <- arima(gls1$residuals,order = c(2,0,1))
gls.pred.time <- predict(gls1.arima,n.ahead = 24)$pred

# create new object adding gls prediction and coefficients
gls.pred.proper <- gls.pred.time+c(gls1$coefficients)
gls.proper.res <- flu.test-gls.pred.proper

# plot residuals
plot(gls.res)
lines(HW1.res,col=2)
lines(gls.proper.res,col=3)

# get sum of equared residuals
sum(HW1.res^2) #490.0992
sum(gls.res^2) #710.44
sum(gls.proper.res^2) #641.443
