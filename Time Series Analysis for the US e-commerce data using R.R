library(fpp2)
library(ggplot2)
library(plotly)
library(tseries)

commerce <- read.csv("C://Users//35389//ecomm_us.csv") 
commsales <- ts(commerce$ECOMNSA, frequency=4, start=c(1999,4))
plot(commsales)

#Plot a simple moving average
#Quarterly retails of US e-commerce
plot(commsales, main="Raw Time Series")
plot(ma(commsales,5))

# For the Basic Time Series Models.

# 1. Using the Average or Mean model:
fcast.mean<-meanf(commsales, h=3)
summary(fcast.mean)
plot(fcast.mean)

#2. Using naive model:
fcast.naive<-naive(commsales, h=3)
summary(fcast.naive)
plot(fcast.naive)

#3. Using Seasonal naive model
fcast.seasonalnaive<-snaive(commsales, h=3)
summary(fcast.seasonalnaive)
plot(fcast.seasonalnaive)

# Using the Simple Exponential Smoothing model
commfit<-ses(commsales, h=3)
commfit
commfit$model
round(accuracy(commfit),2)

autoplot(commfit)+
  autolayer(fitted(commfit),series = "Fitted")

#Using the Holt exponential smoothing model
comm2fit<-holt(commsales, h=3)
comm2fit
comm2fit$model
round(accuracy(comm2fit),3)

autoplot(comm2fit)+
  autolayer(fitted(comm2fit),series = "Fitted")

#Using the Holt-Winters model to fit
comm4fit<-hw(commsales, 3)
comm4fit$model
comm4fit
accuracy(comm4fit)

autoplot(comm4fit)+
  autolayer(fitted(comm4fit),series = "Fitted")

#Using the ets model=ZZZ
comm3fit<-ets(commsales, model = "ZZZ")
comm3fit
comm3fit$model
accuracy(comm3fit)

autoplot(comm3fit)+
  autolayer(fitted(comm3fit),series = "Fitted")


# ARIMA Time Series Models.

#Check the order of differencing required
ndiffs(commsales)

#Plot the differenced Time Series
dcommsales <- diff(commsales)
plot(dcommsales)

#Assess stationarity of the differenced series
adf.test(dcommsales)

#ACF/PACF plots. Choosing p and q
Acf(dcommsales)
Pacf(dcommsales)

#Fitting an ARIMA model
fit <- arima(commsales, order=c(0,1,1))
fit

#Evaluating Model Fit
qqnorm(fit$residuals)
qqline(fit$residuals)
Box.test(fit$residuals, type="Ljung-Box")
checkresiduals(fit)
accuracy(fit)

#Forecasting with the fitted model
forecast(fit, 3)
plot(forecast(fit, 3), xlab="Quarter", ylab="Quarter Sales")

#auto ARIMA function
plot(commsales)
fit2 <- auto.arima(commsales)
fit2
plot(forecast(fit2, 3), xlab="Quarter", ylab="Quarter Sales")
