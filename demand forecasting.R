library("readxl")
library(forecast)
library("zoo")
#library("dplyr")
#read data from the excel file
SKU1598data<- read_excel("Selected SKU by group 8.xlsx", sheet = "SKU 1598")
SKU2083data<- read_excel("Selected SKU by group 8.xlsx", sheet = "SKU 2083")
SKU4020data<- read_excel("Selected SKU by group 8.xlsx", sheet = "SKU 4020")
SKU5817data<- read_excel("Selected SKU by group 8.xlsx", sheet = "SKU 5817")

###########################################################################
SKU1598data.ts <- ts(SKU1598data$demand, start = c(2017,10), end =c(2020,12), freq =12)
plot(SKU1598data.ts, xlab="Time series", ylab=" demand (per month)", main = "Demand of SKU 1598 over time series", bty="l", type ="b", xaxt ="n", yaxt="n")
axis (1,seq(2017,2021,1), las =1)
axis (2,las=1)
#we can see there are trend and randomness exist from the graph.
#forecast SKU1598 ( forecast for next 12 months)
SKU1598.ts <- ts(SKU1598data$demand,start=c(2017,10), end=c(2020,12),freq=12)
#Partition the data
nValid <- 12
nTrain <- length(SKU1598.ts) - nValid
train.ts <- window(SKU1598.ts, start=c(2017,10), end=c(2017,nTrain))
valid.ts <- window(SKU1598.ts, start=c(2017,nTrain+1), end=c(2017,nTrain+nValid))
#navie forecast method
seasonal <- snaive(train.ts, h=nValid)
accuracy(seasonal,valid.ts)
plot(seasonal, xlab="Time series", ylab=" demand (per month)", main = "SKU 1598 ", bty="l", xaxt ="n",yaxt="n")
axis (1,seq(2017,2021,1), las =1)
axis (2,las=1)

#create trailing and centered moving averages
ma.trailing <- rollmean(SKU1598.ts,k=12,align="right") 
ma.centered <- ma(SKU1598.ts,order=12)
plot(SKU1598.ts, xlab="Time", ylab="Demand (per month)", ylim=c(0,1000),xlim=c(2017,2021),bty="l",xaxt="n", main ="Demand under moving averages method")
axis(1, at=seq(2017,2021), labels=format(seq(2017,2021)))
lines(ma.centered,lwd=2)
lines(ma.trailing,lwd=2,lty=2)
#make the forecast
last.ma<-tail(ma.trailing,1)
ma.trailing.pred<-ts(rep(last.ma,nValid),start=c(2017,nTrain+1), end=c(2017,nTrain+nValid),freq=12)
legend(2017,1000, c("Demand (per month)","Centered Moving Average", "Trailing Moving Average"), lty=c(1,1,2),cex=0.6)
#performance measure
accuracy(ma.trailing.pred,valid.ts)

#Do the smoothing
smoothingmethod <- ets(train.ts)
smoothingmethod.pred <- forecast(smoothingmethod,h=nValid,level=0)
#plot
plot(smoothingmethod.pred, ylim = c(0, 1000),  ylab = "Demand", xlab = "Time", bty = "l", xaxt = "n", xlim = c(2017,2021.25), main = "Demand under smoothing method", flty = 2)
axis(1, at = seq(2017, 2021, 1), labels = format(seq(2017, 2021, 1)))
lines(smoothingmethod.pred$fitted, lwd = 2, col = "blue")
lines(valid.ts)
#performance measure
accuracy(smoothingmethod.pred, valid.ts)

#Make the exponential smoothing
smoothingmethod.expo <- ets(train.ts,alpha=0.2,beta=0.15,gamma=0.05,restrict="FALSE")
smoothingmethod.expo.forecast <- forecast(smoothingmethod.expo, h=nValid, level=0)
plot(smoothingmethod.expo.forecast, ylab = "Demand (per month)", xlab= "Time", bty = "l", xaxt = "n", xlim = c(2017,2021), main= "Demand under exponential smoothing method", flty = 2)
axis(1, at = seq(2017, 2021, 1), labels = format(seq(2017,2021, 1)))
lines(smoothingmethod.expo.forecast$fitted, lwd = 2, col = "blue")
lines(valid.ts)
#performance measure
accuracy(smoothingmethod.expo.forecast, valid.ts)

#Perform regression method
SKU1598data$index <- 1:length(SKU1598data$salesdate)
train.tslm <- tslm(train.ts ~ trend)
summary(train.tslm)
train.tslm.forecast <- forecast(train.tslm,h=nValid,level=0)
plot(train.tslm.forecast,xlab="Time", ylab="Demand", ylim= c(0,1000), bty="l",flty=2,xaxt="n", main = "Demand under regression method")
axis(1, at=seq(2017,2021,1),labels=format(seq(2017,2021,1)))
lines(train.tslm.forecast$fitted,lwd=2,col="blue")
lines(valid.ts)

summary(train.tslm.forecast,valid.ts)
#polynomial trend
train.tslm.poly <- tslm(train.ts ~trend + I(trend^2))
summary(train.tslm.poly)
train.tslm.poly.forecast <- forecast(train.tslm.poly,h=nValid,level=0)
#exponential trend
train.expo <- tslm(train.ts ~ trend, lambda=0)
summary(train.expo)
train.expo.forecast <- forecast(train.expo,h=nValid,level=0)
#Comparing the trends
plot(train.tslm.poly.forecast,xlab="Time", ylab="Demand(per month)", ylim= c(0,1000),xlim=c(2017,2021.25), bty="l",flty=2,xaxt="n", main = "Compare the trend")
axis(1, at=seq(2017,2021,1),labels=format(seq(2017,2021,1)))
lines(valid.ts)
lines(train.tslm.poly.forecast$fitted,lwd=2,col="blue")
lines(train.tslm.forecast$fitted,lwd=2,col="red",lty=1)
lines(train.tslm.forecast$mean,lwd=2,col="red",lty=3)
lines(train.expo.forecast$fitted,lwd=2,col="green",lty=1)
lines(train.expo.forecast$mean,lwd=2,col="green",lty=3)
legend(2017, 1020, legend=c("Quadratic forecast", "Linear forecast","Exponential forecast"),col=c("blue", "red","green"), lty=c(2,2,2), cex=0.7)
# we can see the polynomial forecast perform the best

#Accuracy measures
accuracy(train.tslm.forecast, valid.ts)
accuracy(train.tslm.poly.forecast, valid.ts)
accuracy(train.expo.forecast, valid.ts)

#ARIMA method
#selecting ARIMA model
fitSKU1598 <- auto.arima(SKU1598.ts)
ARIMASKU1598 <- arima(SKU1598.ts,order=c(0,1,1))
ARIMASKU1598
#using ARIMA (0,1,1)to forecast
SKU1598.tsforecasts<-forecast(ARIMASKU1598,h=12)
SKU1598.tsforecasts
#plot for the forecast
plot(SKU1598.tsforecasts,main="Demand forecast by using ARIMA method of SKU1598",xlab = "Time", ylab = "Demand")
lines(SKU1598.tsforecasts$fitted,lwd=2,col="blue")
#Produce a correlation plot of the forecast error 
#and perform the Ljung-Box test by typing
acf(SKU1598.tsforecasts$residuals,lag.max=12,main="ACF SKU1598")
Box.test(SKU1598.tsforecasts$residuals,lag=12,type="Ljung-Box")
#plot the forecast error
plot(SKU1598.tsforecasts$residuals,main="Residuals of SKU1598", xaxt ="n", yaxt="n",xlab="Time", ylab="Residuals")
axis (1,seq(2017,2021,1), las =1)
axis (2,las=1)


#############################################################################
# explore and visualize data > create a time series of SKU2083
SKU2083.ts <- ts(SKU2083data$demand,start=c(2019,1), end=c(2020,12),freq=12)
plot(SKU2083.ts, xlab="Time series", ylab=" demand (per month)", main = "Demand of SKU 2083 over time series", bty="l", type ="b", xaxt ="n", yaxt="n")
axis (1,seq(2019,2021,1), las =1)
axis (2,las=1)
#we can see there are trend and randomness exist from the graph.
#Partition the data
nValid <- 6
nTrain <- length(SKU2083.ts) - nValid
train.ts <- window(SKU2083.ts, start=c(2019,1), end=c(2019,nTrain))
valid.ts <- window(SKU2083.ts, start=c(2019,nTrain+1), end=c(2019,nTrain+nValid))
#navie forecast method
seasonal <- snaive(train.ts, h=nValid)
accuracy(seasonal,valid.ts)
plot(seasonal, xlab="Time series", xlim=c(2019,2021),ylab=" demand (per month)", main = "SKU 2083 ", bty="l", xaxt ="n",yaxt="n")
axis (1,seq(2017,2022,1), las =1)
axis (2,las=1)

#create trailing and centered moving averages
ma.trailing <- rollmean(SKU2083.ts,k=6,align="right") 
ma.centered <- ma(SKU2083.ts,order=6)
plot(SKU2083.ts, xlab="Time", ylab="Demand (per month)", ylim=c(20,3000),xlim=c(2019,2021),bty="l",xaxt="n",main="Demand under moving averages")
axis(1, at=seq(2019,2021), labels=format(seq(2019,2021)))
lines(ma.centered,lwd=2)
lines(ma.trailing,lwd=2,lty=2)
#make the forecast
last.ma<-tail(ma.trailing,1)
ma.trailing.pred<-ts(rep(last.ma,nValid),start=c(2019,nTrain+1), end=c(2019,nTrain+nValid),freq=12)
legend(2019.4,3200, c("Demand (per month)","Centered Moving Average", "Trailing Moving Average"), lty=c(1,1,2),cex=0.6)
#performance measure
accuracy(ma.trailing.pred,valid.ts)


#Do the smoothing
smoothingmethod <- ets(train.ts)
smoothingmethod.pred <- forecast(smoothingmethod,h=nValid,level=0)
#plot
plot(smoothingmethod.pred, ylim = c(20, 3000),  ylab = "Demand", xlab = "Time", bty = "l", xaxt = "n", xlim = c(2019,2021.25), main = "Demand under smoothing method", flty = 2)
axis(1, at = seq(2019, 2021, 1), labels = format(seq(2019, 2021, 1)))
lines(smoothingmethod.pred$fitted, lwd = 2, col = "blue")
lines(valid.ts)
#performance measure
accuracy(smoothingmethod.pred, valid.ts)

#Make the exponential smoothing
smoothingmethod.expo <- ets(train.ts,alpha = 0.2,beta = 0.15,gamma = 0.05,restrict="FALSE")
smoothingmethod.expo.forecast <- forecast(smoothingmethod.expo, h=nValid, level=0)
plot(smoothingmethod.expo.forecast, ylab = "Demand (per month)", xlab= "Time", bty = "l", xaxt = "n", xlim = c(2019,2021), main= "Demand under exponential smoothing", flty = 2)
axis(1, at = seq(2019, 2021, 1), labels = format(seq(2019,2021, 1)))
lines(smoothingmethod.expo.forecast$fitted, lwd = 2, col = "blue")
lines(valid.ts)
#performance measure
accuracy(smoothingmethod.expo.forecast, valid.ts)

#Perform regression method
SKU2083data$index <- 1:length(SKU2083data$salesdate)
train.tslm <- tslm(train.ts ~ trend)
summary(train.tslm)
train.tslm.forecast <- forecast(train.tslm,h=nValid,level=0)
plot(train.tslm.forecast,xlab="Time", ylab="Demand", ylim= c(20,3000), bty="l",flty=2,xaxt="n", main = "Demand under regression method")
axis(1, at=seq(2019,2021,1),labels=format(seq(2019,2021,1)))
lines(train.tslm.forecast$fitted,lwd=2,col="blue")
lines(valid.ts)
summary(train.tslm.forecast,valid.ts)
#polynomial trend
train.tslm.poly <- tslm(train.ts ~trend + I(trend^2))
summary(train.tslm.poly)
train.tslm.poly.forecast <- forecast(train.tslm.poly,h=nValid,level=0)
#exponential trend
train.expo <- tslm(train.ts ~ trend, lambda=0)
summary(train.expo)
train.expo.forecast <- forecast(train.expo,h=nValid,level=0)
#Comparing the trends
plot(train.tslm.poly.forecast,xlab="Time", ylab="Demand(per month)", ylim= c(20,3000),xlim=c(2019,2021.25), bty="l",flty=2,xaxt="n", main = "Compare the trends")
axis(1, at=seq(2019,2021,1),labels=format(seq(2019,2021,1)))
lines(valid.ts)
lines(train.tslm.poly.forecast$fitted,lwd=2,col="blue")
lines(train.tslm.forecast$fitted,lwd=2,col="red",lty=1)
lines(train.tslm.forecast$mean,lwd=2,col="red",lty=3)
lines(train.expo.forecast$fitted,lwd=2,col="green",lty=1)
lines(train.expo.forecast$mean,lwd=2,col="green",lty=3)
legend(2019.3, 3105, legend=c("Quadratic forecast", "Linear forecast","Exponential forecast"),col=c("blue", "red","green"), lty=1:3, cex=0.6)
# we can see the polynomial forecast perform the best

#Accuracy measures
accuracy(train.tslm.forecast, valid.ts)
accuracy(train.tslm.poly.forecast, valid.ts)
accuracy(train.expo.forecast, valid.ts)

#AR method(less than 2 years data)
#fit and forecast
train.tslm.trend<- tslm(train.ts ~ trend + I(trend^2))
train.res.arima <- Arima(train.tslm.trend$residuals,order = c(1,0,0))
train.res.arima.forecast <- forecast(train.res.arima,h=nValid)

plot(train.tslm.trend$residuals,xlab="Time", ylab="Residuals", bty="l",,xaxt="n",main="The residual of SKU2083")
axis(1, at=seq(2019,2021,1),labels=format(seq(2019,2021,1)))
lines(train.res.arima.forecast$fitted,lwd=2,col="blue")
summary(train.res.arima)
Acf(train.res.arima$residuals,lag.max=12,main=" The ACF of SKU2083")
accuracy(train.res.arima.forecast,valid.ts)

#forecast result for Janurary 2020
forecast(train.tslm.trend)$mean[1]
#actual value
valid.ts[1]

############################################################################
# explore and visualize data > create a time series of SKU4020
SKU4020.ts <- ts(SKU4020data$demand,start=c(2019,1), end=c(2020,4),freq=12)
plot(SKU4020.ts, xlab="Time series", ylab=" demand (per month)", main = "Demand of SKU 4020 over time series", bty="l", type ="b", xaxt ="n", yaxt="n")
axis (1,seq(2019,2021,1),las =1)
axis (2,las=1)
#we can see there are random (because of noise) and trend from the graph
#Partition the data
nValid <- 6
nTrain <- length(SKU4020.ts) - nValid
train.ts <- window(SKU4020.ts, start=c(2019,1), end=c(2019,nTrain))
valid.ts <- window(SKU4020.ts, start=c(2019,nTrain+1), end=c(2019,nTrain+nValid))
#navie forecast method
seasonal <- snaive(train.ts, h=nValid)
accuracy(seasonal,valid.ts)

#create trailing and centered moving averages
ma.trailing <- rollmean(SKU4020.ts,k=6,align="right") 
ma.centered <- ma(SKU4020.ts,order=6)
plot(SKU4020.ts, xlab="Time", ylab="Demand (per month)", ylim=c(30,500),xlim=c(2019,2021),bty="l",xaxt="n", main="Demand under moving averages")
axis(1, at=seq(2019,2021), labels=format(seq(2019,2021)))
lines(ma.centered,lwd=2)
lines(ma.trailing,lwd=2,lty=2)
last.ma<-tail(ma.trailing,1)
ma.trailing.pred<-ts(rep(last.ma,nValid),start=c(2019,nTrain+1), end=c(2019,nTrain+nValid),freq=12)
legend(2020,500, c("Demand (per month)","Centered Moving Average", "Trailing Moving Average"), lty=c(1,1,2),cex=0.6)
#performance measure
accuracy(ma.trailing.pred,valid.ts)
#Do the smoothing
smoothingmethod <- ets(train.ts)
smoothingmethod.pred <- forecast(smoothingmethod,h=nValid,level=0)
#plot
plot(smoothingmethod.pred, ylim = c(30, 500),  ylab = "Demand", xlab = "Time", bty = "l", xaxt = "n", xlim = c(2019,2021.25), main = "Demand under smoothing method", flty = 2)
axis(1, at = seq(2019, 2021, 1), labels = format(seq(2019, 2021, 1)))
lines(smoothingmethod.pred$fitted, lwd = 2, col = "blue")
lines(valid.ts)
#performance measure
accuracy(smoothingmethod.pred, valid.ts)

#Make the exponential smoothing
smoothingmethod.expo <- ets(train.ts,alpha = 0.2,beta = 0.15,gamma = 0.05,restrict="FALSE")
smoothingmethod.expo.forecast <- forecast(smoothingmethod.expo, h=nValid, level=0)
plot(smoothingmethod.expo.forecast, ylab = "Demand (per month)", xlab= "Time", bty = "l", xaxt = "n", xlim = c(2019,2020), main= "Demand under exponential smoothing", flty = 2)
axis(1, at = seq(2019, 2021, 1), labels = format(seq(2019,2021, 1)))
lines(smoothingmethod.expo.forecast$fitted, lwd = 2, col = "blue")
lines(valid.ts)
#performance measure
accuracy(smoothingmethod.expo.forecast, valid.ts)


#Perform regression method
SKU4020data$index <- 1:length(SKU4020data$salesdate)
train.tslm <- tslm(train.ts ~ trend)
summary(train.tslm)
train.tslm.forecast <- forecast(train.tslm,h=nValid,level=0)
plot(train.tslm.forecast,xlab="Time", ylab="Demand", ylim= c(30,500), bty="l",flty=2,xaxt="n", main = "Demand under regression method")
axis(1, at=seq(2019,2021,1),labels=format(seq(2019,2021,1)))
lines(train.tslm.forecast$fitted,lwd=2,col="blue")
lines(valid.ts)
summary(train.tslm.forecast,valid.ts)
#polynomial trend
train.tslm.poly <- tslm(train.ts ~trend + I(trend^2))
summary(train.tslm.poly)
train.tslm.poly.forecast <- forecast(train.tslm.poly,h=nValid,level=0)
#exponential trend
train.expo <- tslm(train.ts ~ trend, lambda=0)
summary(train.expo)
train.expo.forecast <- forecast(train.expo,h=nValid,level=0)
#Comparing the trends
plot(train.tslm.poly.forecast,xlab="Time", ylab="demand (per month)", ylim= c(30,500),xlim=c(2019,2021.25), bty="l",flty=2,xaxt="n", main = "Compare the trends")
axis(1, at=seq(2019,2021,1),labels=format(seq(2019,2021,1)))
lines(valid.ts)
lines(train.tslm.poly.forecast$fitted,lwd=2,col="blue")
lines(train.tslm.forecast$fitted,lwd=2,col="red",lty=1)
lines(train.tslm.forecast$mean,lwd=2,col="red",lty=3)
lines(train.expo.forecast$fitted,lwd=2,col="green",lty=1)
lines(train.expo.forecast$mean,lwd=2,col="green",lty=3)
legend(2020.2, 500, legend=c("Quadratic forecast", "Linear forecast","Exponential forecast"),col=c("blue", "red","green"), lty=1:3, cex=0.8)
# we can see the polynomial forecast perform the best

#Accuracy measures
accuracy(train.tslm.forecast, valid.ts)
accuracy(train.tslm.poly.forecast, valid.ts)
accuracy(train.expo.forecast, valid.ts)

#AR method(less than 2 years data)
#fit and forecast
train.tslm.trend<- tslm(train.ts ~ trend + I(trend^2))
train.res.arima <- Arima(train.tslm.trend$residuals,order = c(1,0,0))
train.res.arima.forecast <- forecast(train.res.arima,h=nValid)

plot(train.tslm.trend$residuals,xlab="Time", xlim=c(2019,2021),ylab="Residuals", bty="l",,xaxt="n",main="The residuals of SKU4020")
axis(1, at=seq(2019,2021,1),labels=format(seq(2019,2021,1)))
lines(train.res.arima.forecast$fitted,lwd=2,col="blue")
summary(train.res.arima)
Acf(train.res.arima$residuals,lag.max=12,main=" The ACF of SKU4020")
accuracy(train.res.arima.forecast,valid.ts)

#forecast result for Janurary 2020
forecast(train.tslm.trend)$mean[1]
#actual value
valid.ts[1]

###########################################################################
# explore and visualize data >create a time series of SKU5817
SKU5817.ts <- ts(SKU5817data$demand,start=c(2019,2), end=c(2020,7),freq=12)
plot(SKU5817.ts, xlab="Time series", xlim=c(2019,2021),ylab=" demand (per month)", main = "Demand of SKU 5817 over time series", bty="l", type ="b", xaxt ="n", yaxt="n")
axis (1,seq(2019,2020,1),las =1)
axis (2,las=1)
# we can see there are trend and random (because of noise) from the graph
#Partition the data
nValid <- 6
nTrain <- length(SKU5817.ts) - nValid
train.ts <- window(SKU5817.ts, start=c(2019,2), end=c(2019,nTrain))
valid.ts <- window(SKU5817.ts, start=c(2019,nTrain+1), end=c(2019,nTrain+nValid))
#navie forecast method
seasonal <- snaive(train.ts, h=nValid)
accuracy(seasonal,valid.ts)

#create trailing and centered moving averages
ma.trailing <- rollmean(SKU5817.ts,k=6,align="right") 
ma.centered <- ma(SKU5817.ts,order=6)
plot(SKU5817.ts, xlab="Time", ylab="Demand (per month)", ylim=c(40,4200),xlim=c(2019,2020),bty="l",xaxt="n", main ="Demand under moving averages")
axis(1, at=seq(2019,2021), labels=format(seq(2019,2021)))
lines(ma.centered,lwd=2)
lines(ma.trailing,lwd=2,lty=2)
last.ma<-tail(ma.trailing,1)
ma.trailing.pred<-ts(rep(last.ma,nValid),start=c(2019,nTrain+1), end=c(2019,nTrain+nValid),freq=12)
legend(2019.5,4500, c("Demand (per month)","Centered Moving Average", "Trailing Moving Average"), lty=c(1,1,2),cex=0.7)

#performance measure
accuracy(ma.trailing.pred,valid.ts)
#Do the smoothing
smoothingmethod <- ets(train.ts)
smoothingmethod.pred <- forecast(smoothingmethod,h=nValid,level=0)
#plot
plot(smoothingmethod.pred, ylim = c(40, 4200),  ylab = "Demand", xlab = "Time", bty = "l", xaxt = "n", xlim = c(2019,2021.25), main = "Demand under smoothing method", flty = 2)
axis(1, at = seq(2019, 2021, 1), labels = format(seq(2019, 2021, 1)))
lines(smoothingmethod.pred$fitted, lwd = 2, col = "blue")
lines(valid.ts)
#performance measure
accuracy(smoothingmethod.pred, valid.ts)

#Make the exponential smoothing
smoothingmethod.expo <- ets(train.ts,alpha = 0.2,beta = 0.15,gamma = 0.05,restrict="FALSE")
smoothingmethod.expo.forecast <- forecast(smoothingmethod.expo, h=nValid, level=0)
plot(smoothingmethod.expo.forecast, ylab = "Demand (per month)", xlab= "Time", bty = "l", xaxt = "n", xlim = c(2019,2021), main= "Demand under exponential smoothing", flty = 2)
axis(1, at = seq(2019, 2021, 1), labels = format(seq(2019,2021, 1)))
lines(smoothingmethod.expo.forecast$fitted, lwd = 2, col = "blue")
lines(valid.ts)
#performance measure
accuracy(smoothingmethod.expo.forecast, valid.ts)

#Perform regression method
SKU5817data$index <- 1:length(SKU5817data$salesdate)
train.tslm <- tslm(train.ts ~ trend)
summary(train.tslm)
train.tslm.forecast <- forecast(train.tslm,h=nValid,level=0)
plot(train.tslm.forecast,xlab="Time", ylab="Demand", ylim= c(40,4200), bty="l",flty=2,xaxt="n", main= " Demand under regression method")
axis(1, at=seq(2019,2021,1),labels=format(seq(2019,2021,1)))
lines(train.tslm.forecast$fitted,lwd=2,col="blue")
lines(valid.ts)
summary(train.tslm.forecast,valid.ts)
#polynomial trend
train.tslm.poly <- tslm(train.ts ~trend + I(trend^2))
summary(train.tslm.poly)
train.tslm.poly.forecast <- forecast(train.tslm.poly,h=nValid,level=0)
#exponential trend
train.expo <- tslm(train.ts ~ trend, lambda=0)
summary(train.expo)
train.expo.forecast <- forecast(train.expo,h=nValid,level=0)
#Comparing the trends
plot(train.tslm.poly.forecast,xlab="Time", ylab="demand (per month)", ylim= c(40,4200),xlim=c(2019,2021.25), bty="l",flty=2,xaxt="n", main = "Compare the trends")
axis(1, at=seq(2019,2021,1),labels=format(seq(2019,2021,1)))
lines(valid.ts)
lines(train.tslm.poly.forecast$fitted,lwd=2,col="blue")
lines(train.tslm.forecast$fitted,lwd=2,col="red",lty=1)
lines(train.tslm.forecast$mean,lwd=2,col="red",lty=3)
lines(train.expo.forecast$fitted,lwd=2,col="green",lty=1)
lines(train.expo.forecast$mean,lwd=2,col="green",lty=3)
legend(2020, 4200, legend=c("Quadratic forecast", "Linear forecast","Exponential forecast"),col=c("blue", "red","green"), lty=1:3, cex=0.8)
# we can see the polynomial forecast perform the best

#Accuracy measures
accuracy(train.tslm.forecast, valid.ts)
accuracy(train.tslm.poly.forecast, valid.ts)
accuracy(train.expo.forecast, valid.ts)


#AR method(less than 2 years data)
#fit and forecast
train.tslm.trend<- tslm(train.ts ~ trend + I(trend^2))
train.res.arima <- Arima(train.tslm.trend$residuals,order = c(1,0,0))
train.res.arima.forecast <- forecast(train.res.arima,h=nValid)

plot(train.tslm.trend$residuals,xlab="Time", ylab="Residuals", bty="l",xlim=c(2019,2021),xaxt="n",main="The resudual of SKU5817")
axis(1, at=seq(2019,2021,1),labels=format(seq(2019,2021,1)))
lines(train.res.arima.forecast$fitted,lwd=2,col="blue")
summary(train.res.arima)
Acf(train.res.arima$residuals,lag.max=12,main=" The ACF of SKU5817")
accuracy(train.res.arima.forecast,valid.ts)

#forecast result for Feburary 2020
forecast(train.tslm.trend)$mean[1]
#actual value
valid.ts[1]
##############################################################################################

