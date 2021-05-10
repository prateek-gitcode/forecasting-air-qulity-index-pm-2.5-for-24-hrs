#Importing the Dataset
library(readxl)
Delhi <- read_excel("Delhi (1).xlsx", range = "A3:B2374")
View(Delhi)
str(Delhi)
#EXPLORATORY DATA ANALYSIS
#SPlitting the date and time in the date column
Delhi$hms <- data.frame(do.call("rbind",strsplit(as.character(Delhi$date)," ",fixed = TRUE)))
Delhi$date <- as.Date(Delhi$hms$X1)
Delhi$hour<- Delhi$hms$X2
Delhi$pm25<- as.numeric(Delhi$pm25)
Delhi <- Delhi[,c(1,4,2)]#Choosing the final columns of the data
Delhi<- Delhi[order(Delhi$date),]#Sorting the dataset
#Imputation of missing values using interpolating mean
sum(is.na(Delhi$pm25))
#install.packages("imputeTS")
library("imputeTS")
Delhi$pm25 <- na_interpolation(Delhi$pm25)
boxplot(Delhi$pm25)
#Plotting pm25 over time(Very high variance)
library(ggplot2)
library(lmtest)
library(forecast)
ggplot(Delhi,aes(date,pm25)) + geom_line() + scale_x_date('Days') + 
  ylab("pm25") + xlab("")
plot.ts(Delhi$pm25)
trend <- lm(Delhi$pm25~c(1:length(Delhi$pm25)))
detrend= residuals(trend)
plot.ts(detrend)
Delhi$mavg <- ma(Delhi$pm25,order = 24)
ggplot() + 
  geom_line(data = Delhi, aes(x=date,y=pm25,colour="ma")) + 
  geom_line(data = Delhi, aes(x=date,y=mavg, colour="Daily ma")) + 
  ylab('pm25')
summary(Delhi)
Delhi2 <- replace(Delhi,TRUE, lapply(Delhi, na.aggregate))
summary(Delhi2)
ggplot() + 
  geom_line(data = Delhi2, aes(x=date,y=pm25,colour="ma")) + 
  geom_line(data = Delhi2, aes(x=date,y=mavg, colour="Daily ma")) + 
  ylab('pm25')
#DECOMPOSITION OF DATA:Take Seasonality, trend and cycle into account
#Calculating seasonal component
count_ma <- ts(Delhi2$mavg,frequency = 24)
decomp = stl(count_ma,s.window = "periodic")
#deseasonal_cnt <- seasadj(decomp)
plot(decomp)
#Test for Stationary series- Visual check for stationary variance
#Augmented Dickey Fuller Test
library(tseries)
adf.test(count_ma,alternative = "stationary")

#PLOTTING AUTOCORRELATIONS AND CHOOSING MODEL ORDER- ACF and PACF plots
#ACF plots display correlation between a series and its lags
Acf(count_ma,main='')
#PACF plots display correlation between a series and its lags explained, on the basis of previous lags
Pacf(count_ma,main='')
#Differencing: Deseasonalize data to bring it closer to the bounds
count_d1 <- diff(log(Delhi2$mavg), lag = 6,differences = 1)
plot.ts(count_d1)
adf.test(count_d1,alternative = "stationary")#Augmented Dickey Fuller Test of the differenced series
#Look for spikes at specific lag points of the differenced series
Acf(count_d1,main='ACF plot for differenced series')
Pacf(count_d1,main='PACF plot for differenced series')

#FITTING THE ARIMA MODEL
#Getting the p,d,q values with the help of auto.arima
auto.arima(count_d1,seasonal = FALSE)
#Evaluating and Iterating until the model makes sense
fit <- auto.arima(count_d1,seasonal = FALSE)
tsdisplay(residuals(fit),lag.max = 40,main = '(1,0,3) model residuals')
#Modifying the model according to the lag points
fit2 <- arima(count_d1,order = c(1,1,11))
tsdisplay(residuals(fit2),lag.max = 20,main = 'Seasonal model residuals')

#Forecasting Model with new fit 
par(mfrow=c(1,1))
fcast <- forecast(fit2,h=50)
plot(fcast)

#TESTING THE ARIMA MODEL WITH A HOLDOUT SET
#Test the Model Performance with a holdout set 
hold <- window(ts(count_d1),start=2335)
noholdfit <- arima(ts(count_d1[-c(2335:2365)]),order = c(1,1,11))
noholdforecast <- forecast(noholdfit,h=30)
plot(noholdforecast,main = "")
#Season inclusive ARIMA
auto.arima(count_d1,seasonal = TRUE)
season_fit= auto.arima(count_d1,seasonal = TRUE)
seas_fcast <- forecast(season_fit,h=30)
plot(seas_fcast)
seas_fcast

#FURTHUR TESTING AND FINAL SELECTION OF MODELS
#Test against the first auto.arima p,d,q values
tsdisplay(residuals(season_fit),lag.max = 15,main = 'Seasonal Model Residuals')
#fit3= arima(seas_fcast,order(2,0,2))
fit3 <- auto.arima(count_d1, seasonal = TRUE)
tsdisplay(residuals(fit3),lag.max = 15, main = "Seasonal model residuals")
#Custom ARIMA p,d,q values
fit4 <- arima(count_d1, order = c(1,1,13))
tsdisplay(residuals(fit4),lag.max = 15, main = "Seasonal Model resiiduals fit4")

fit5 <- arima(count_d1, order = c(0,1,12))
tsdisplay(residuals(fit5),lag.max = 15, main = "Seasonal Model resiiduals fit5")

#Default ARIMA p,d,q values
fit6 <- arima(count_d1,order = c(1,1,1))
tsdisplay(residuals(fit6),lag.max = 15, main = "Seaonal model Residuals(1,1,1)")

#Fitting all the "fit"s into one chart for comparision
par(mfrow= c(2,3))

#Choices of fits and evaluation of accuracy
fcast <- forecast(season_fit,h= 30)
fcast
plot(fcast)

fcast2 <- forecast(fit2, h=30)
plot(fcast2)


fcast3 <- forecast(fit4, h=30)
plot(fcast3)


fcast4 <- forecast(fit5, h=30)
plot(fcast4)


fcast5 <- forecast(fit6, h=30)
plot(fcast5)
#Accuracy of fits(Comparing the RMSE of different forecasts)
accuracy(fcast)#RMSE: 0.03751497(Choice of Forecast)
accuracy(fcast2)#RMSE:0.02808731
accuracy(fcast3)#RMSE:0.02808619
accuracy(fcast4)#RMSE:0.02809013
accuracy(fcast5)#RMSE:0.03974788
