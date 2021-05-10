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
ggplot(Delhi,aes(date,pm25)) + geom_line() + scale_x_date('Days') + 
  ylab("pm25") + xlab("")
#Plotting the data over weekdays to find outliers
ggplot(Delhi, aes(date,pm25)) + geom_point(color = "navyblue") + 
  facet_wrap( ~ weekdays.Date(Delhi$date)) + scale_x_date('week') + ylab("pm25") + xlab("")
#Creating a time series object to pass on to tsclean()
library(tseries)
library(forecast)
ts_obj <- ts(Delhi[,c('pm25')])
Delhi$ts_clean = tsclean(ts_obj)#tsclean fuction to identify and replace outliers and input missing values if any
#Plotting cleaned data
ggplot() + 
  geom_line(data = Delhi, aes(x= date, y= ts_clean)) + ylab('cleaned pm25')
#Getting moving averages on a daily basis(days) to compare with cleaned data
Delhi$cnt_ma <- ma(Delhi$ts_clean,order = 24)
ggplot() + 
  geom_line(data = Delhi, aes(x=date,y=ts_clean,colour="ma")) + 
  geom_line(data = Delhi, aes(x=date,y=cnt_ma, colour="Daily ma")) + 
  ylab('pm25')
#DECOMPOSITION OF DATA:Take Seasonality, trend and cycle into account
#Calculating seasonal component
count_ma <- ts(na.omit(Delhi$cnt_ma),frequency = 30)
decomp = stl(count_ma,s.window = "periodic")
deseasonal_cnt <- seasadj(decomp)
plot(decomp)
#Test for Stationary series- Visual check for stationary variance
#Augmented Dickey Fuller Test
adf.test(count_ma,alternative = "stationary")

#PLOTTING AUTOCORRELATIONS AND CHOOSING MODEL ORDER- ACF and PACF plots
#ACF plots display correlation between a series and its lags
Acf(count_ma,main='')
#PACF plots display correlation between a series and its lags explained, on the basis of previous lags
Pacf(count_ma,main='')
#Differencing: Deseasonalize data to bring it closer to the bounds
count_d1 <- diff(deseasonal_cnt,differences = 1)
count_d1
plot(count_d1)
adf.test(count_d1,alternative = "stationary")#Augmented Dickey Fuller Test of the differenced series
#Look for spikes at specific lag points of the differenced series
Acf(count_d1,main='ACF plot for differenced series')
Pacf(count_d1,main='PACF plot for differenced series')


auto.arima(deseasonal_cnt,seasonal = FALSE)

fit <- auto.arima(deseasonal_cnt,seasonal = FALSE)
tsdisplay(residuals(fit),lag.max = 45,main = '(1,1,1) model residuals')

fit2 <- arima(deseasonal_cnt,order = c(1,1,10))
tsdisplay(residuals(fit2),lag.max = 20,main = 'Seasonal model residuals')


par(mfrow=c(1,1))
fcast <- forecast(fit2,h=50)
plot(fcast)


hold <- window(ts(deseasonal_cnt),start=1200)
noholdfit <- arima(ts(deseasonal_cnt[-c(1200:2347)]),order = c(1,1,10))
noholdforecast <- forecast(noholdfit,h=50)
plot(noholdforecast,main = "")
lines(ts(deseasonal_cnt))

season_fit= auto.arima(deseasonal_cnt,seasonal = TRUE)
seas_fcast <- forecast(season_fit,h=30)
plot(seas_fcast)
lines(ts(count_ma))
lines(ts(deseasonal_cnt))
