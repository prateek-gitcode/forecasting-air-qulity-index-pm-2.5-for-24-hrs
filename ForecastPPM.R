#Importing the dataset
library(readxl)
Delhi <- read_excel("Delhi (1).xlsx", range = "A3:B2374")
View(Delhi)
###########################################################################
str(Delhi)#Structure of the dataset.
###########################################################################
#Separating the date and time instances from the 'date' column of the dataset
Delhi$hms <- data.frame(do.call("rbind",strsplit(as.character(Delhi$date)," ",fixed = TRUE)))
Delhi$date <- as.Date(Delhi$hms$X1)
Delhi$hour<- as.Date(Delhi$hms$X2)
###########################################################################
Delhi <- Delhi[,c(1,4,2)]
Delhi$pm25 <-as.numeric(Delhi$pm25) #Converting character format to factor
#Converting the data frame into time series format
library(xts)
Delhi <- xts(x= Delhi, order.by = Delhi$date)
class(Delhi)
###########################################################################
#Exploratory Data Analysis 
nrow(Delhi)
ncol(Delhi)
start(Delhi)
end(Delhi)
frequency(Delhi)
summary(Delhi$pm25)
sum(is.na(Delhi$pm25))
length(unique(Delhi$pm25))
library(tseries)
ts_obj <- ts(Delhi[,c('pm25')])
library(ggplot2)
ggplot(Delhi,aes(Delhi$hour,Delhi$pm25))+geom_line()+scale_x_date('hours')+ylab("pm25")+xlab("")
