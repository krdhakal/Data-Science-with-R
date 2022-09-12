## loading data##
getwd()
setwd('/Volumes/GoogleDrive/My Drive/ML_AI/Edureka/DS with R/Class 9/Files_Module9')
dss <- read.csv("M9_Monthly_Milkproduction.csv",header = TRUE)
colnames(dss) <- c('Year',"MilkProd")
head(dss)

## line plot ##
dssts <- ts(dss)
dssts
plot.ts(dssts)
milk.ts <- ts(dss$MilkProd, start=1962, freq=12)
plot((milk.ts), main='Milk Production Time Series', ylab=' Milk Production')

### Data Partition###

dsstraining <- dss[1:20,]
dssvalidation <- dss[21:nrow(dss),]

### rmse function#####
rmse<-function(error){
  sqrt(mean(error^2))
}
MAPE<-function(error,sales){
  mean(abs(error/sales))*100  
}


#ARIMA model in R
install.packages("forecast")
library(forecast)
dssts_filter <- dssts[1:20,c(2)]
tsData = ts(dssts_filter, start = c(1962,1), frequency = 4)
tsData

y = auto.arima(tsData)
y
plot(forecast(y, h=4))
pred <- forecast(y, h=4)
pred

pred_1 <- data.frame(pred)
pred_1

pred_1$original <- dssts[21:24,2]
pred_1$residual <- pred_1$Point.Forecast-pred_1$original
rmse(pred_1$residual)
MAPE(error,pred_1$original)
Acf(y$residuals)

#Moving average demonstration

install.packages("smooth")
library(smooth)
library(zoo)


#We can also decompose time series before moving ahead
decompose_milk = decompose(milk.ts)
plot(decompose_milk)


#Holtwinter's exponential smoothing
fit_holt_winter <- hw(y = milk.ts, h=12)
summary(fit_holt_winter)
plot(forecast(fit_holt_winter))
values <- fit_holt_winter$mean
error_holt_winter <- birthvalidation-values

#Acf(birthtraining.ts)
Acf(fit_holt_winter$residuals, type = "correlation")


