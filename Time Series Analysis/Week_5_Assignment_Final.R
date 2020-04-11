rm(list=ls())                            
while (!is.null(dev.list()))  dev.off
library(tsdl)
library(quantmod)
library(forecast)
library(lmtest)


tsdl
Microeconomic <- subset(tsdl,"Microeconomic")
str(Microeconomic)
Microeconomic[2]

# extract the 2nd microeconomic time series

Microeconomic[[2]]
expenditure <- as.data.frame(Microeconomic[[2]]) # for viewing only
plantexp <- ts(Microeconomic[[2]], frequency = 4, start = c(1964, 1)) # for Time series analysis
str(plantexp)

plot.ts(plantexp,ylab ="Expenditure in billions")
summary(plantexp)

sd <- sapply(plantexp, sd)
sd
mod2 <- decompose(plantexp)
plot(mod2)
#removing seasonality

mod3 <-  plantexp - mod2$seasonal
plot((mod3))
plot(mod3, xlab = 'Years', ylab = 'Expenditure in billions', main = 'Expenditure in billions removed seasonality')

#################################################

library(lmtest)

getwd()

setwd('C:\\Users\\ashle\\Desktop\\CPS\\Intermediate\\timeseries')

mydata <- read.csv('Daily Female Births.csv')  #part 2

head(mydata)

with(mydata, plot(Births ~ Date,xlab = "Date", ylab = "Female Births", main = "Date vs. Female Births"))

mod <- lm(mydata$Births ~ mydata$Date)

plot(mod$residuals ~ mod$fitted.values)
abline(0,0)

plot(mod, which = 1)

dwtest(mod)

Qty_ts <- ts(data=mydata$Births, start=1958, end =2019, freq= 12)
#plotting time series
str(Qty_ts)
plot.ts(Qty_ts)

#partition
qty_train<-window(Qty_ts, start = 1958, c(2003,12))
sum(qty_train)
qty_test<- window(Qty_ts, start = 2004 )
sum(qty_test)

#ARIMA model 
autoArima_train <- auto.arima(qty_train)
plot(forecast(autoArima_train, h=12))
ArimaModel_train <- forecast(autoArima_train, h=12)

#check for accuracy
summary(ArimaModel_train)


#test dataset
#ARIMA model
autoArima_test <- auto.arima(qty_test)
plot(forecast(autoArima_test, h=12))
ArimaModel_test <- forecast(autoArima_test, h=12)
#check for accuracy
summary(ArimaModel_test)

model2 <- hw(qty_train, initial='optimal', h=12 )
plot(model2)

accuracy(model2)


accuracy(model2)


model3 <- hw(qty_test, initial='optimal', h=12) #test data set
plot(model3)

accuracy(model3)
summary(model3)

#Exponential Smoothening training
ets_train <- ets(qty_train)
ets_train

#Forecast for ets component training
fcast_ets_train <- forecast(ets_train, h = 12)
plot(fcast_ets_train)
summary(fcast_ets_train)
#Accuracy
accuracy(fcast_ets_train)

#Exponential Smoothening test
ets_test <- ets(qty_test)
ets_test

#Forecast for ets component
fcast_ets_test <- forecast(ets_test, h = 12)
plot(fcast_ets_test)
summary(fcast_ets_test)
#Accuracy
accuracy(fcast_ets_test)
