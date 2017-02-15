#Applied ARIMA model for forecasting

install.packages("zoo")
#
install.packages("chron") 
#Create chronological objects which represent dates and times of day
install.packages("FitAR")
?FitAR
#fitting arima model, for calling arima package
install.packages("xts")
#to convert data to time series
install.packages("forecast")
library(forecast)
#for autom.arima function
install.packages("tseries")
library(tseries)
#adf testing: to test the stationarity of the data
install.packages("MASS")
#for Histogram using MASS fucntion
?truehist
?boxcox
install.packages("quantmod")
?quantmod
install.packages("ggplot2")

library("zoo")
library("chron") 
library("FitAR")
library("xts")
library("forecast")
library("tseries") 
library("MASS")
library("quantmod")
library("ggplot2")

setwd("//192.168.209.12/karvy analytics/Healthcare/POCs/Procedure Forecast/Primary Document/Critical Docs/Forecast_Daily_test1")
getwd()
dir()

data = read.csv("Procedure Forecast_Input Test.csv")
#View(data)
str(data)-**

#Change the format of the date for the "date" column
data$Date = as.Date(data$Date,format = "%m/%d/%Y")
View(data)

####Convert the data into time series format for procedureA   ####
#xts: converting to integer format
data_procedureA = xts(data$ProcedureA,data$Date)
plot.ts(data_procedureA,main='procedureA')
#plot.ts = plotting time series data where x axis takes time by default
#ts.plot(data_procedureA)

#Plot the histogram for procedureA  
truehist(data_procedureA)
View(data_procedureA)

#Decompose the time series data into seasonal and trend components

#converting the data with frequency
procedureA_freq <- ts(data_procedureA, frequency=7)
print(procedureA_freq)

#decomposing the data
procedureA_comp = decompose(procedureA_freq)
plot(procedureA_comp)

#Remove the seasonal component from times series data and plot it
procedureA_adjust = procedureA_freq - procedureA_comp$seasonal
plot(procedureA_adjust)
plot(procedureA_comp$seasonal)

#Remove the seasonal and random component from times series data and plot it
procedureA_adjust_random = procedureA_adjust - procedureA_comp$random
plot(procedureA_adjust_random)

#Save the actual data without any transformation
data_procedureA_actual = data_procedureA

#Remove the seasonal component from the data to apply ARIMA modelling
data_procedureA = ceiling(procedureA_adjust)


#data_dissatified is the data points after removing seasonal component

###################################################################
#procedureA_freq2 <- ts(data_procedureA, frequency=4)
#plot(procedureA_freq2)
#decompose_procedureA_freq2 = decompose(procedureA_freq2)
#plot(decompose_procedureA_freq2)
#Next step is substracting the seasonal component


#procedureA_freq3 <- ts(procedureA_freq2, frequency=3)
#plot(procedureA_freq3)
#decompose_procedureA_freq3 = decompose(procedureA_freq3)
#plot(decompose_procedureA_freq3)

###################################################################
#Apply Box-cox transformation on y to stabilize the variance
#In the below formula, we do not have any independent variable x, on which y will depend
#View(data_procedureA)
plot(data_procedureA)
#data_procedureA ~ 1 is selecting only one variable: Y
bc = boxcox(data_procedureA ~ 1)
lam1<-bc$x[which.max(bc$y)]
lam1

#Plot the transformed data after applying boxcox transformation with the lambda identified
truehist(bxcx(data_procedureA,lam1))

data_procedureA = bxcx(data_procedureA,lam1)
plot.ts(data_procedureA,main = "Box-cox Transformed data - procedureA  ")

## For adf test, Null hypothesis is Time series data is not stationary
adf.test(data_procedureA)
#We have rejected Null hypothesis that the data is not stationary since p-value is found to be less than 0.05. Hence data is stationary


##Check out the values of p and q for applying ARIMA modelling
acf(data_procedureA, lag.max=20) # plot a correlogram
pacf(data_procedureA, lag.max=20) # plot a partial correlogram


#test analysis using various ARIMA models:

tfit1 = arima(data_procedureA, order=c(1,0,1))
summary(tfit1)
tsdiag(tfit1)
tfit1

tfit2 = arima(data_procedureA, order=c(1,0,0))
summary(tfit2)
tsdiag(tfit2)
tfit2

tfit3 = arima(data_procedureA, order=c(0,0,1))
summary(tfit3)
tsdiag(tfit3)
tfit3

tfit4 = arima(data_procedureA, order=c(0,0,0))
summary(tfit4)
tsdiag(tfit4)
tfit4

#final model for ARIMA: 

#Apply ARIMA modelling
fit = auto.arima(data_procedureA)
summary(fit)
fit
tsdiag(fit)

#Forecast the next 7 days values using the fitted ARIMA model
procedureA_forecast = forecast.Arima(fit, h=7)

#Plot the forecasted vs real values
plot.forecast(procedureA_forecast)
lines(fitted(fit),col="red")

#Apply inverse boxcox for the forecasted values
procedureA_forecast = ((procedureA_forecast$mean)*lam1 + 1)^(1/lam1)
k = procedureA_comp$seasonal[1:7]

procedureA_forecast = as.numeric(procedureA_forecast)
###Add the seasonal component in the forecasted values
#ceiling and floor fucntions are used for rounding off to higher and lower integer to the decimal value
procedureA_forecast = ceiling(procedureA_forecast + k)
procedureA_forecast
class(procedureA_forecast)

#link to date with 24/09/2015 as starting date

plot(data_procedureA_actual)

plot(procedureA_forecast)

data_procedureA = bxcx(data_procedureA,lam1,InverseQ = TRUE)
data_procedureA = floor(data_procedureA + procedureA_comp$seasonal)
plot(data_procedureA)
