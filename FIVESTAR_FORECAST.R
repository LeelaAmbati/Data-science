## DATA INTEGRATION

overall2010 <- read.csv("C:/Users/pullannagari.manasa/Desktop/starRatings/present/overall2010.csv")

overall2011 <- read.csv("C:/Users/pullannagari.manasa/Desktop/starRatings/present/overall2011.csv")

overall2012 <- read.csv("C:/Users/pullannagari.manasa/Desktop/starRatings/present/overall2012.csv")

overall2013 <- read.csv("C:/Users/pullannagari.manasa/Desktop/starRatings/present/overall2013.csv")

overall2014 <- read.csv("C:/Users/pullannagari.manasa/Desktop/starRatings/present/overall2014.csv")

overall2015 <- read.csv("C:/Users/pullannagari.manasa/Desktop/starRatings/present/overall2015.csv")

overall2016 <- read.csv("C:/Users/pullannagari.manasa/Desktop/starRatings/present/overall2016.csv")


foroverallrating<-rbind(overall2010,overall2011,overall2012,overall2013,overall2014,overall2015,overall2016)
#foroverallrating$year<-as.character(foroverallrating$year)
#foroverallrating$year = as.Date(foroverallrating$year,format = "%y")
#write.csv(foroverallrating,"C:/Users/pullannagari.manasa/Desktop/overall2126.csv")
#install.packages("xts")
#library("xts")
#data_procedureA = xts(foroverallrating$OverallRating,foroverallrating$year)
#dsts<-ts(foroverallrating$OverallRating)

#procedureA_freq <- ts(data_procedureA, frequency=365)
#procedureA_comp = decompose(procedureA_freq)
#plot(procedureA_comp)
#seasonplot(procedureA_freq,col=rainbow(12),year.labels=TRUE)
#tfit1=arima(procedureA_freq, order=c(1,0,1))
#prediction <- forecast.Arima(tfit1, h=3)
#plot.forecast(prediction)

# DEVIDING DATA INTO TRAIN AND TEST

train<-rbind(overall2010,overall2011,overall2012,overall2013,overall2014,overall2015)
test<-overall2016

#Change the format of the date for the "date" column
train$year<-as.character(train$year)
train$year<-as.Date(train$year,"%Y")


contract_numbers=unique(train$Contract.Number)
forecasted = data.frame(Contract.Number = factor(), rating = numeric())


for(i in 1:length(contract_numbers))
  
{
  
  
  contracter<-subset(train,train$Contract.Number==contract_numbers[i])
  library("xts")
  data_procedureA = xts(contracter$OverallRating,contracter$year)
  #plot.ts(data_procedureA,main='procedureA')
  
  
  #converting the data with frequency
  procedureA_freq <- ts(data_procedureA, frequency=3)
  #print(procedureA_freq)
  #decomposing the data
  procedureA_comp = decompose(procedureA_freq)
  #plot(procedureA_comp)
  #Remove the seasonal component from times series data and plot it
  procedureA_adjust = procedureA_freq - procedureA_comp$seasonal
  #plot(procedureA_adjust)
  #plot(procedureA_comp$seasonal)
  
  #Remove the seasonal and random component from times series data and plot it
  procedureA_adjust_random = procedureA_adjust - procedureA_comp$random
  #plot(procedureA_adjust_random)
  
  
  #Save the actual data without any transformation
  data_procedureA_actual = data_procedureA
  
  #Remove the seasonal component from the data to apply ARIMA modelling
  data_procedureA = ceiling(procedureA_adjust)
  
  
  
  ## For adf test, Null hypothesis is Time series data is not stationary
  library("tseries")
  #adf.test(rnorm(data_procedureA),k=0)
  ##Check out the values of p and q for applying ARIMA modelling
  #acf(data_procedureA, lag.max=20) # plot a correlogram
  #pacf(data_procedureA, lag.max=20) # plot a partial correlogram
  
  
  #test analysis using various ARIMA models:
  
  #tfit1 = arima(data_procedureA, order=c(0,0,0))
  #summary(tfit1)
  #tsdiag(tfit1)
  #tfit1
  
  #Forecast the next 3 year values using the fitted ARIMA model
  #library("forecast")
  #procedureA_forecast = forecast.Arima(tfit1, h=3)
  
  #Plot the forecasted vs real values
  #plot.forecast(procedureA_forecast)
  #lines(fitted(fit),col="red")
  #Plot the forecasted vs real values
  #plot.forecast(procedureA_forecast)
  #lines(fitted(tfit1),col="red")
  #library(forecast)
  fit1 <- ses(data_procedureA, alpha=0.2, initial="simple", h=3)
  
  ydr<-data.frame(fit1)
  f<-ydr[1,1]
  x<-data.frame(contracter$Contract.Number[1],f)
  
  forecasted<-rbind(forecasted,x)
}
 

#rounding the forecasted values

forecasted$f<-round(forecasted$f, digits = 1)
forecasted$f[forecasted$f>="3.0" & forecasted$f<="3.3"]<-3
 forecasted$f[forecasted$f>="3.4" & forecasted$f<="3.7"]<-"3.5"
forecasted$f[forecasted$f>="3.0" & forecasted$f<="3.3"]<-"3"
forecasted$f[forecasted$f>="3.8" & forecasted$f<="3.9"]<-"4"
forecasted$f[forecasted$f>="1.0" & forecasted$f<="1.3"]<-"1"
forecasted$f[forecasted$f>="1.4" & forecasted$f<="1.7"]<-"1.5"
forecasted$f[forecasted$f>="1.8" & forecasted$f<="1.9"]<-"2"
forecasted$f[forecasted$f>="2.0" & forecasted$f<="2.3"]<-"2"
forecasted$f[forecasted$f>="2.4" & forecasted$f<="2.7"]<-"2.5"
forecasted$f[forecasted$f>="2.8" & forecasted$f<="2.9"]<-"3"
forecasted$f[forecasted$f>="4.0" & forecasted$f<="4.3"]<-"4"
forecasted$f[forecasted$f>="4.4" & forecasted$f<="4.7"]<-"4.5"
forecasted$f[forecasted$f>="4.8" & forecasted$f<="4.9"]<-"5"
forecasted$f[forecasted$f>="5.0" & forecasted$f<="5.3"]<-"5"
forecasted$f[forecasted$f>="5.4" & forecasted$f<="5.7"]<-"5"
forecasted$f[forecasted$f>="5.8" & forecasted$f<="5.9"]<-"5"
forecasted$f[forecasted$f>="0.0" & forecasted$f<="0.9"]<-"0"
## EVALUATION
cm1<-table(test$OverallRating,forecasted$f)
print(cm1)
1-sum(diag(cm1))/sum(cm1)
library(caret) 
confusionMatrix(cm1)

