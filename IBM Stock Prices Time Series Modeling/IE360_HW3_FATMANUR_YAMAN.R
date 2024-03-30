install.packages("readxl")
library(readxl)
install.packages("tseries")
library(tseries)
install.packages("forecast")
library(forecast)

data<-read_excel('C:\\Users\\fatma\\Desktop\\IBMStocks.xls')
head(data)

data.ts<-ts(data$'IBM Stock Price')
data
#Plot
?lot(data.ts)
#Autocorrelation Function
acf(data.ts)
#Partial Autocorrelation
pacf(data.ts)
#Test
adf.test(data.ts)

#Taking the first difference.
data.ts.diff<-diff(data.ts)
plot(data.ts.diff)
acf(data.ts.diff)
pacf(data.ts.diff)
adf.test(data.ts.diff)

#T?king the second difference.
data.ts.diff.diff<-diff(data.ts.diff)
plot(data.ts.diff.diff)
acf(data.ts.diff.diff)
pacf(data.ts.diff.diff)
adf.test(data.ts.diff.diff)


#Fit ARIMA(0,2,1) model.
data.diff.model<-Arima(data.ts, order=c(0,2,1))
data.diff.model
?sdisplay(data.diff.model$residuals)
#Fit auto.arima model.
data.diff.auto<-auto.arima(data.ts)
data.diff.auto
tsdisplay(data.diff.auto$residuals)

#Forecast.
data.ts.forecast<-forecast(data.diff.model)
data.ts.forecast
plot(data.ts.forecast)

