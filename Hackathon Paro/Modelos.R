
# Librerias

library(fpp2)
library(ggplot2)
library(MLTools)
library(readxl)
library(lmtest)


# Leemos los datos

fdata <- read.table("Paro_Espana.txt",header = TRUE)

# Pasamos a serie temporal

fdata_ts <- ts(fdata$TOTAL,start = 2001, frequency = 12)

# Representamos

autoplot(fdata_ts) +
  ggtitle("Unemployment in Spain") +
  xlab("Year") + ylab("Number unemployed")

####################################
# Modelo 1
####################################

ggtsdisplay(fdata_ts,lag.max = 25)

Lambda <- BoxCox.lambda.plot(fdata_ts,10)

adf.test(fdata_ts, alternative = "stationary")
ndiffs(fdata_ts)

fdata_diff <- diff(fdata_ts,differences = 1)
ggtsdisplay(fdata_diff,lag.max = 25)

arima.fit <- Arima(fdata_ts,
                   order=c(1,1,0), 
                   lambda = Lambda,
                   include.constant = TRUE)
summary(arima.fit) #summary of training errors and estimated coefficients
coeftest(arima.fit) #statistical significance of estimated coefficients
autoplot(arima.fit) #root plot
# Check residuals
CheckResiduals.ICAI(arima.fit, bins = 100)

autoplot(fdata_ts, series = "Real")+
  forecast::autolayer(arima.fit$fitted, series = "Fitted")

#Perform future forecast
fdata_est <- forecast(arima.fit, h=1)
autoplot(fdata_est)





####################################
# Modelo 2
####################################

ggtsdisplay(fdata_ts,lag.max = 25)

fdata_diff2 <- diff(fdata_ts,differences = 1)
ggtsdisplay(fdata_diff2,lag.max = 100)

# Notamos cierta estacionalidad...

fdata_diff_esta <- diff(fdata_diff2, lag = 12, differences = 1)
ggtsdisplay(fdata_diff_esta,lag.max = 100)

sarima.fit <- Arima(fdata_ts,
                   order=c(1,1,0),
                   seasonal = list(order=c(0,1,1), period=12),
                   include.constant = FALSE)
summary(sarima.fit) 
coeftest(sarima.fit)
autoplot(sarima.fit) 


CheckResiduals.ICAI(sarima.fit, bins = 100)

ggtsdisplay(residuals(sarima.fit),lag.max = 100)


autoplot(fdata_ts, series = "Real")+
  forecast::autolayer(sarima.fit$fitted, series = "Fitted")


y2_est <- forecast(sarima.fit, h=1)
autoplot(y2_est)

