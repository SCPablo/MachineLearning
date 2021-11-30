
# Librerias

library(fpp2)
library(ggplot2)
library(MLTools)
library(readxl)
library(lmtest)  
library(tseries) #contains adf.test function
library(Hmisc)
library(tidyverse)


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
                   seasonal = list(order=c(2,1,1), period=12),
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



####################################
# Modelo 3
####################################

fdata_ts_cut <- window(fdata_ts, start=2011)

ggtsdisplay(fdata_ts_cut,lag.max = 25)

fdata_diff3 <- diff(fdata_ts_cut,differences = 1)
ggtsdisplay(fdata_diff3,lag.max = 100)

fdata_diff_esta2 <- diff(fdata_diff3, lag = 12, differences = 1)
ggtsdisplay(fdata_diff_esta2,lag.max = 100)

sarima2.fit <- Arima(fdata_ts_cut,
                    order=c(1,1,0),
                    seasonal = list(order=c(1,1,0), period=12),
                    include.constant = FALSE)
summary(sarima2.fit) 
coeftest(sarima2.fit)
autoplot(sarima2.fit) 


CheckResiduals.ICAI(sarima2.fit, bins = 100)

ggtsdisplay(residuals(sarima2.fit),lag.max = 100)

autoplot(fdata_ts_cut, series = "Real")+
  forecast::autolayer(sarima2.fit$fitted, series = "Fitted")


y3_est <- forecast(sarima2.fit, h=1)
autoplot(y3_est)

####################################
# Modelo 4
####################################

fdata2 <- fdata

fdata2  <- fdata2 %>%
  mutate(indice = seq(1:250)) %>%
  mutate(covid = ifelse(indice< 231,0,1)) %>%
  select(-indice,-DATE)

fdata2_ts <- ts(fdata2)
autoplot(fdata2_ts)

y <- fdata2_ts[,1]/1000000
x <- fdata2_ts[,2]

TF.fit <- arima(y,
                order=c(1,0,0),
                seasonal = list(order=c(1,0,0),period=24),
                xtransf = x,
                transfer = list(c(0,9)), #List with (r,s) orders
                include.mean = TRUE,
                method="ML")

summary(TF.fit)
coeftest(TF.fit)
TF.RegressionError.plot(y,x,TF.fit,lag.max = 200)
TF.Identification.plot(x,TF.fit)


xlag = Lag(x,0)   # b
xlag[is.na(xlag)]=0
arima.fit <- arima(y,
                   order=c(0,0,0),
                   seasonal = list(order=c(0,0,0),period=24),
                   xtransf = xlag,
                   transfer = list(c(1,1)), #List with (r,s) orders
                   include.mean = FALSE,
                   method="ML")
summary(arima.fit) # summary of training errors and estimated coefficients
coeftest(arima.fit) # statistical significance of estimated coefficients
# Check residuals
CheckResiduals.ICAI(arima.fit)
# If residuals are not white noise, change order of ARMA
ggtsdisplay(residuals(arima.fit),lag.max = 50)


### Cross correlation residuals - expl. variable
res <- residuals(arima.fit)
res[is.na(res)] <- 0
ccf(y = res, x = x)
########

# Check fitted
autoplot(y, series = "Real")+
  forecast::autolayer(fitted(arima.fit), series = "Fitted")