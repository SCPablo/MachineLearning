#################################################################################
##############       LabPractice 4.5 Forecasting     ############################
##############     ----------- solution ---------    ############################
#################################################################################

library(MLTools)
library(fpp2)
library(ggplot2)
library(readxl)
library(lmtest)  #contains coeftest function
library(tseries) #contains adf.test function
library(forecast)

## Set working directory ---------------------------------------------------------------------------------------------

## Load custom functions ---------------------------------------------------------------
# source("ForecastingTools.R")

## Load dataset -------------------------------------------------------------------------------------------------------
fdata <- read_table2("Assignment 2/UnemploymentSpain.dat")
# fdata <- read.table("CarRegistrations.dat",header = TRUE)
# Convert to time series object
fdata_ts <- ts(fdata)
y <- fdata_ts[,2]
# for daily data
autoplot(y)


## Training and validation ------------------------------------------------------------
y.TR <- subset(y, end = length(y)-5*12) #Leave 5 years for validation
y.TV <- subset(y, start = length(y)-5*12+1)

## Identification and fitting frocess -------------------------------------------------------------------------------------------------------
autoplot(y.TR)
autoplot(y)

# Box-Cox transformation
Lambda <- BoxCox.lambda.plot(y.TR,12)
Lambda <- BoxCox.lambda.plot(y,12)
# Lambda <- BoxCox.lambda(y) #other option
z <- BoxCox(y.TR,Lambda)
autoplot(z)

# Differentiation: if the ACF decreases very slowly -> needs differenciation
ggtsdisplay(y,lag.max = 100)

# If differencing is needed
Bz <- diff(y,differences = 1)
ggtsdisplay(Bz,lag.max = 100) #differences contains the order of differentiation

# Seasonal Differentiation
# If differencing is needed
B12Bz <- diff(Bz, lag = 12, differences = 1)
ggtsdisplay(B12Bz,lag.max = 100)

# ACF and PACF of the time series -> identify significant lags and order
ggtsdisplay(B12Bz,lag.max = 100)


# Fit seasonal model with estimated order
arima.fit <- Arima(y.TR,
                   order=c(1,1,0),
                   seasonal = list(order=c(0,0,0),period=12),
                   lambda = Lambda,
                   include.constant = FALSE)
arima.fit <- Arima(y,
                   order=c(1,1,0),
                   seasonal = list(order=c(2,1,1),period=12),
                   include.constant = FALSE)
summary(arima.fit) # summary of training errors and estimated coefficients
coeftest(arima.fit) # statistical significance of estimated coefficients
autoplot(arima.fit) # root plot

# Check residuals
CheckResiduals.ICAI(arima.fit, bins = 100)
# If residuals are not white noise, change order of ARMA
ggtsdisplay(residuals(arima.fit),lag.max = 100)


# Check fitted
autoplot(y.TR, series = "Real")+
  forecast::autolayer(arima.fit$fitted, series = "Fitted")
autoplot(y, series = "Real")+
  forecast::autolayer(arima.fit$fitted, series = "Fitted")


# Perform future forecast
y_est <- forecast(arima.fit, h=1)
autoplot(y_est)

## Validation error for h = 1 -------------------------------------------------------------------------------------------------------
# Obtain the forecast in validation for horizon = 1 using the trained parameters of the model
y.TV.est <- y*NA
for (i in seq(length(y.TR)+1, length(y), 1)){# loop for validation period
  y.TV.est[i] <- forecast(subset(y,end=i-1), # y series up to sample i 
                          model = arima.fit,       # Model trained (Also valid for exponential smoothing models)
                          h=1)$mean                # h is the forecast horizon
}

#Plot series and forecast 
autoplot(y)+
  forecast::autolayer(y.TV.est)

#Compute validation errors
accuracy(y.TV.est,y)

y.TV.est


y_est[("mean")]
