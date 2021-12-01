#################################################################################
##############           S3 ARIMA II Example         ############################
##############     ----------- solution ---------    ############################
#################################################################################

library(MLTools)
library(fpp2)
library(ggplot2)
library(readxl)
library(lmtest)  #contains coeftest function
library(tseries) #contains adf.test function
library(readr)

## Set working directory ---------------------------------------------------------------------------------------------


## Load dataset -------------------------------------------------------------------------------------------------------

fdata <- UnemploymentSpain <- read_table2("Assignment 2/UnemploymentSpain.dat")
# Convert to time series object
fdata_ts <- ts(fdata)
# index to select a time series
y <- fdata_ts[,2]


## Identification and fitting frocess -------------------------------------------------------------------------------------------------------
autoplot(y)

# Box-Cox transformation
Lambda <- BoxCox.lambda.plot(y,12)
# Lambda <- BoxCox.lambda(y) #other option
z <- BoxCox(y,Lambda)
autoplot(z)

# Differentiation: if the ACF decreases very slowly -> needs differenciation
ggtsdisplay(y)
# Alternative test
adf.test(z, alternative = "stationary")
ndiffs(z)
# If differencing is needed
Bz <- diff(y,differences = 1)

# ACF and PACF of the time series -> identify significant lags and order
ggtsdisplay(Bz,lag.max = 25)

# Fit model with estimated order
arima.fit <- Arima(y,
                   order=c(1,1,0), 
                   include.constant = TRUE)
summary(arima.fit) #summary of training errors and estimated coefficients
coeftest(arima.fit) #statistical significance of estimated coefficients
autoplot(arima.fit) #root plot
# Check residuals
CheckResiduals.ICAI(arima.fit, bins = 100)
# If residuals are not white noise, change order of ARMA

#######

#Check  forecast
autoplot(y, series = "Real")+
  forecast::autolayer(arima.fit$fitted, series = "Fitted")

#Perform future forecast
y_est <- forecast(arima.fit, h=1)
autoplot(y_est)

y_est[("mean")]



