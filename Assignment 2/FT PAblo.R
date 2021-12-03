library(MLTools)
library(fpp2)
library(ggplot2)
library(TSA)
library(lmtest)  #contains coeftest function
library(tseries) #contains adf.test function
library(Hmisc) # for computing lagged variables

## Set working directory ---------------------------------------------------------------------------------------------


## Load dataset -------------------------------------------------------------------------------------------------------
fdata <- read.table("./datosCovid.txt",header = TRUE, sep = "")
colnames(fdata) <- c("DATE","TOTAL")
# Convert to time series object
fdata_ts <- ts(fdata[147:250,], frequency = 12, start = 2001)
y <- fdata_ts[,2]
x <- fdata_ts[,3]
# for daily data
autoplot(y)
# Create time series and scale values 
#y <- fdata_ts[,1]/10000





TF.fit <- arima(y,
                order=c(1,0,0),
                seasonal = list(order=c(0,0,0),period=12),
                xtransf = x,
                transfer = list(c(0,7)), #List with (r,s) orders
                include.mean = TRUE
)
summary(TF.fit) # summary of training errors and estimated coefficients
coeftest(TF.fit) # statistical significance of estimated coefficients
# Check regression error to see the need of differentiation
TF.RegressionError.plot(y,x,TF.fit,lag.max = 100)


#Tras haber hecho la primera ejecucición vemos claramente que tenemos que diferenciar la parte regular
TF.fit <- arima(y,
                order=c(1,1,0),
                seasonal = list(order=c(0,0,0),period=12),
                xtransf = x,
                transfer = list(c(0,7)), #List with (r,s) orders
                include.mean = TRUE
)
summary(TF.fit) # summary of training errors and estimated coefficients
coeftest(TF.fit) # statistical significance of estimated coefficients
# Check regression error to see the need of differentiation
TF.RegressionError.plot(y,x,TF.fit,lag.max = 100)

#Ahora vemos que tenemos que diferenciar la estacionaria
TF.fit <- arima(y,
                order=c(1,1,0),
                seasonal = list(order=c(1,1,0),period=12),
                xtransf = x,
                transfer = list(c(0,7)), #List with (r,s) orders
                include.mean = TRUE,
                method="ML"
)
summary(TF.fit) # summary of training errors and estimated coefficients
coeftest(TF.fit) # statistical significance of estimated coefficients
# Check regression error to see the need of differentiation
TF.RegressionError.plot(y,x,TF.fit,lag.max = 100)




#Vemos que ya se ha corregido los errores pero sigue apareciendo un decrecimiento exponencial en forma senoidal por lo que volvemos a diferenciar en la parte regular
TF.fit <- arima(y,
                order=c(1,1,0),
                seasonal = list(order=c(1,1,0),period=12),
                xtransf = x,
                transfer = list(c(0,7)), #List with (r,s) orders
                include.mean = TRUE,
                method = "ML"
)
summary(TF.fit) # summary of training errors and estimated coefficients
coeftest(TF.fit) # statistical significance of estimated coefficients
# Check regression error to see the need of differentiation
TF.RegressionError.plot(y,x,TF.fit,lag.max = 200)
TF.Identification.plot(x,TF.fit)
CheckResiduals.ICAI(TF.fit)

xlag = Lag(x,0)   # b
xlag[is.na(xlag)]=0
TF.fit <- arima(y,
                order=c(1,1,1),
                seasonal = list(order=c(1,2,0),period=12),
                xtransf = xlag,
                transfer = list(c(3,1)), #List with (r,s) orders
                include.mean = TRUE,
                method = "ML"
)


autoplot(y, series = "Real")+
  forecast::autolayer(fitted(TF.fit), series = "Fitted")


arima.fit <- arima(y,
                   order=c(1,1,1),
                   seasonal = list(order=c(1,1,0),period=12),
                   include.mean = FALSE,
                   method="ML")

autoplot(y, series = "Real")+
  forecast::autolayer(sarima.fit$fitted, series = "Fitted")



#Prueba prediciendo y
fdata_ts <- ts(fdata[0:230,], frequency = 12, start = 2001)
y <- fdata_ts[,2]
x <- fdata_ts[,3]

arima.fit <- arima(y,
                   order=c(1,1,1),
                   seasonal = list(order=c(1,1,0),period=12),
                   include.mean = FALSE,
                   method="ML")

summary(arima.fit) # summary of training errors and estimated coefficients
coeftest(arima.fit) # statistical significance of estimated coefficients
# Check residuals
CheckResiduals.ICAI(arima.fit)
# If residuals are not white noise, change order of ARMA
ggtsdisplay(residuals(arima.fit),lag.max = 50)


y_est <- forecast(arima.fit, h=21)
autoplot(y_est)

for (i in seq(length(y.TR)+1, length(y), 1)){# loop for validation period
  y.TV.est[i] <- forecast(subset(y,end=i-1), # y series up to sample i 
                          model = arima.fit,       # Model trained (Also valid for exponential smoothing models)
                          h=1)$mean                # h is the forecast horizon
}

autoplot(y)+
  forecast::autolayer(y_est$mean)






