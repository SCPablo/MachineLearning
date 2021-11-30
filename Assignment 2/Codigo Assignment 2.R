
# Librerias

library(fpp2)
library(ggplot2)
library(MLTools)
library(readxl)
library(lmtest)

# Leemos los datos

fdata <- read.table("UnemploymentSpain.dat",header = TRUE)

# Pasamos a serie temporal

fdata_ts <- ts(fdata$TOTAL,start = 2001, frequency = 12)

# Representamos

autoplot(fdata_ts) +
  ggtitle("Unemployment in Spain") +
  xlab("Year") + ylab("Number unemployed")

#####################################
# Apartado 1
#####################################

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

#####################################
# Apartado 2
#####################################


