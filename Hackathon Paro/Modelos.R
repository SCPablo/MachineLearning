
# Librerias

library(fpp2)
library(ggplot2)


# Leemos los datos

fdata <- read.table("Paro_Espana.txt",header = TRUE)

# Pasamos a serie temporal

fdata_ts <- ts(fdata$TOTAL,start = 2001, frequency = 12)

# Representamos

autoplot(fdata_ts) +
  ggtitle("Unemployment in Spain") +
  xlab("Year") + ylab("Number unemployed")

