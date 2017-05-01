library("lmtest")
library('forecast')
library('car')
library('TTR')
library('itsmr')
setwd("/Users/macuser/Documents/TimeSeries")
globTemp <- read.table("report3Data.txt",sep="\t", header=TRUE)
x <- globTemp[["year"]]
y <- globTemp[["annual_anom"]]
z <- globTemp[["actualfuture"]]
plot(x, y, type = "b", xlab = "year", ylab = "Difference from Baseline", col = rainbow(90))
linearFit <- lm(y ~ x)
linearFit
durbinWatsonTest(linearFit)
globTempTimeSeries <- ts(y, frequency=1, start=c(1880,1), end = c(2010,1))
globTempExtend <- ts(z, frequency= 1, start = 2011, end = 2015)
print(globTempTimeSeries)
globTempTimeSeries
plot.ts(globTempTimeSeries, xlab = "year", ylab = "Difference from Baseline", cex=3.0)
ySMA3 <- SMA(globTempTimeSeries,n=10)
plot.ts(ySMA3)
ts.plot(globTempTimeSeries, ySMA3, xlab = "year", ylab = "Difference from Baseline",gpars= list(lty= c(1,5), lwd =c(1,2)))
globTempDif1 <- diff(globTempTimeSeries, differences=1)
plot.ts(globTempDif1, xlab = "year", ylab = "First Differences")
firstAcf <- acf(globTempTimeSeries, lag.max = 20, lwd = 10, col = rainbow(7, start = .5, end = 0))
acf(globTempDif1, lag.max=20, lwd = 10)             
acf(globTempDif1, lag.max=20, plot=FALSE)
pacf(globTempDif1, lag.max=20, lwd = 10)             
pacf(globTempDif1, lag.max=20, plot=FALSE)
globTempDif2 <- diff(globTempTimeSeries, differences=2)
acf(globTempDif2, lag.max=20, lwd = 10)             
acf(globTempDif2, lag.max=20, plot=FALSE)
pacf(globTempDif2, lag.max=20, lwd = 10)             
pacf(globTempDif2, lag.max=20, plot=FALSE)
globTempArima <- auto.arima(globTempTimeSeries, ic = "aic", trace = TRUE, approximation = FALSE)
globTempArima
library("forecast")
globTempforecasts <- forecast.Arima(globTempArima, h=10)
globTempforecasts
plot.forecast(globTempforecasts, xlim=c(1880, 2020), ylab = "Difference from Baseline", pi.col = 225, fcol = 225, flty = 3, cex = 1.5)
points(x, z, type='l', lwd = 4)
ar(x, aic = TRUE, order.max = NULL,
   method = c("yule-walker", "burg", "ols", "mle", "yw"))
yw(x,1)