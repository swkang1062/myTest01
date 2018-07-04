
library(tseries)
library(forecast)
library(fpp)

str(AirPassengers)

plot(AirPassengers)

adf.test(AirPassengers)
ts.plot(AirPassengers)

plot.ts(AirPassengers)
ts.plot(diff(AirPassengers))

ts.plot(log(AirPassengers))


acf(AirPassengers)
pacf(AirPassengers)


acf(log(AirPassengers))
pacf(log(AirPassengers))

acf(diff(AirPassengers))
pacf(diff(AirPassengers))


acf(diff(log(AirPassengers)))
pacf(diff(log(AirPassengers)))


acf(diff(log(AirPassengers), differences = 2))
pacf(diff(log(AirPassengers), differences = 2))


plot( stl(AirPassengers, s.window="periodic") )

stl_AirPassengers <- stl(AirPassengers,s.window = "periodic")

class(stl_AirPassengers$time.series)


acf(stl_AirPassengers$time.series[,"seasonal"])
pacf(stl_AirPassengers$time.series[,"seasonnal"])

acf(stl_AirPassengers$time.series[,"trend"])
pacf(stl_AirPassengers$time.series[,"trend"])

acf(stl_AirPassengers$time.series[,"remainder"])
pacf(stl_AirPassengers$time.series[,"remainder"])


?decompose
?stats
library(help = "stats")
add_AirPassenger <- decompose(AirPassengers, type = "additive")
mul_AirPassenger <- decompose(AirPassengers, type = "multiplicative")


str(add_AirPassenger)
str(mul_AirPassenger)
is.na(mul_AirPassenger$seasonal)
is.na(mul_AirPassenger$random)
?decompose
is.na( (stl_AirPassengers$time.series[,"seasonal"]) )

plot(add_AirPassenger)
plot(mul_AirPassenger)
plot(stl(AirPassengers, s.window = "periodic"))

names(add_AirPassenger)
add_AirPassenger$random

#----------- random
par(mfrow=c(2,1))
acf(add_AirPassenger$random, na.action = na.pass)
acf(mul_AirPassenger$random, na.action = na.pass)

pacf(add_AirPassenger$random, na.action = na.pass)
pacf(mul_AirPassenger$random, na.action = na.pass)

#-------- Trend
acf(add_AirPassenger$trend, na.action = na.pass)
acf(mul_AirPassenger$trend, na.action = na.pass)

pacf(add_AirPassenger$trend, na.action = na.pass)
pacf(mul_AirPassenger$trend, na.action = na.pass)


#--------- Seasonal
acf(add_AirPassenger$seasonal, na.action = na.pass)
acf(mul_AirPassenger$seasonal, na.action = na.pass)

pacf(add_AirPassenger$seasonal, na.action = na.pass)
pacf(mul_AirPassenger$seasonal, na.action = na.pass)


# --------------- auto. arima 비교
add_random_fit <- auto.arima(add_AirPassenger$random)

mul_random_fit <- auto.arima(mul_AirPassenger$random)

fit <- auto.arima(AirPassengers)

#---------------
par(mfrow=c(3,1))
plot( predict(add_random_fit, n.ahead = 12*5)$pred)
plot( predict(mul_random_fit, n.ahead = 12*5)$pred)
plot( predict(fit,            n.ahead = 12*4)$pred)

# -------------- forecast.... 
# 
plot( forecast(fit, h=12*5) )
plot( forecast(add_random_fit, h=12*5))
plot( forecast(mul_random_fit, h=12*5))

plot( forecast(add_AirPassenger$seasonal, h=12*5))
plot( forecast(mul_AirPassenger$seasonal, h=12*5))

plot( forecast(add_AirPassenger$trend, h=12*5))
plot( forecast(mul_AirPassenger$trend, h=12*5))

par(mfrow=c(1,1))

#--------- compose 해보자

# 1. add model
n_ahead = 12*5
str(add_fore_random)
add_fore_comp$x
add_fore_random <- forecast(add_AirPassenger$random  , h=n_ahead)
add_fore_trend  <- forecast(add_AirPassenger$trend   , h=n_ahead)
add_fore_season <- forecast(add_AirPassenger$seasonal, h=n_ahead)
add_fore_comp   <- add_fore_random
add_fore_comp$x <- add_fore_random$x + add_fore_trend$x + add_fore_season$x
add_fore_comp$mean <- add_fore_random$mean + add_fore_trend$mean + add_fore_season$mean
add_fore_comp$upper <- add_fore_random$upper + 
  add_fore_trend$upper + add_fore_season$upper
add_fore_comp$lower <- add_fore_random$lower + 
  add_fore_trend$lower + add_fore_season$lower


mul_fore_random <- forecast(mul_AirPassenger$random  , h=n_ahead)
mul_fore_trend  <- forecast(mul_AirPassenger$trend   , h=n_ahead)
mul_fore_season <- forecast(mul_AirPassenger$seasonal, h=n_ahead)
mul_fore_comp   <- mul_fore_random
mul_fore_comp$x <- mul_fore_random$x * mul_fore_trend$x * mul_fore_season$x
mul_fore_comp$mean <- mul_fore_random$mean * mul_fore_trend$mean * mul_fore_season$mean
mul_fore_comp$upper <- mul_fore_random$upper * 
  mul_fore_trend$upper * mul_fore_season$upper
mul_fore_comp$lower <- mul_fore_random$lower * 
  mul_fore_trend$lower * mul_fore_season$lower

par(mfrow=c(1,1))
plot(add_fore_comp)
plot(mul_fore_comp)


plot(stl(log(AirPassengers), s.window = "periodic"))
plot( forecast(log10(AirPassengers)) )
plot( forecast(AirPassengers))

forecast_log <- forecast( log10(AirPassengers))
forecast_unlog <- forecast_log
forecast_unlog$x <- 10^forecast_log$x
forecast_unlog$mean <- 10^forecast_log$mean
forecast_unlog$upper <- 10^forecast_log$upper
forecast_unlog$lower <- 10^forecast_log$lower
plot(forecast_unlog)

### ====================
## forecast 의 upper bound 보다 큰 값을 outlier로 볼 수 있을까?
##  현재 80%, 95% 가 보여진다...
##   95% 이상???
##
forecast_log$upper[,"95%"]


### time series에 변화가 있다면 이를 다시 모델에 반영해야...
##
library(changepoint)
?cpt

?tsoutliers
