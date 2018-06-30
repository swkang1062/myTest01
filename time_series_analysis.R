
#############
##  시계열 분석
##    안정적 시계열
##    1. 시간에 따른 평균이 불변
##    2. 시간에 따른 분산이 불변
##    3. 시점에 따른 공분산이 불변

##   안정적이지 않은 경우, 안정적이 될 수 있도록 변환 후 시계열 적용

## ARIMA 분석 순서
##  1. 시각화하여 안정적 시계열인지 확인
##  2. 안정적 시계열 자료로 변환
##  3. ACF(Auto Correlation Function)/PACF(Partial Correlation Function)
##      혹은 auto.arima를 이용하여 최적화된 파라미터를 찾음
##  4. ARIMA 모형 구축
##  5. (필요한 경우) 미래에 추이에 대한 예측
install.packages("tseries")
install.packages("forecast")
install.packages("fpp")
library(tseries)
library(forecast)
library(fpp)

str(AirPassengers)

plot(AirPassengers)
## 시간에 따라 평균 증가
## 시간에 따라 분산 증가
## ==> 안정적이지 않음

## Seasionality, Trend, random 요소로 분해하여 그래프 그림
## stl(Seasonal Decomposition Of Time Series By Loess)
plot(stl(AirPassengers, s.window = "periodic"))

## 안정적 시계열인지 test
## adf(Augmented Dickey-Fuller Test)
## stationary.test 사용가능
##   default : adf.test
##   stationary.test(x, method = "pp")   : pp.test
##   stationary.test(x, method = "kpss") : kpss.test
adf.test(AirPassengers, alternative = "stationary")  ## p-value : 0.01
## Dickey-Fuller = -7.3186, Lag order = 5, p-value = 0.01
adf.test(log(AirPassengers))
# Dickey-Fuller = -6.4215, Lag order = 5, p-value = 0.01
adf.test(diff(AirPassengers))
# Dickey-Fuller = -7.0177, Lag order = 5, p-value = 0.01
## 안정적 시계열로 변환 : 차분(diff) 과 로그(log)
adf.test(diff(log(AirPassengers)), alternative = "stationary", k=0)
## Dickey-Fuller = -9.6003, Lag order = 0, p-value = 0.01
## p-value 는 모두 .... 0.01 인데... 
## adf 는 seasonality 와 trend 를 포함한 stationary를 테스트하는 것일까?
## ==> Lag 값과 Dickey-Fuller 값을 해석해야 하는 것이 아닐까??



Box.test(AirPassengers, lag=20, type = "Ljung-Box")
## kpss test 는 Null Hypothesis 가 stationary
## p 값이 작으면... not stationary 하고.. diff 가 필요하다는 것
kpss.test(AirPassengers)   # p-value = 0.01
# In kpss.test(AirPassengers) : p-value smaller than printed p-value
kpss.test(diff(log(AirPassengers))) ## p-value : 0.1 위의 값보다 나아졌다
# In kpss.test(diff(log(AirPassengers))) : p-value greater than printed p-value

### ARIMA 모델에 필요한 3가지 parameter
###  p : previous - p 시점 전의 값이 영향을 미침        AR(p)
###  q : q(??) 시점까지의 moving average가 영향을 미침  MA(q)
###  d : diff(?)  d번 차분하면 stationary 
##   p 차수, q 차수, d 차분을 결정하기 위해서는 
##   KPSS test4, ACF, PACF 를 그려보아야 함
##   R의 auto.arima를 통해 한번에 해결???
para_AirP <- auto.arima(AirPassengers)
#
# Series: AirPassengers 
# ARIMA(2,1,1)(0,1,0)[12] 
# Coefficients:
#  ar1     ar2      ma1
# 0.5960  0.2143  -0.9819
# s.e.  0.0888  0.0880   0.0292
# sigma^2 estimated as 132.3:  log likelihood=-504.92
# AIC=1017.85   AICc=1018.17   BIC=1029.35

para_diff_log_AirP <- auto.arima(diff(log(AirPassengers)))
# Series: diff(log(AirPassengers)) 
# ARIMA(0,0,1)(0,1,1)[12] 
# Coefficients:
#  ma1     sma1
# -0.4018  -0.5569
# s.e.   0.0896   0.0731
# sigma^2 estimated as 0.001369:  log likelihood=244.7
# AIC=-483.39   AICc=-483.2   BIC=-474.77


para_log_AirP <- auto.arima(log(AirPassengers))
para_log_AirP
# Series: log(AirPassengers) 
# ARIMA(0,1,1)(0,1,1)[12] 
# Coefficients:
#  ma1     sma1
# -0.4018  -0.5569
# s.e.   0.0896   0.0731
# sigma^2 estimated as 0.001371:  log likelihood=244.7
# AIC=-483.4   AICc=-483.21   BIC=-474.77

tsdiag(para_AirP)
tsdiag(para_diff_log_AirP)
tsdiag(para_log_AirP)
## 점차 증가/감소 가 없는 형태라서.... ???
## 대체로 가정을 만족한다???



## ARIMA 모형 만들기
fit1 <- arima(AirPassengers, c(2,1,1), seasonal=list(order=c(0,1,0), period= 12))

fit2 <- arima(diff(log(AirPassengers)), c(0,0,1), seasonal = list( order=c(0,1,1), period=12))

fit3 <- arima(log(AirPassengers), c(0,1,1), seasonal=list( order=c(0,1,1), period=12))

## 미래 추이 예측
par(mfrow= c(3,1))
pred1 <- predict(fit1, n.ahead =  10*12)
ts.plot(AirPassengers, pred1$pred, log="y")

pred2 <- predict(fit2, n.ahead = 10*12)
ts.plot(AirPassengers, 2.718^pred2$pred, log="y", lty=c(1,3))

pred3 <- predict(fit3, n.ahead = 10.*12)
ts.plot(AirPassengers, 2.718^pred3$pred, log="y")

## order decistion
##    PACF 값이 점선 넘어선 값 : pacf( data, lag.max =20 ) : AR(p)
##    ACF  값이 점선 넘어선 값 : acf (data,  lag.max = 20) : MA(q)
##    정상성을 보이는 diff     : diff(data, differences = n) : d
##       ==> diff 후에 plot.ts()로 plot 확인
##   seasonality 조정   :  fpp 패키지의 seasadj() 함수
##     계절 요인을 additive 혹은 multiplicative 하게 할 수 있음


plot.ts(diff(AirPassengers, differences = 1))  ## 이게 가장 좋아 보임 d = 1
plot.ts(diff(AirPassengers, differences = 2)) 
plot.ts(diff(AirPassengers, differences = 3))

diff1_AirPassengers <- diff(AirPassengers, differences = 1)
par(mfrow = c(1,1))
acf(diff1_AirPassengers, lag.max = 10 , )  # 다소 나가기는 하는데.. n = 2 이후
                                        # 점선 내...  MA(1)

pacf(diff1_AirPassengers, lag.max = 20)



##    

## 분해 시계열
##  1. Trend Factor
##  2. Seasonality Factor
##  3. Cyclical Factor
##  4. Irragular Factor
##

