pib1=ts(pib,start=c(2010,1),frequency=4)
pib1
ts.plot(pib1)
monthplot(pib1)
pib1_diff=diff(pib1)
ts.plot(pib1_diff)
monthplot(pib1_diff)
library(aTSA)
adf.test(pib1)

acf(pib1_diff,lag=30)[1:10]

pacf(pib1_diff,lag=30)

modelo1=arima(pib1,order=c(0,1,0),seasonal=list(order=c(1,0,0),period=4))
modelo1
residuos1=residuals(modelo1)
acf(residuos1,lag=30)

predict(modelo1,8)
forecast(modelo1,8)

beer1=ts(beer,start=c(2012,1),frequency=12)
ts.plot(beer1)
beer1

length(beer1)
t=1:140
linear=lm(beer1~t)
summary(linear)
Y1=residuals(linear)
ts.plot(Y1)

grupos=matrix(1,140,1)
grupos[71:140]=2
library(lawstat)
levene.test(Y1,grupos,location="mean")

Y=ts(Y1,start=c(2012,1),
     frequency=12)
ts.plot(Y)
monthplot(Y)

acf(Y,lag=60)

Y_diff12=diff(Y,lag=12)
acf(Y_diff12,lag=60)
pacf(Y_diff12,lag=60)

modelo1=arima(Y,order=c(0,0,0),
              seasonal=list(order=c(1,1,0),
              frequency=12))
modelo1
residuos1=residuals(modelo1)
acf(residuos1,lag=60)

modelo2=arima(Y,order=c(0,0,0),
              seasonal=list(order=c(0,1,1),
                            frequency=12))
modelo2
residuos2=residuals(modelo2)
acf(residuos2,lag=60)
pacf(residuos2,lag=60,ylim=c(-0.2,1))

modelo3=arima(Y,order=c(3,0,0),
          seasonal=list(order=c(0,1,1),
                      frequency=12))
modelo3

modelo4=arima(Y,order=c(1,0,0),
          seasonal=list(order=c(0,1,1),
                     frequency=12))
modelo4
residuos4=residuals(modelo4)
acf(residuos4,lag=60)

Box.test(residuos4,lag=7)


modelo5=arima(Y,order=c(13,0,0),
              seasonal=list(order=c(0,1,1),
                            frequency=12),
  fixed=c(NA,0,0,0,0,0,NA,0,0,0,0,0,NA,NA))
modelo5

modelo6=arima(Y,order=c(13,0,0),
              seasonal=list(order=c(0,1,1),
                            frequency=12),
              fixed=c(NA,0,0,0,0,0,0,0,0,0,0,0,NA,NA))
modelo6
residuos6=residuals(modelo6)
acf(residuos6,lag=40)
Box.test(residuos6,lag=15)

modelo7=arima(Y,order=c(15,0,0),
              seasonal=list(order=c(0,1,1),
                            frequency=12),
              fixed=c(NA,0,0,0,0,0,0,0,0,0,0,0,NA,0,NA,NA))
modelo7
residuos7=residuals(modelo7)
acf(residuos7,lag=40)
Box.test(residuos7,lag=5)

modelo8=arima(Y,order=c(0,0,15),
              seasonal=list(order=c(0,1,1),
                            frequency=12),
              fixed=c(NA,0,0,0,0,0,0,0,0,0,0,0,NA,0,NA,NA))
modelo8
residuos8=residuals(modelo8)
acf(residuos8,lag=40)
Box.test(residuos8,lag=3)

modelo9=arima(Y,order=c(0,0,15),
              seasonal=list(order=c(0,1,1),
                            frequency=12),
              fixed=c(NA,0,NA,0,0,0,0,0,0,0,0,0,NA,0,NA,NA))
modelo9
residuos9=residuals(modelo9)
acf(residuos9,lag=40)


forecast(modelo9,24)
linear
16519752-14805*142
14417442-862001
