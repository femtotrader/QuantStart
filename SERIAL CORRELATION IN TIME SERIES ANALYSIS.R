setwd("/Users/hutianyou/Documents/Python/QuantStart")
getwd()
help(rnorm)
# Simple covariance
set.seed(1)
x = seq(1,100) + 20.0*rnorm(1:100)
set.seed(2)
y = seq(1,100) + 20.0*rnorm(1:100)
plot(x,y)
cov(x,y)
cor(x,y)

set.seed(1)
w = rnorm(1000)
acf(w)

#Random Walk
set.seed(4)
x = w = rnorm(1000)
for (t in 2:1000) x[t] = x[t-1] + w[t]
plot(x, type = 'l')
acf(x)
acf(diff(x))

install.packages('quantmod')
require('quantmod')
getSymbols('MSFT', src='yahoo')
MSFT
Op(MSFT)
acf(diff(Ad(MSFT)), na.action = na.omit)

getSymbols('^GSPC', src = 'yahoo')
acf(diff(Ad(GSPC)), na.action = na.omit)

#4.AUTOREGRESSIVE MOVING AVERAGE ARMA(P, Q) MODELS FOR TIME SERIES ANALYSIS - PART 1
###AR(p)
set.seed(1)
x = w =rnorm(100)
for (t in 2:100) x[t] = 0.6*x[t-1] + w[t]
layout(1:2)
plot(x, type='l')
acf(x)
x.ar = ar(x, method = 'mle')
x.ar$order
typeof(x.ar)
x.ar$ar
x.ar
help(ar)
x.ar$ar + c(-1.96, 1.96)*sqrt(x.ar$asy.var.coef)
makeAR1 = function(para){
  set.seed(1)
  x = w =rnorm(100)
  for (t in 2:100) x[t] = para*x[t-1] + w[t]
  layout(1:2)
  plot(x, type='l')
  acf(x)
  return (x)
}

x = makeAR1(-0.6)
x.ar = ar(x, method="mle")
x.ar$order
x.ar$ar
x.ar$ar + c(-1.96, 1.96)*sqrt(x.ar$asy)

makeAR2 = function(para1, para2){
  set.seed(1)
  x = w =rnorm(100)
  for (t in 3:100) x[t] = para1*x[t-1] +para2*x[t-2] + w[t]
  layout(1:2)
  plot(x, type='l')
  acf(x)
  return (x)
}

x = makeAR2(0.666, -0.333)
x.ar = ar(x, method="mle")
x.ar$order
x.ar$ar
require('quantmod')
getSymbols('AMZN')
help("getSymbols")
help(layout)
plot(Cl(AMZN))
amznrt = diff(log(Cl(AMZN)))
plot(amznrt)
acf(amznrt, na.action = na.omit)
amznrt.ar = ar(amznrt, na.action = na.omit)
amznrt.ar$order
amznrt.ar$ar
amznrt.ar$asy.var.coef[1,1]
amznrt.ar$ar[1] + c(-1.96,1.96)*sqrt(amznrt.ar$asy.var.coef[1,1])
amznrt.ar$ar[2] + c(-1.96,1.96)*sqrt(amznrt.ar$asy.var.coef[1,1])
#S&P500 US Equity Index
getSymbols("^GSPC")
GSPC
plot(Cl(GSPC))
gspcrt = diff(log(Cl(GSPC)))
plot(gspcrt)
acf(gspcrt, na.action = na.omit) # There is evidence of long memory
gspcrt.ar = ar(gspcrt, na.action = na.omit)
gspcrt.ar$order # 20, it tells taht there is likely a lot more complexity in the serial correlation than 
#a simple liner model of past prices

# 5.AUTOREGRESSIVE MOVING AVERAGE ARMA(P, Q) MODELS FOR TIME SERIES ANALYSIS - PART 2
# Simulations and Correlograms
#MA(1)
set.seed(1)
x <- w <- rnorm(100)
for (t in 2:100) x[t] <- w[t] + 0.6*w[t-1]
layout(1:2)
plot(x, type = 'l')
acf(x)
x.ma <- arima(x, order=c(0,0,1))
x.ma
0.6023 + c(-1.96,1.96)*0.0827 
# 0.440208 0.764392, 0.6 is in this interval, thus this model is a good fit.
# Let's make it a function
makeMA1 = function(para){
  set.seed(1)
  x <- w <- rnorm(100)
  for (t in 2:100) x[t] <- w[t] + para*w[t-1]
  layout(1:2)
  plot(x, type = 'l')
  acf(x)
  return (x)
}
x <- makeMA1(-0.6)
x.ma <- arima(x, order=c(0,0,1))
x.ma
-0.7298 + c(-1.96, 1.96)*0.1008
# MA(3)
set.seed(3)
x <- w <- rnorm(1000)
for (t in 4:1000) x[t] = w[t] + 0.6*w[t-1] + 0.4*w[t-2] + 0.3*w[t-3]
layout(1:2)
plot(x, type = 'l')
acf(x)
x.ma <- arima(x, order = c(0,0,3))
x.ma$coef[1] + c(-1.96,1.96)*0.030
x.ma$coef[2] + c(-1.96,1.96)*0.0349
x.ma$coef[3] + c(-1.96,1.96)*0.0318
# Fit a MA(1,2,3) model to AMZN data
# MA(1)
require(quantmod)
getSymbols("AMZN")
amznrt = diff(log(Cl(AMNZ)))
amznrt.ma <- arima(amznrt, order = c(0,0,1))
amznrt.ma
acf(amznrt.ma$res[-1])
#MA(2)
#MA(2) is capturing a lot os the autocorrelation, but not all of the long-memor effects
amznrt.ma <- arima(amznrt, order = c(0,0,2))
amznrt.ma
acf(amznrt.ma$res[-1])
#MA(3)
amznrt.ma <- arima(amznrt, order = c(0,0,3))
amznrt.ma
acf(amznrt.ma$res[-1])

#S&P500
#MA(1)
getSymbols("^GSPC")
gspcrt <- diff(log(Cl(GSPC)))
gspcrt.ma <- arima(gspcrt, order=c(0,0,1))
gspcrt.ma
acf(gspcrt.ma$res[-1])
#MA(2)
getSymbols("^GSPC")
gspcrt <- diff(log(Cl(GSPC)))
gspcrt.ma <- arima(gspcrt, order=c(0,0,2))
gspcrt.ma
acf(gspcrt.ma$res[-1])
#MA(3)
getSymbols("^GSPC")
gspcrt <- diff(log(Cl(GSPC)))
gspcrt.ma <- arima(gspcrt, order=c(0,0,3))
gspcrt.ma
acf(gspcrt.ma$res[-1])

#6.AUTOREGRESSIVE MOVING AVERAGE ARMA(P, Q) MODELS FOR TIME SERIES ANALYSIS - PART 3
# ARMA(1,1)
set.seed(1)
x <- arima.sim(n=1000, model=list(ar=0.5, ma=-0.5))
plot(x)
acf(x)
arima(x, order=c(1,0,1))
# ARMA(2,2)
set.seed(1)
x <- arima.sim(n=1000, model=list(ar=c(0.5, -0.25), ma=c(0.5, -0.3)))
plot(x)
acf(x)
arima(x, order=c(2,0,2))
# Choosing the best model
set.seed(3)
x <- arima.sim(n=1000, model=list(ar=c(0.5,-0.25,0.4), ma=c(0.5, -0.3)))

final.aic <- Inf
final.order <- c(0,0,0)
for (i in 0:4) for (j in 0:4){
  current.aic <- AIC(arima(x, order = c(i, 0, j)))
  if (current.aic < final.aic){
    final.aic <- current.aic
    final.order <- c(i, 0, j)
    final.arma <- arima(x, order=final.order)
  }
}
final.aic
final.order
final.arma
# The following two formulas are consistent
acf(resid(final.arma))
acf(final.arma$res)
Box.test(resid(final.arma), lag = 20, type="Ljung-Box")
# Financial Data
require(quantmod)
getSymbols("^GSPC")
sp = diff(log(Cl(GSPC)))

spfinal.aic <- Inf
spfinal.order <- c(0, 0, 0)
for (i in 0:4) for (j in 0:4){
  spcurrent.aic <- AIC(arima(sp, order=c(i, 0, j)))
  if(spcurrent.aic < spfinal.aic){
    spfinal.aic <- spcurrent.aic
    spfinal.order <- c(i,0,j)
    spfinal.arma <- arima(sp, order=c(i, 0, j))
  }
}
spfinal.order
acf(resid(spfinal.arma), na.action = na.omit)
Box.test(resid(spfinal.arma), lag=20, type="Ljung-Box")
###Hence there is additional autocorrelation in the residuals that is not explained by the fitted ARMA(3,3) model.

#6.AUTOREGRESSIVE INTEGRATED MOVING AVERAGE ARIMA(P, D, Q) MODELS FOR TIME SERIES ANALYSIS
set.seed(2)
x <- arima.sim(list(order=c(1,1,1), ar=0.6, ma=-0.5), n=1000)
plot(x)
x.arima <- arima(x, order=c(1, 1, 1))
x.arima
0.6470 + c(-1.96, 1.96)*0.1065
-0.5165 + c(-1.96, 1.96)*0.1189
acf(resid(x.arima))
Box.test(resid(x.arima), lag=20, type="Ljung-Box")
##Financial Data
install.packages("forecast")
library(forecast)
help(library)
require(quantmod)
getSymbols("AMZN", from="2013-01-01")
amzn = diff(log(Cl(AMZN)))
azfinal.aic <- Inf
azfinal.order <- c(0,0,0)
for (p in 1:4) for (d in 0:1) for ( q in 1:4) {
  azcurrent.aic = AIC(arima(amzn, order=c(p,d,q)))
  if(azcurrent.aic < azfinal.aic){
    azfinal.aic = azcurrent.aic
    azfinal.order = c(p,d,q)
    azfinal.arima = arima(amzn, order = azfinal.order)
  }
}
azfinal.order
acf(resid(azfinal.arima), na.action = na.omit)
Box.test(resid(azfinal.arima), lag=20, type="Ljung-Box")
plot(forecast(azfinal.arima, h=25))

getSymbols("^GSPC", from="2013-01-01")
sp = diff(log(Cl(GSPC)))
spfinal.aic <- Inf
spfinal.order <- c(0,0,0)
for (p in 1:4) for (d in 0:1) for ( q in 1:4) {
  spcurrent.aic = AIC(arima(sp, order=c(p,d,q)))
  if(spcurrent.aic < spfinal.aic){
    spfinal.aic = spcurrent.aic
    spfinal.order = c(p,d,q)
    spfinal.arima = arima(sp, order = spfinal.order)
  }
}
spfinal.order
acf(resid(spfinal.arima), na.action = na.omit)
Box.test(resid(spfinal.arima), lag=20, type="Ljung-Box")
plot(forecast(spfinal.arima, h=25))


