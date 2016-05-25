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










