library(quantmod)
library(lattice)
library(timeSeries)
library(rugarch)

getSymbols("^GSPC", from="1950-01-01")
spReturns = diff(log(Cl(GSPC)))
#anyNA(spReturns)
spReturns[as.character(head(index(Cl(GSPC)),1))] = 0 # The first value is NA, so it's replaced with 0

windowsLength = 500
foreLength = length(spReturns) - windowsLength
forecasts <- vector(mode="character", length=foreLength)
for (d in 0:foreLength){
  
}