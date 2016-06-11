# Import the necessary libraries

library(quantmod)
library(lattice)
library(timeSeries)
# For Mac OS, please install [https://www.xquartz.org/]
library(rugarch)

# Obtain the S&P500 returns and truncate the NA value
getSymbols("^GSPC", from="1950-01-01")
spReturns = diff(log(Cl(GSPC)))
#anyNA(spReturns)
spReturns[as.character(head(index(Cl(GSPC)),1))] = 0 # The first value is NA, so it's replaced with 0

# Create the forcasts vector to store the predictions
windowsLength = 500
foreLength = length(spReturns) - windowsLength
forecasts <- vector(mode="character", length=foreLength)
#for (d in 0:foreLength){
for (d in 0:4){
  # Obtain the S&P rolling window for this day
  spReturnOffset = spReturns[(1+d):(windowsLength+d)]
  
  # Fit the ARIMA model
  final.aic <- Inf
  final.order <- c(0,0,0)
  for (p in 0:5) for (q in 0:5) {
    if( p == 0 && q == 0){
      next
    }
    arimaFit = tryCatch( arima(spReturnOffset, order = c(p,0,q)),
                         error = function(err) FALSE,
                         warning = function(err) FALSE) # Returning FALSE
    if(!is.logical( arimaFit )){
      current.aic <- AIC(arimaFit)
      if(current.aic < final.aic){
        final.aic <- current.aic
        final.order <- c(p, 0, q)
        final.arima <- arima(spReturnOffset, order=final.order)
      } 
      }else {
        next
      }
  }
    
    # Specify and fit the GARCH model
    #help("ugarchspec")
    #list(armaOrder=c(final.order[1], final.order[3]), include.mean =T)['armaOrder']
    spec = ugarchspec(
      variance.model = list(garchOrder = c(1,1)),
      mean.model = list(armaOrder=c(final.order[1], final.order[3]), include.mean =T),
      distribution.model = "sged"
    )
    #spec
    #help("ugarchfit")
    fit = tryCatch(
      ugarchfit(spec, spReturnOffset, solver = "hybrid"),
      error = function(e) e,
      warning = function(w) w
    )
    #fit
    # In the GARCH model does not converge, set the direction to "long" else
    # choose the correct forecast direction based on the returns prediction
    # Output the results to the screen and the forecasts vector
    if(is(fit, "warning")){
      forecast[d+1] = paste(index(spReturnOffset[windowsLength]), 1, sep=",")
      print(paste(index(spReturnOffset[windowsLength]), 1, sep=","))
    } else {
      fore = ugarchforecast(fit, n.ahead = 1)
      #fore@forecast
      ind = fore@forecast$seriesFor
      forecasts[d+1] = paste(colnames(ind), ifelse(ind[1]<0, -1, 1), sep=",")
      print(forecasts[d+1])
    }
  
}

write.csv(forecasts, file="forecasts.csv", row.names = FALSE)






