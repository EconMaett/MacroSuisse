# Function to extract forecast error variance from a forecast object
# CI lower = y(t+h|t) - 1.96*sig(h)
# Therefore sig(h)^2 = [CI lower - y(t+h|t))/(-1.96)]^2
# Get exact percentile (1.96 yield basically the same)
getForecastVariance <- function(fcst) {
  z957 <- qnorm(0.975, 0, 1)
  sigh2 <- ((fcst$lower[, "95%"] - fcst$mean) / (-z957))^2
  return(sigh2)
}

# END