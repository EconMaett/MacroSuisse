# Useful function to calculate weighted mean of indexed series
calcIndex <- function(series, weights, baseY) {
  
  series <- ts_index(series, baseY) * 100
  
  Index <- sapply(X = seq_len(nrow(series)), FUN = function(i) {
    weighted.mean(x = as.matrix(series[i, ]), w = weights, na.rm = TRUE)
  })
  
  Index <- xts(Index, order.by = as.Date(index(series)))
  
  return(Index)
}

# END