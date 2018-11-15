POSIXt2BPstr <- function(x) {
  # this function assumes that just months are significant in before present times 
  # thus just monthly values are returned by the function
  # x :: POSIXt [Anno Domini]
  xY <- -as.numeric(as.character(x, format='%Y')) + 1950
  xM <- as.character(x, format='%m')
  paste(xY,'-',xM,' BP',sep='')
}
