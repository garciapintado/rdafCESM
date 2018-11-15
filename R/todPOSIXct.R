todPOSIXct <- function(x) {
  # time of day CESM envrun$START_TOD format
  # x :: POSIXct
  xf <- format(x, format='%H%M%S') 
  x <- as.numeric(substr(xf,1,2))*3600 + 
       as.numeric(substr(xf,3,4))*60 + 
       as.numeric(substr(xf,5,6))
  formatC(round(x), width=5, flag='0')
}


