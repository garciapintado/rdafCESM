elapsed_months <- function(t0,t1) {
  # t0 :: POSIXct
  # t1 :: POSIXct
  # t1 >= t0

  t0 <- as.POSIXlt(t0)
  t1 <- as.POSIXlt(t1)
  12 * (t1$year - t0$year) + (t1$mon - t0$mon)
} # end function elapsed_months

