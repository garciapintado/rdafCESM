getNrestarts <- function(times, dtREST=NULL) {
  # times:  POSIXct vector to be splitted into monthly chunks
  # dtREST: scalar constant monthly interval to restart a run
  # the function chek the times vector can be exactly divided by the requested dtREST montly interval

  runNmonths <- rep(NA,length(times)-1)

  if (is.null(dtREST))
    return(rep(0,length(runNmonths)))

  for (it in 1:(length(times)-1)) {
    runNmonths[it] <- elapsed_months(times[it],times[it+1])
  }
  if (any(runNmonths %% dtREST != 0))
    stop('getRestarts:: time intervals not exact multipliers of dtREST')
  nrestarts <- as.integer(runNmonths / dtREST) - 1
  return(nrestarts)
}

