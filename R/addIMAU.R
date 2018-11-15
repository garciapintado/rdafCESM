addIMAU <- function(x, dsn) {
  # IMAU is either a list of length 6
  # to be written into the user_nl_pop2 file in the namelist &forcing_imau_nml 
  fname    <- 'user_nl_pop2'
  namelist <- 'forcing_imau_nml'
  m <- length(dsn)
  if (m > 1)
    cat('modifyCESM:: --ensemble input | m=',m,'--\n',sep='')

  if (length(x) != 6)
    stop('addIMAU: input not a 6-element list')
  vnames <- names(x)
  for (im in 1:m) {
    zo <- file(file.path(dsn[im],fname),'a')
    for (i in 1:length(x)) { # write variables for this ensemble member
      if (length(x[[i]]) == 1)
        val <- x[[i]]
      else if (length(x[[i]]) == m)
        val <- x[[i]][im]
      else
        stop('variable must be scalar or m-long')
      if (is.character(val)) {
        cat(vnames[i],"&",namelist," = '",val,"'\n",sep="", file=zo)
      } else if (is.logical(val)) {
        cat(vnames[i],"&",namelist," = .",val,".",           "\n",sep='', file=zo)
      } else {
       stop('addIMAU:: unknown variable TYPE')
      }
    }
    close(zo) 
  } # end for im
}
