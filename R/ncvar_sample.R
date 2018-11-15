ncvar_sample <- function(fnamep, queryDF) {
 # sample a netCDF file at specific variables and locations
 # fname:   complete name, including path to a netCDF file containing the requested information
 # queryNC: data.frame, with columns 'x','y','zl','vKIND'
 # this assumes POP2 netCDF format. It should be checked how the function applies to other CESM components or netCDF climate files
 
 # queryDF$vKIND assumes vKIND has a COMP.vname format. Thus the first 5 characters in vKIND are dropped to look up the requested variables 
 # the function also assumes coordinate order is  X-Y-Z-T (longitude, latitude, depth/height, time)
 #
 #require(ncdf4) 
 #require(splancs)

 nci <- nc_open(fnamep)
 ncv <- rep(NA, nrow(queryDF)) # vector with retrieved samples

 for (i in 1:length(ncv)) {
   vname <- substring(queryDF[i,'vKIND'],6)
   att  <- ncatt_get(nci, vname)
   coo  <- strsplit(att$coordinates, split=' ')[[1]]
   if (length(nci$var[[vname]]$dim) != length(coo))
     stop('ncvar_sample: error in coordinate identification')
   dimvals <- sapply(nci$var[[vname]]$dim,FUN=function(x){x$vals})
   names(dimvals) <- sapply(nci$var[[vname]]$dim,FUN=function(x){x$name})
   if ('time' %in% names(dimvals)) {
     if (length(dimvals$time) > 1)
       stop('ncvar_sample:: function not ready for more thant one time grids')
   }
   dims    <- sapply(nci$var[[vname]]$dim,FUN=function(x){x$len})
   start  <- rep(1,length(coo))
   start[1:2] <- as.numeric(queryDF[i,c('gidX','gidY')])
   if (!is.na(queryDF[i,'zl'])) # otherwise the third coordinate is [1-length] time
     start[3] <- queryDF[i,'zl']
   ncv[i] <-  ncvar_get(nci, vname, 
                        start, count=rep(1,length(start)))
 }
 nc_close(nci)
 return(ncv)
}
