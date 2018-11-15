expandLMquery <- function(lmQuery, climf) {
  # expand a structured lmQuery list into a data.frame,
  # where each row is a specific request for linear modeling [or univariate covariance analysis]
  # the function does not interpolate into specific requested locations, but rather takes the nearest neighbour original cell in grid to
  # requested locations. So interpolation errors are avoided
  # a netcdf file is required with only purpose of retrieving metadata [numbers of layers]
  #require(splancs)
  #require(ncdf4) 

  queryList <- data.frame(x=numeric(0), y=integer(0), name=character(0),  vKIND=character(0), gidX=integer(0), gidY=integer(0),
                          z=numeric(0), zl=integer(0),
                          stringsAsFactors = FALSE)
  ir        <- 0
  nci <- nc_open(climf)

  vKINDs <- unique(as.vector(sapply(lmQuery,FUN=function(x){x$vKIND})))
  for (i in 1:length(lmQuery)) {
    nl <- length(lmQuery[[i]][['z']])
    nv <- length(lmQuery[[i]]$vKIND)
    for (iv in 1:nv) {
      vKIND <- lmQuery[[i]]$vKIND[iv]
      vname <- substring(vKIND,6)
      if (!(vname %in% names(nci$var)))
        stop('expandLMquery ---ERR001---')
      att <- ncatt_get(nci,vname)
      coo <- strsplit(att$coordinates, split=' ')[[1]]
      gpos <- cbind(as.numeric(ncvar_get(nci,coo[1])),as.numeric(ncvar_get(nci,coo[2]))) # [gLON,gLAT]
      dims <- sapply(nci$var[[vname]]$dim,FUN=function(x){x$len})
      cooz <- grep('z',coo)
      if (length(cooz) == 1) {
        is3D <- TRUE
        gz <- ncvar_get(nci,coo[cooz])
      } else {
        is3D <- FALSE
        gz <- NULL
      }
      gid  <- n2dist(gpos, matrix(lmQuery[[i]]$pos,1))$neighs                          # nearest neighbour index
      gids <- which(matrix(1:(dims[1]*dims[2]),dims[1],dims[2])==gid, arr.ind=TRUE)     # in geographical matrices
      for (il in 1:nl) {
        # cat('xpnd query: ',names(lmQuery)[[i]],' | vKIND:',vKIND,' | il:',il,'\n')
	if (il > 1 && !is3D)                                 # just do it once for the only available level for the variable
          break 
        ir <- ir + 1
        queryList[ir,c('x','y')] <- c(gpos[gid,1],gpos[gid,2])
        queryList[ir,'name']     <- names(lmQuery)[i]
        queryList[ir,'vKIND']    <- vKIND
        queryList[ir,'gidX']     <- gids[1]
        queryList[ir,'gidY']     <- gids[2]
        if (is3D) {
          zid <- which.min(abs(lmQuery[[i]]$z[il]*100 - gz)) # warning: assumes request in [m] and netCDF [cm]
          queryList[ir,'z']        <- gz[zid]/100 # [m]
          queryList[ir,'zl']       <- zid
        } else {
        #  queryList[ir,'z']        <- NA default
        #  queryList[ir,'zl']       <- NA
        }
      } # end il
    } # end iv
  } # end i 
  nc_close(nci)
  return(queryList)
}

