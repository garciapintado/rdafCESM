whichMaxAMOC <- function(fname, dimbounds=NULL, moc_ic=1, moc_itr=2, it=1) {
  # fname  :: POP2 NetCDF file, including path, from which AMOC is obtained
  # latlim :: [\deg] latitudinal interval on which maximum AMOC is searched
  # zlim   :: [cm]   depth interval on which maximum AMOC is searched
  # moc_ic :: index within ["Eulerian Mean","Eddy-Induced (bolus)", "Submeso"]
  # moc_itr:: index within ["Global Ocean - Marginal Seas",
  #                         "Atlantic Ocean + Mediterranean Sea + Labrador Sea + GIN Sea + Arctic Ocean + Hudson Bay"]
  # yscl      :: 1.E-05 default to convert original [cm] to [km] in depth vector
    
  # CESM::POP2 MOC grid dimension names:
  # "lat_aux_grid moc_z moc_components transport_region time"
    
  REGION_MASK  <- NULL # nullify only to avoid compiler note on unexisting variables

  nci <- nc_open(fname) 
  vnames <- c('TLONG','TLAT','ULAT','REGION_MASK','lat_aux_grid','moc_z', 'MOC')             # support variables, each: [nlon,nlat]
  for (vname in vnames) {
    eval(parse(text=paste(vname,' <- list(); ',vname,'$att <- ncatt_get(nci,"',vname,'");',
                                               vname,'$vals <- ncvar_get(nci,"',vname,'")',sep='')))
  }; rm(vnames)

  dimnames <- c('lat_aux_grid','moc_z')  
  MOC$coordinates <- strsplit(MOC$att$coordinates,' ', fixed=TRUE)
  if (!all(names(dimnames) %in% MOC$coordinates))
    stop('getting MOC:: ---ERR001---')

  vKIND <- 'MOC'
  MOC$dim <- nci$var[[vKIND]]$dim
  if (length(dim(MOC$vals)) == 5)
    MOC$vals <- MOC$vals[,,,,it]
  MOC$vals <- MOC$vals[,,moc_ic,moc_itr]
  layer2D  <- MOC$vals # [n_auxlat,n_zt] = [nx,ny], where
  
  xseq <- MOC$dim[[1]]$vals          # lat_aux_grid
  yseq <- MOC$dim[[2]]$vals          # moc_z
  nx   <- length(xseq)               # [e.g. g37:105; g16:395]
  ny   <- length(yseq)               # [61] for both g37 & g16
  # ncDimnames <- sapply(nci$var[[vKIND]]$dim, function(x){x$name}) # == coordinates

  # get SN coordinates for default   
  TLAT$vals[REGION_MASK$vals != 6] <- NA # always take Atlantic bound even for global meridional circulation evaluation
  latSN <- range(TLAT$vals, na.rm=TRUE)  # Atlantic South and North latitude
  Sid <- which(xseq > latSN[1])[1] 
  Nid <- which(xseq > latSN[2])[1] - 1 

  if (is.null(dimbounds)) {
    dimbounds <- list()
    dimbounds[['x']] <- xseq[c(Sid,Nid)]               # latitude [\deg]
    dimbounds[['y']] <- c(500*100, Inf)                # depth     > 500m as default 
  }
    
  # make mask for max AMOC calculation
  diml <- list()
  mskl <- list()
  for (id in 1:length(dimnames)) {
    #if (MOC$dim[[id]]$name != dimnames[id]) {
    #  stop('whichMaxAMOC:: ---ERR002---') }
    diml[[id]] <- MOC$dim[[id]]$vals
    mskl[[id]] <- diml[[id]] >= min(dimbounds[[id]]) &
                  diml[[id]] <= max(dimbounds[[id]])
  }
  if (!is.null(layer2D)) {
    maxAMOC <- rDAF::gridMax(diml[[1]],diml[[2]],layer2D,
                                    mskl[[1]],mskl[[2]])
  }
  # image(lat_aux_grid$vals, moc_z$vals/100, MOC$vals, col=matlab.like(100), ylim=rev(range(moc_z$vals/100)))
  # abline(h=500*100, col='grey')
  # abline(v=dimbounds[['lat_aux_grid']], col='grey')
  Ssum <- sum(layer2D[Sid,])
  Smax <- max(layer2D[Sid,])
  Nsum <- sum(layer2D[Nid,])
  Nmax <- max(layer2D[Nid,])
  ans <- rbind(maxAMOC,
               c(xseq[Sid],NA,Ssum),
               c(xseq[Sid],yseq[which.max(layer2D[Sid,])],Smax),
               c(xseq[Nid],NA,Nsum),
               c(xseq[Nid],yseq[which.max(layer2D[Nid,])],Nmax))
  rownames(ans) <- c('max','Ssum','Smax','Nsum','Nmax')
  colnames(ans) <- c('lat_aux_grid','moc_z','val')
  
  nc_close(nci)
  return(ans)
} # end function whichMaxAMOC()

