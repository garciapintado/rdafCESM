whichMaxAMOC <- function(fname, dimbounds=NULL, moc_ic=1, moc_itr=2, getExp = 'S') {
  # fname  :: POP2 NetCDF file, including path, from which AMOC is obtained
  # latlim :: [\deg] latitudinal interval on which maximum AMOC is searched
  # zlim   :: [cm]   depth interval on which maximum AMOC is searched
  # moc_ic :: index within ["Eulerian Mean","Eddy-Induced (bolus)", "Submeso"]
  # moc_itr:: index within ["Global Ocean - Marginal Seas",
  #                         "Atlantic Ocean + Mediterranean Sea + Labrador Sea + GIN Sea + Arctic Ocean + Hudson Bay"]

  # require(ncdf4)  
  # require(rDAF)
    
  # CESM::POP2 AMOC grid dimension names:
  # "lat_aux_grid moc_z moc_components transport_region time"
    
  nci <- nc_open(fname)

  REGION_MASK <- NULL # nullify only to avoid compiler note on unexisting variables
  lat_aux_grid <- NULL
  
  vnames <- c('TLONG','TLAT','ULAT','REGION_MASK','lat_aux_grid','moc_z', 'MOC')             # support variables, each: [nlon,nlat]
  for (vname in vnames) {
    eval(parse(text=paste(vname,' <- list(); ',vname,'$att <- ncatt_get(nci,"',vname,'");',
                                               vname,'$vals <- ncvar_get(nci,"',vname,'")',sep='')))
  }; rm(vnames)

  dimnames <- c('lat_aux_grid','moc_z')  
  MOC$coordinates <- strsplit(MOC$att$coordinates,' ', fixed=TRUE)
  if (!all(names(dimnames) %in% MOC$coordinates))
    stop('whichMaxAMOC:: ---ERR001---')
  # ncDimnames <- sapply(nci$var[[vKIND]]$dim, function(x){x$name}) # == coordinates
  MOC$dim <- nci$var[['MOC']]$dim
  MOC$vals <- MOC$vals[,,moc_ic,moc_itr]

  regmsk <- ifelse(moc_itr == 2, 6, NULL)
  TLAT$vals[REGION_MASK$vals != 6] <- NA # always take Atlantic bound even for global meridional circulation evaluation
  latSN <- range(TLAT$vals, na.rm=TRUE)  # Atlantic South and North latitude
  Sid <- which(lat_aux_grid$vals > latSN[1])[1] 
  Nid <- which(lat_aux_grid$vals > latSN[2])[1] - 1 

  if (is.null(dimbounds)) {
    dimbounds <- list()
    dimbounds[['lat_aux_grid']] <- lat_aux_grid$vals[c(Sid,Nid)]             # [\deg]
    dimbounds[['moc_z']]        <- c(500, Inf)*100                           # [cm] 
  }
    
  # make mask for max AMOC calculation
  diml <- list()
  mskl <- list()
  for (id in 1:length(dimnames)) {
    if (MOC$dim[[id]]$name != dimnames[id]) {
      stop('whichMaxAMOC:: ---ERR002---') }
    diml[[dimnames[id]]] <- MOC$dim[[id]]$vals
    mskl[[dimnames[id]]] <- diml[[dimnames[id]]] >= min(dimbounds[[dimnames[id]]]) &
                            diml[[dimnames[id]]] <= max(dimbounds[[dimnames[id]]])
  }
  if (!is.null(MOC$vals)) {
    maxAMOC <- rDAF::gridMax(diml[[1]],diml[[2]],MOC$vals,
                                    mskl[[1]],mskl[[2]])
  }
  # image(lat_aux_grid$vals, moc_z$vals/100, MOC$vals, col=matlab.like(100), ylim=rev(range(moc_z$vals/100)))
  # abline(h=500*100, col='grey')
  # abline(v=dimbounds[['lat_aux_grid']], col='grey')
  names(maxAMOC) <- c('maxlat','maxz','maxval')
  
  if (!is.null(getExp)) {
    namesat <-  paste(c('S.min:','N.max'),dimbounds[['lat_aux_grid']])
    

  } 
  nc_close(nci)
  return(maxAMOC)
} # end function whichMaxAMOC()

