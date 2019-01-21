whichMaxAMOC <- function(fname, dimbounds=NULL, moc_ic=1, moc_itr=2) {
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
  if (is.null(dimbounds)) {
    dimbounds <- list()
    dimbounds[['lat_aux_grid']] <- c(0, 50)              # [\deg]
    dimbounds[['moc_z']]        <- c(500, Inf)*100       # [cm] 
  }
    
  dimnames <- c('lat_aux_grid','moc_z')
  # retrieve metadata
  nci <- nc_open(fname)
  vKIND <- 'MOC'
  MOC <- list()
  MOC$att         <- ncatt_get(nci,varid='MOC')
  MOC$coordinates <- strsplit(MOC$att$coordinates,' ', fixed=TRUE)
  if (!all(names(dimnames) %in% MOC$coordinates))
    stop('whichMaxAMOC:: ---ERR001---')
  # ncDimnames <- sapply(nci$var[[vKIND]]$dim, function(x){x$name}) # == coordinates
  MOC$dim <- nci$var[[vKIND]]$dim

  # make mask
  diml <- list()
  mskl <- list()
  for (id in 1:length(dimnames)) {
    if (MOC$dim[[id]]$name != dimnames[id]) {
      stop('whichMaxAMOC:: ---ERR002---') }
    diml[[dimnames[id]]] <- MOC$dim[[id]]$vals
    mskl[[dimnames[id]]] <- diml[[dimnames[id]]] >= min(dimbounds[[dimnames[id]]]) &
                            diml[[dimnames[id]]] <= max(dimbounds[[dimnames[id]]])
  }

  val <-  ncvar_get(nci, 'MOC')[,,moc_ic,moc_itr]
  if (!is.null(val)) {
    maxAMOC <- rDAF::gridMax(diml[[1]],diml[[2]],val,
                                    mskl[[1]],mskl[[2]])
  }
  names(maxAMOC) <- c('maxlat','maxz','maxval')
  nc_close(nci)
  return(maxAMOC)
} # end function whichMaxAMOC()

