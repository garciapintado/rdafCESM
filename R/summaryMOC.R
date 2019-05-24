summaryMOC <- function(fname, dimbounds=NULL, moc_ic=1, moc_itr=2, it=NULL, fromPlotMOC=FALSE) {
  # fname  :: POP2 NetCDF file, including path, from which AMOC is obtained
  # latlim :: [\deg] latitudinal interval on which maximum AMOC is searched
  # zlim   :: [cm]   depth interval on which maximum AMOC is searched
  # moc_ic :: index within ["Eulerian Mean","Eddy-Induced (bolus)", "Submeso"]
  # moc_itr:: index within ["Global Ocean - Marginal Seas",
  #                         "Atlantic Ocean + Mediterranean Sea + Labrador Sea + GIN Sea + Arctic Ocean + Hudson Bay"]
  # yscl      :: 1.E-05 default to convert original [cm] to [km] in depth vector
    
  # CESM::POP2 MOC grid dimension names:
  # "lat_aux_grid moc_z moc_components transport_region time"

  if (!fromPlotMOC) { # not internally called from
    REGION_MASK  <- NULL # nullify only to avoid compiler note on unexisting variables
    ULAT         <- NULL

    nci <- nc_open(fname) 
    vnames <- c('ULAT','REGION_MASK','lat_aux_grid','moc_z', 'MOC')             # support variables, each: [nlon,nlat]
    for (vname in vnames) {
      eval(parse(text=paste(vname,' <- list(); ',vname,'$att <- ncatt_get(nci,"',vname,'");',
                                                 vname,'$vals <- ncvar_get(nci,"',vname,'")',sep='')))
    }; rm(vnames)

    dimnames <- c('lat_aux_grid','moc_z')  
    MOC$coordinates <- strsplit(MOC$att$coordinates,' ', fixed=TRUE)
    if (!all(names(dimnames) %in% MOC$coordinates))
      stop('getting MOC:: ---ERR001---')

    if (length(dim(MOC$vals)) == 5) {
      if (is.null(it))
        MOC$vals <- apply(MOC$vals,MARGIN=1:4,FUN=mean)
      else
        MOC$vals <- MOC$vals[,,,,it]
    }
    MOC$vals <- MOC$vals[,,moc_ic,moc_itr]
    layer2D  <- MOC$vals # [n_auxlat,n_zt] = [nx,ny], where

    vKIND <- 'MOC'    
    vdim <- nci$var[[vKIND]]$dim
    xseq <- vdim[[1]]$vals             # lat_aux_grid
    yseq <- vdim[[2]]$vals             # moc_z
    nx   <- length(xseq)               # [e.g. g37:105; g16:395]
    ny   <- length(yseq)               # [61] for both g37 & g16
    # ncDimnames <- sapply(nci$var[[vKIND]]$dim, function(x){x$name}) # == coordinates
  } else { # fromPlotMOC == TRUE
    # copy variables from the calling environment
    vnames <- c('REGION_MASK','ULAT','xseq','vdim','layer2D')
    for (vname in vnames) {
      eval(parse(text=paste(vname,' <- get("',vname,'", parent.frame())',sep=''))) }
  }
  # get SN coordinates for default   
  ULAT$vals[REGION_MASK$vals != 6] <- NA # always take Atlantic bound even for global meridional circulation evaluation
  latSN <- range(ULAT$vals, na.rm=TRUE)  # Atlantic South and North latitude
  Sid  <- which(xseq > latSN[1])[1]      # South maximum southward export 
  Eid  <- which.min(abs(xseq))           # Equator maximum southward export
  Nid  <- which(xseq > latSN[2])[1] - 1  # North maximum southward export

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
    diml[[id]] <- vdim[[id]]$vals
    mskl[[id]] <- diml[[id]] >= min(dimbounds[[id]]) &
                  diml[[id]] <= max(dimbounds[[id]])
  }
  if (!is.null(layer2D)) {
    maxMOC <- rDAF::gridMax(diml[[1]],diml[[2]],layer2D,
                            mskl[[1]],mskl[[2]], sFUN=max)
    minMOC <- rDAF::gridMax(diml[[1]],diml[[2]],layer2D,
                            mskl[[1]],mskl[[2]], sFUN=min)
  }
  # image(lat_aux_grid$vals, moc_z$vals/100, MOC$vals, col=matlab.like(100), ylim=rev(range(moc_z$vals/100)))
  # abline(h=500*100, col='grey')
  # abline(v=dimbounds[['lat_aux_grid']], col='grey')
  Smax <- max(layer2D[Sid,mskl[[2]]])
  Emax <- max(layer2D[Eid,mskl[[2]]])
  Nmax <- max(layer2D[Nid,mskl[[2]]])
  ans <- rbind(maxMOC,
               minMOC,
               c(xseq[Sid],yseq[which(layer2D[Sid,] == Smax)],Smax),
               c(xseq[Eid],yseq[which(layer2D[Eid,] == Emax)],Emax),               
               c(xseq[Nid],yseq[which(layer2D[Nid,] == Nmax)],Nmax))
  rownames(ans) <- c('max','min','Smax','Emax','Nmax')
  colnames(ans) <- c('lat_aux_grid','moc_z','val')

  if (!fromPlotMOC)
    nc_close(nci)
  return(ans)
} # end function summaryMOC()

