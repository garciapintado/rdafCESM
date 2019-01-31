getClimTS <- function(tlimStr=NULL, DOUT_S_ROOT, CASE, by='year', allowNA=TRUE, comps=NULL, loop=NULL) {

  # tlimStr     :: [2]   CHARACTER: time limits
  # DOUT_S_ROOT :: [m]   CHARACTER: path to CESM output archive
  # CASE        :: [m]   CHARACTER: name of CESM CASE
  # by          :: [1]   dT for the time sequence passed to seq.POSIXt
  # allowNA     :: [1]   LOGICAL: TRUE to stop in model archive output is not found
  # comps       :: [>=1] CHARACTER of model components, over which to conduct means. NULL for all model components below
  # loop        :: [1]   CHARACTER: If not NULL, output for each component $com will be stored in subfolder '$DOUT_S_ROOT/$com/post/$loop'  
    
  # purpose: obtain time series of mean values integrated for a specific dt for POP2 netCDF files
  # tlimStr is a closed-open interval at both ends. That is, the last date included date is one second less than tlimStr[2]
  # For example: ["1850-01-01 00:00:00","1910-01-01 00:00:00") => ["1909-12-31 23:59:59"] as end time.
  # That is, integrations covering 1850-1909 including both

  # tlimStr: limits for the statistics
  # DOUT_S_ROOT :: [m] path to CESM archive
  # CASE        :: [m] name of CESM CASE
  #
  # get climatological months, climatological seasons, and climatological years as netCDF files
  # this is a wrapper around the NCO command ncra

  Sys.LINK   <- 'ln -fs'
  Sys.MOVE   <- 'mv -fv'   
  allcomps <- c("atm",   "ice",     "lnd",     "ocn",  "rof")
  allcomph <- c("cam.h0", "cice.h", "clm2.h0", "pop.h","rtm.h0") # monthly input: $CASE.$comph.AAAA-MM.nc

  if (is.null(comps))
    comps <- allcomps
  if (!all(comps %in% allcomps))
    stop('getClimTS:: input comps not defined in CESM compset')  
  comph <- allcomph[match(comps, allcomps)]
  
  seasons <- list()
  seasons$JFM <- 1:3
  seasons$AMJ <- 4:6
  seasons$JAS <- 7:9
  seasons$OND <- 10:12

  # get climatological months/seasons/years
  cwd <- getwd()
  m <- length(DOUT_S_ROOT)

  for (im in 1:m) {
    cat('getClimTS:: calculating means: im:',im,' | CASE:', CASE[im],'\n')

    for (ic in 1:length(comps)) {
      com <- comps[ic]
      coh <- comph[ic]      
      dsni   <- file.path(DOUT_S_ROOT[im],com,'hist')
      if (!file.exists(dsni)) {
        if (allowNA)
          next
        else
          stop('getClimTS: simulation not found for:',CASE[im])
      }

      dsno <- file.path(DOUT_S_ROOT[im],com,'post')
      if (!is.null(loop))                                         # loop subfolder
        dsno <- file.path(dsno,loop)
      dsno <- file.path(dsno, gsub(' ','_',by))                   # by subfolder

      if(!file.exists(dsno))
        dir.create(dsno, recursive=TRUE)

      if (is.null(tlimStr)) {
        fnames <- dir(dsni, pattern=paste('*',coh,'[[:digit:]]',sep='.'))            # all available monthly files
        mtags  <- substr(fnames,nchar(fnames)-9,nchar(fnames)-3)
        metlimStr <- paste(mtags[c(1,length(mtags))],'-01 00:00:00',sep='') 
      } else {
        metlimStr <- tlimStr
      }

      staT    <-  as.POSIXct(metlimStr[1], tz='GMT', format="%Y-%m-%d %H:%M:%S")
      endT    <-  as.POSIXct(metlimStr[2], tz='GMT', format="%Y-%m-%d %H:%M:%S")
      if (is.null(tlimStr))
        endT <- rDAF::tlag(endT,mlag=1)
      endT <- endT - 1                          # 1 second before the (open) upper bound
      
      seq.ou.min    <-  seq(staT,endT,by=by)
      seq.ou.max    <-  c(seq.ou.min[-1]-1,endT)
      attr(seq.ou.min, 'tzone') <- 'GMT'  
      attr(seq.ou.max, 'tzone') <- 'GMT'
  
      nit <- length(seq.ou.min)                                   # number of integration times

      seq.ou.minStr <- format(seq.ou.min, format='%Y-%m')
      seq.ou.maxStr <- format(seq.ou.max, format='%Y-%m')
      
      for (it in 1:nit) {
        setwd(dsni)
        tseq    <- seq(seq.ou.min[it], seq.ou.max[it], by='month') # input assumed to be monthly
        attr(tseq, 'tzone') <- 'GMT'
        tseqStr <-  format(tseq, format='%Y-%m')
        fnames  <- paste(CASE[im],coh,tseqStr,'.nc',sep='')
        if (!all(file.exists(fnames)))
          stop('getClimTS:: ---ERR001-- fnames do not exist:',paste(fnames,collapse=','))
        if (length(fnames) == 1) {
          setwd(dsno)
          system(paste(Sys.LINK,file.path(dsni,fnames),'.'))             # for monthly output - just link
        } else {
          if (by == 'year' && substr(seq.ou.minStr[it],1,4) == substr(seq.ou.maxStr[it],1,4)) # exact years
            dnameo <- substr(seq.ou.minStr[it],1,4)
          else
            dnameo <- paste(seq.ou.minStr[it],'_',seq.ou.maxStr[it],sep='')
          climf  <- paste(CASE[im],coh,dnameo,'.nc',sep='')
          cat('getClimTS:: generating', climf,'\n')
          syscmd <- paste('ncra',paste(fnames, collapse=' '),climf,sep=' ')         # get climatological means: NCO -ncra
          system(syscmd)
          system(paste(Sys.MOVE,climf,dsno,sep=' '))
        }
      } # end for it
    } # end for ic
  } # end for im
  setwd(cwd)
  invisible(0)
} # end function getClimTS()
