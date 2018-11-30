#!/R
# Author:            Javier Garcia-Pintado - MARUM
# Last modification: 2018-08-14

cesmDA <- function(envcase, DOUT_S_ROOT, comps, tavgs, tlimyStr, timenowStr, ilStr=NULL, 
                   ana, MC, prm, useGA, useGAy, yls, theta, MCgpar, analysis_scn='a0',
                   GET_ANALYSIS=TRUE, MEMORY_SAVE=TRUE, SAVE_NETCDF=TRUE, dsnrest=NULL, POP.zlrange = 1) {

 # envcase:     [>=3] list {CCSMROOT , CASE[m], CASEROOT[m]}
 # DOUT_S_ROOT: [m]   CHARACTER; path to model data storage for IO
 # comps:       [>=1] list with named elements. Names are model component to be analysed: \in:  c("atm",   "ice",     "lnd",     "ocn",  "rof")
 #                    Then, each element contains a  [>=1] CHARACTER vector of variables to be analysed, matching variables archived in history files
 # tlimyStr  :  [2]   CHARACTER; close,open interval [tlimyStr) representing the time-averaged footprint of the observations

 #                    TODO: modify by a mechanism with y-specific time-averages. So, request is for corresponding model averages instead
 

#library(ncdf4)                              
#library(colorRamps)
#library(hydrosim)
#library(rworldmap)
#library(maptools)                                               
#library(Cairo)                                                        # plot devices
#library(akima)                                                        # 2D interpolation
#library(RColorBrewer)

options(warn=1)

  HOME       <- Sys.getenv('HOME')
  Sys.MOVE   <- 'mv -fv'                                                                           # The FORCE options are not optional.  
  CASE_0     <- substring(envcase$CASE[1],1,nchar(envcase$CASE[1])-4)     
  CASEROOT_0 <- substring(envcase$CASEROOT[1],1,nchar(envcase$CASEROOT[1])-4)     

  seasons <- list()
  seasons$JFM <- 1:3
  seasons$AMJ <- 4:6
  seasons$JAS <- 7:9
  seasons$OND <- 10:12
  #tavgs <- c( names(seasons),'ann') # seasons + annual means

  dsnDAR    <- file.path(HOME,'docs','DA','R')
  cesmR     <- file.path(envcase$CCSMROOT, 'scripts','R') 

#source(file.path(cesmR, 'utils', 'getMOV.R'))             # 
#source(file.path(cesmR, 'utils', 'HE_szon.R'))            # 
#source(file.path(cesmR, 'utils', 'H_pages2k_coral.R'))    # WARNING: specific for the 1850DA_test  

#source(file.path(dsnDAR, 'transformations', 'plotGA.R'))                   # OPT, online generation of sampled anamorphosis
#source(file.path(dsnDAR, 'transformations', 'plotPseudoLinearization.R'))  # OPT, online generation of sampled anamorphosis

allcomps <- c("atm",    "ice",    "lnd",     "ocn",  "rof")              # generic model components
allCOM   <- c("CAM",    "CICE",   "CLM2",    "POP2", "RTM")              # specific model components
allcomph <- c("cam.h0", "cice.h", "clm2.h0", "pop.h","rtm.h0")           # climatic input: $CASE.$comph.$timelab


  #source(file.path(dsnR,'DA','stats','varSparse.R')) # 
  #source(file.path(dsnR,'DA','stats','statSparse.R')) # 


  m <- length(DOUT_S_ROOT)
  isensemble <- ifelse(m> 1 && !(prm$method %in% c('pIKF','pMKF')),TRUE,FALSE)

  tstaT     <- as.POSIXct(tlimyStr[1], tz='GMT')
  tendT     <- as.POSIXct(tlimyStr[2], tz='GMT') - 1                

  seq.month <- seq(tstaT,tendT,by='month')                                 # subset for analysis
  seq.mStr  <- substr(datePOSIXct(seq.month),1,7) 
  yearRange <- range(substr(seq.mStr,1,4))
  timelabs  <- paste(paste(yearRange,collapse='-'),tavgs,sep='-')
  # timelabALL <-  paste(paste(yearRange,collapse='-'), unique(c(tavgs,names(seasons))), sep='-')

  # model input: forward transform [Gaussian anamorphosis] of parameter ensemble
  toGA <- rep(FALSE,nrow(MCgpar))                                  # toGA         :: LOGICAL [m]. TRUE for MCgpar rows on which GA is conducted
  MCgparT    <- MCgpar                                             # MCgparTb :: REAL [np,m]. Parameter ensemble. Background forward Transformed into Gaussian

  if (isensemble && useGA) {
    WilkShapiroL <- list()                                              # WilkShapiroL :: List [[m]].  Normality tests
    if (ncol(MCgpar) > 1) { # m > 1
      for (i in 1:nrow(MCgpar)) {
        vKIND <- rownames(MCgpar)[i] 
        if (sd(MCgpar[i,]) == 0)
          next
        WilkShapiroL[[i]] <- shapiro.test(MCgpar[i,])
        #X11();hist(MCgpar[i,], breaks=20,main=rownames(MCgpar)[i])
        if ((WilkShapiroL[[i]]$p < 0.05 && ana[[vKIND]]$trf == 'autoGA') || ana[[vKIND]]$trf == 'GA')
          toGA[i] <- TRUE 
      }
    }

    MCgparAnam <- vector('list',nrow(MCgpar))

    for (i in 1:nrow(MCgpar)) { # fit GA model and forward transform
      if (toGA[i]) {
        vname           <- rownames(MCgpar)[i]
        if (MC[vname,'flag']) {
          MCgparAnam[[i]] <- anamFit(MCgpar[i,], xlim=c(ana[[vname]]$min,ana[[vname]]$max))
          MCgparT[i,]     <- approxM(MCgparAnam[i], MCgpar[i,])
        }         
      }
    }

    #if (3 > 2) {
    #  for (i in 1:nrow(MCgpar)) {
    #    vname <- rownames(MCgpar)[i]
    #    if (MC[vname,'flag'])
    #      plotGA(MCgpar[i,],MCgparAnam[[i]],MCgparT[i,],
    #             dsn=file.path(CASEROOT_0,'plots','GA'), fname=paste(gsub("[.]","_",vname),'pdf',sep='.'),
    #             rlab=paste(vname,'- raw'), glab=paste(vname,'- ana'), 
    #             family='Times', cex=1.2)
    #  } 
    #  cbind(apply(MCgpar[MC$flag,],MARGIN=1,FUN=function(x){shapiro.test(x)$p}),
    #        apply(MCgparT[MC$flag,],MARGIN=1,FUN=function(x){shapiro.test(x)$p}))
    #}
  } # end if (isensemble) forward input transform

  # model variables
  # tmp
  if (!all(names(comps)  %in% 'ocn')) # tmp stop: TODO: expand to any COMPONENT/variable
    stop('cesmDA:: code version for analysis of only ocean variables')
  # end tmp

  icom <- 1      # TODO: make as loop for all components
  
  com <- names(comps)[icom]
  COM <- allCOM[match(com, allcomps)]
  coh <- allcomph[match(com, allcomps)]
  xKINDs <- comps[[icom]]

  anaKINDs  <- paste(COM,xKINDs,sep='.')                     # subset of ana :: could include atmospheric variables, etc.
  if (!all(anaKINDs %in% names(ana))) {
    stop('CESM-DA:: variables not included in analysis definition: ',
         paste(anaKINDs[!(anaKINDs %in% names(ana))],collapse=','))
  }
  X     <- list()                                                                  # variables for analysis
  for (iv in 1:length(xKINDs)) {
    X[[iv]] <- list()
    X[[iv]]$val <-  list()
    for (itv in 1:length(timelabs))
      X[[iv]]$val[[timelabs[itv]]] <- list()
  }; names(X) <- anaKINDs


  # rename background
  #for (im in 1:m) {
  #  for (timelab in timelabALL) {
  #    climf   <- paste(envcase$CASE[im],coh,timelab,'nc',sep='.')                        # background
  #    dsno <- file.path(DOUT_S_ROOT[im],com,'post')
  #    if (!is.null(ilStr)) {
  #      dsno <- file.path(dsno,ilStr) }
  #    climfbp  <- file.path(dsno,climf)
  #    climfbp <- gsub('.nc','.b.nc',climfp)
  #   if (file.exists(climfp) && ! file.exists(climfbp))
  #  system(paste(Sys.MOVE,climfp,climfbp))
   #  }
  # }

  # get COM metadata: - any ensemble member
  im <- 1
  dsno <- file.path(DOUT_S_ROOT[im],com,'post')
  if (!is.null(ilStr)) {
    dsno <- file.path(dsno,ilStr) }
  climfb   <-  paste(envcase$CASE[im],coh,timelabs[1],'nc',sep='.')
  nci <-  nc_open(file.path(dsno,climfb))
  for (i in 1:length(X)) {
     xKIND <- xKINDs[i]
     X[[i]]$att <- ncatt_get(nci,xKIND)
  }
  dims <- nci$dim                                        # [cm]  z_t: "depth from surface to midpoint of layer"
  TLONG <- TLAT <- ULONG <- ULAT <- REGION_MASK <- NULL
  if (com == 'ocn') {
    suppvar <- c('TLONG','TLAT','ULONG','ULAT','REGION_MASK')             # support variables, each: [nlon,nlat]
    for (i in suppvar) {
      eval(parse(text=paste(i,' <- list(); ',i,'$att <- ncatt_get(nci,"',i,'");',i,'$val <- ncvar_get(nci,"',i,'")',sep='')))
    }; rm(suppvar)
    z_t <- nci$dim$z_t$vals                          # [cm] center of POP2 ocean layers
    z_w <- nci$dim$z_w$vals                          # [cm] top    of POP2 ocean layers
  
    TLONG180 <- TLONG$val 
    TLONG180[TLONG180 > 180] <-  TLONG180[TLONG180 > 180] - 360  # in [-180,180]
    ULONG180 <- ULONG$val 
    ULONG180[ULONG180 > 180] <-  ULONG180[ULONG180 > 180] - 360  # in [-180,180]
  }
  nc_close(nci)

  gauDA <- list()                                # process gauDA object : matching y-HE pairs with structure based on yKINDs [i.e. stack yTYPEs]  
  for (ik in 1:length(yls)) {                    # yKINDs do not match model variable names xKINDs. One forward operator needed for each $yKIND.$yTYPE
    # gauDA: generic
    yKIND <- names(yls)[ik]
    gauDA[[ik]] <- list()
    names(gauDA)[ik] <- yKIND
    ps <- sapply(yls[[ik]],FUN=function(x){length(x$p$data$y)})                                            # length for each yTYPE within this yKIND
    gauDA[[ik]]$yTYPE   <- rep(names(yls[[ik]]),ps)                                                        # [p]
    gauDA[[ik]]$y       <- as.numeric(unlist(sapply(yls[[ik]],FUN=function(x){x$p$data$y})))               # [p]
    gauDA[[ik]]$pos     <- do.call(rbind,lapply(yls[[ik]],FUN=function(x){x$p$data$pos}))                  # [p]
    gauDA[[ik]]$z       <- as.numeric(do.call(c,lapply(yls[[ik]],FUN=function(x){x$p$data$z})))            # [p] [cm]
    gauDA[[ik]]$timelab <- as.character(do.call(c,lapply(yls[[ik]],FUN=function(x){x$p$data$timelab})))    # [p]
    gauDA[[ik]]$r       <- as.numeric(do.call(c,lapply(yls[[ik]],FUN=function(x){x$p$data$r})))            # [p] R-diagonal 
    gauDA[[ik]]$HE      <- matrix(NA,sum(ps),m)                                                            # [p,m]

    oboo <-  gauDA[[ik]]$pos[,1] < 0                                                                       # if observation longitudes in [-180,180] ->  [0,360]
    gauDA[[ik]]$pos[oboo,1] <- gauDA[[ik]]$pos[oboo,1] + 360
    rm(oboo)

    # gauDA: specific HE mappings
    if (yKIND == "KIND_albicei") {                     # forced parameterisation
      gauDA[[ik]]$HE <- MCgpar['CICE.albicei',,drop=FALSE]

    } else if (yKIND == "KIND_albicev") {              # forced parameterisation
      gauDA[[ik]]$HE <- MCgpar['CICE.albicev',,drop=FALSE]

    } else if (yKIND == "KIND_coral") {                # PAGES2k coral data - note this is an add-hoc mapping for this pre-industrial test
      comy <- 'ocn'
      cohy <- allcomph[match(com, allcomps)]
      gauDA[[ik]]$lmSST   <- do.call(rbind,lapply(yls[[ik]],FUN=function(x){x$p$data$lmSST}))                # for proxy~SST linear mapping
      SST <- matrix(NA,sum(ps),m)                                                                        # [p,m]     
      ytimelabs <- unique(gauDA[[yKIND]]$timelab)
      for (ity in 1:length(ytimelabs)) {                                                                       # each possible observation time
        timelab <- ytimelabs[ity]
        tboo <- gauDA[[ik]]$timelab == ytimelabs[ity]
        for (im in 1:m) {
          dsno <- file.path(DOUT_S_ROOT[im],comy,'post')
          if (!is.null(ilStr)) {
            dsno <- file.path(dsno,ilStr) }
          climfb   <-  paste(envcase$CASE[im],cohy,timelab,'nc',sep='.')
          if (!file.exists(file.path(dsno,climfb))) {
            if (!isensemble) {
              stop('cesmDA:: missing member for parameter-space KF') 
            } else {
              cat('cesmDA::',envcase$CASE[im],'member not found\n') 
              next
            }
          }
          nci <-  nc_open(file.path(dsno,climfb))
          POP2.TEMP.1 <-  ncvar_get(nci,'TEMP', start=c(1,1,1,1), count=c(-1,-1,1,-1))         # top layer 
          nc_close(nci)
          isna <- is.na(POP2.TEMP.1)

          SST[tboo,im] <- interpEarth(TLONG$val[!isna], TLAT$val[!isna], POP2.TEMP.1[!isna],
                                      xo=gauDA[[ik]]$pos[tboo,1], yo=gauDA[[ik]]$pos[tboo,2], output='points')
        }
      } # end getting SST at matching times
      # HEfoo <-  gauDA[[ik]]$HE
      
      for (ig in 1:length(gauDA[[ik]]$y)) {
        gauDA[[ik]]$HE[ig,] <- gauDA[[ik]]$lmSST[ig,1] + gauDA[[ik]]$lmSST[ig,2]*SST[ig,]                     # forward proxy model   
      }
      rm(ytimelabs, timelab, tboo)

    } else if (yKIND == 'KIND_ocn_sst') {                                                          # MARGO synthetic sea surface temperature (10m depth)
      comy <- 'ocn'
      cohy <- allcomph[match(com, allcomps)]
      dualKIND     <- list()                                                                       # model -> dual of the observation space
      dualKIND$val <- list()
      ytimelabs <- unique(gauDA[[yKIND]]$timelab)
      for (ity in 1:length(ytimelabs)) {                                                           # interp z-levels
        timelab <- ytimelabs[ity]
        dualKIND$val[[timelab]] <- list()
        for (im in 1:m) {
          dsno <- file.path(DOUT_S_ROOT[im],comy,'post')
          if (!is.null(ilStr)) {
            dsno <- file.path(dsno,ilStr) }
          climfb   <-  paste(envcase$CASE[im],cohy,timelab,'nc',sep='.')
          if (!file.exists(file.path(dsno,climfb))) {
            if (!isensemble) {
              stop('cesmDA:: missing member for parameter-space KF') 
            } else {
              cat('cesmDA::',envcase$CASE[im],'member not found\n')
              next 
            }
          }
          nci <-  nc_open(file.path(dsno,climfb))
          POP2.TEMP.top2 <-  ncvar_get(nci,'TEMP', start=c(1,1,1,1), count=c(-1,-1,2,-1))           # two top layers - just to get MARGO-depth interpolations 
          dualKIND$val[[timelab]][[im]] <- apply(POP2.TEMP.top2[,,1:2], c(1,2), mean, na.rm=TRUE)   # average into 1 layer
          nc_close(nci)

        } # end for im
      } # end for ity
      isna <- is.na(dualKIND$val[[1]][[1]])                                                         # land
      for (ity in 1:length(ytimelabs)) {
        cat('processing HE: ',yKIND,'|',ytimelabs[ity],'\n')
        timelab <- ytimelabs[ity]     
        tboo    <- gauDA[[ik]]$timelab == timelab
        for (im in 1:m) {
          if (is.null(dualKIND$val[[timelab]][[im]]))
            next
          gauDA[[ik]]$HE[tboo,im] <- interpEarth(TLONG$val[!isna], TLAT$val[!isna], dualKIND$val[[timelab]][[im]][!isna], # if (yKIND %in% onU) use ULONG,ULAT (velocity grid)
                                                 xo=gauDA[[ik]]$pos[tboo,1], yo=gauDA[[ik]]$pos[tboo,2], output = 'grid')
        }
      }
      rm(ytimelabs, timelab, dualKIND, tboo)

    #} else if (yKIND == "KIND_szon") {                 # reanalysis: zonal mean of salinity vertical profiles in the South Atlantic
    #  ytimelabs <- unique(gauDA[[ik]]$timelab)
    #  for (ity in 1:length(ytimelabs)) {                                                                   # each possible observation time
    #    tboo <- gauDA[[ik]]$timelab == ytimelabs[ity]
    #    z <- gauDA[[ik]]$z[tboo]                                                                           # [cm] y-space 
    #    gauDA[[ik]]$HE[tboo,] <- HE_szon(DOUT_S_ROOT, envcase$CASE ,ytimelabs[ity], z=z)
    #  }
    #  rm(ytimelabs, tboo, z)
    } else {
      stop('cesmDA:: --ERR000-- requested yKIND not available')
    }
    if (isensemble) {
      gauDA[[ik]]$Hx <- rowMeans(gauDA[[ik]]$HE, na.rm=TRUE)
    } else {
      gauDA[[ik]]$Hx <- gauDA[[ik]]$HE[,1]
    }
    gauDA[[ik]]$dy <- gauDA[[ik]]$y - gauDA[[ik]]$Hx                                                   # recalculated in analyse()
  } # end for ik :: gauDA process
  saveRDS(gauDA, file=file.path(dsnrest, paste(CASE_0,"gauDA_raw",timenowStr,ilStr,"rds",sep=".")))    # non-transformed variables

  # calibrate Gaussian Anamorphosis of dual of observation space (HE) & conduct forward transform:
  # y & HE at each location assumed to have the same distribution function
  gauDAT <- gauDA   
  if (isensemble && useGAy) {                                                                                # forward transform
    for (ik in 1:length(gauDA)) {
      vKIND <- names(gauDA)[ik]
      if (ana[[vKIND]]$trf != 'GA')
        next
      p <- length(gauDA[[ik]]$y)
      gauDA[[ik]]$anam <- vector('list',p)
      for (i in 1:p) {
        gauDA[[ik]]$anam[[i]] <- anamFit(gauDA[[ik]]$HE[i,])
        gauDAT[[ik]]$HE[i,]   <- approxM(gauDA[[ik]]$anam[i], gauDA[[ik]]$HE[i,])                      # forward transform HE & y
        gauDAT[[ik]]$y[i]     <- approxM(gauDA[[ik]]$anam[i], gauDA[[ik]]$y[i]) 
        #sca <- sd(gauDAT[[ik]]$HE[i,], na.rm=TRUE) / sd(gauDA[[ik]]$HE[i,], na.rm=TRUE)               # moments are preserved - unneeded
        #gauDAT[[ik]]$r[i]   <- (sqrt(gauDA[[ik]]$r[i]) * sca )^2
      }
      gauDAT[[ik]]$Hx <- rowMeans(gauDAT[[ik]]$HE, na.rm=TRUE)
      gauDAT[[ik]]$dy <- gauDAT[[ik]]$y - gauDAT[[ik]]$Hx                                                   # recalculated in analyse()
    } # e.g. 14 secs for all MARGO locations [1919 observations]
    saveRDS(gauDAT, file=file.path(dsnrest, paste(CASE_0,"gauDA_ana",timenowStr,ilStr,"rds",sep="."))) # non-transformed variables
  } # end if (isensemble)

  #if (3 > 2) { #tmp
  # e <- list()
  # e$vKINDs <- rownames(MCgpar)[MC$flag]        #"CAM.cldfrc_rhminl"
  # e$yKIND <- "KIND_ocn_sst"
  # e$yklab <- "SST"
  # e$dsn   <- file.path(CASEROOT_0,'plots','GA','linearizations')
  # e$is    <- c(1,which.max(gauDA[[ik]]$dy),which.min(gauDA[[ik]]$dy))
  # for (ei in e$is) {
  #   e$i     <- ei
  #   e$pos   <- round(gauDA[[e$yKIND]]$pos[ei,],2)
  #   plotGA(gauDA[[yKIND]]$HE[ei,], gauDA[[yKIND]]$anam[[ei]], gauDAT[[yKIND]]$HE[ei,],
  #          dsn=e$dsn, fname=paste(yKIND,'_y_',ei,'_GA.pdf',sep=''), 
  #          rlab=paste('SST (',e$pos[1],',',e$pos[2],') - raw'), 
  #          glab=paste('SST (',e$pos[1],',',e$pos[2],') - ana'), 
  #          rsamples=gauDA[[e$yKIND]]$y[ei],  
  #          family='Times', cex=1.2)
  #   for (vKIND in e$vKINDs) {
  #     e$vKIND <- vKIND 
  #     e$fname <- paste(gsub("[.]","_",e$vKIND),"_",e$yKIND,"_",ei,"_linearization.pdf",sep="")
  #     plotPseudoLinearization(e$dsn, e$fname, MCgpar, MCgparT, gauDA, gauDAT, e$vKIND, e$yKIND,ei, e$yklab,'serif') 
  #   }
  # }
  #} # end tmp

  #onU <- c('POP2.UVEL','POP2.VVEL')

  # augmentation list: $value[,m], $pos $
  aug <- list()
  if (!all.equal(rownames(MC),rownames(MCgpar)))
   stop('MC rownames != MCgpar rownames')
  for (ip in 1:nrow(MCgpar)) {
    if (!MC[ip,'flag'] || !MC[ip,'ispar'])
      next
    vKIND <- rownames(MCgpar)[ip]
    aug[[vKIND]] <- list()
    aug[[vKIND]]$E       <- MCgparT[ip,]            # [n,m] forward transformed for useGA and ensemble
    aug[[vKIND]]$pos     <- c(NA,NA)                  # [n,2] global
    aug[[vKIND]]$timelab <- timelabs[1]               # [n]   not used by DA - no mapping from aug variables into y
    aug[[vKIND]]$z       <- NA                        # [n]   global
  }

  # augmentation by derived variables
  if (1 > 2) {
   # OHC :: Oceanic Heat content (J/cm3) can be obtained as:
   # OHC = TEMP * POP_CpSW * POP_rhoSW, where 
   # TEMP is Potential Temperature
   # cp_sw is specific heat salt water
   # rho_sw is density of salt water
   cp_sw  <- 3.996e7                    # /constants.F90.html        (cm^2/s^2/C)
   rho_sw <- 4.1/3.996                  #         "                  (g/cm^3)
  }

  prm$m <- m

  if (GET_ANALYSIS) {

    POP.zlseq <- seq(POP.zlrange[1],POP.zlrange[2])                        # index vector for vertical layer


      # memory-saving iterative analysis:
      # multiplies netCDF I/O but saves workspace memory
      # a) get variables for analysis as netCDF slabs [1-layer/variable at a time]
      # b) analyse individual variable-timelab and store as netCDF   [...CASE.vname.timelab.nc]

      #gauDA1r <- gauDA[[1]]$r

      for (iv in 1:length(X)) {
        vKIND <- names(X)[iv]
        coo  <- strsplit(X[[iv]]$att$coordinates, split=' ')[[1]]
        zcoo <- c('z_w','z_t')[c('z_w','z_t') %in% coo]
        is3D <- ifelse(length(zcoo) > 0, TRUE, FALSE)
        nzl <- ifelse(is3D,diff(POP.zlrange)+1,1)
        count <- NA
        start <- NA
        if (nzl > 1)
          count <-  c(-1,-1,1,-1)
        grd <- strsplit(X[[vKIND]]$att$coordinates, split=' ')[[1]][1]              # CHARACTER
        if (grd == 'TLONG') {
          gLON <- TLONG$val
          gLAT <- TLAT$val
        } else {
          gLON <- ULONG$val
          gLAT <- ULAT$val
        }

        for (itv in 1:length(X[[iv]]$val)) { # get climatological season ensemble + annual means
          timelab <- names(X[[iv]]$val)[itv]
          for (iz in 1:nzl) {
            zl <- POP.zlseq[iz]
            fit <- paste(names(X)[iv], zl, timelab, ilStr, sep='.')
            cat('CESM_DA::',fit,'\n')
            start <- NA
            if (iz > 1) {
              start <- c(1,1,zl,1) }

            # load variable-layer ensemble background
            for (im in 1:m) {
              dsno <- file.path(DOUT_S_ROOT[im],com,'post')
              if (!is.null(ilStr)) {
                dsno <- file.path(dsno,ilStr) }
              climfb   <-  paste(envcase$CASE[im],coh,timelab,'nc',sep='.')
              if (!file.exists(file.path(dsno,climfb))) {
                if (!isensemble) {
                  stop('cesmDA:: missing member for parameter-space KF') 
                } else {
                  cat('cesmDA::',envcase$CASE[im],'member not found\n') 
                  next
                }
              }
              nci <-  nc_open(file.path(dsno,climfb))
              X[[iv]]$val[[timelab]][[im]] <- ncvar_get(nci,xKINDs[iv], start=start, count=count)
              if (nzl > 1) {                                                                             # in 3D variable
                X[[iv]]$z <- eval(parse(text=zcoo))[zl]
              } else {
                X[[iv]]$z <- 0
              }
              nc_close(nci)
              #for (ia in ias) {
              #  analysis_scn <- paste('a',ia,sep='')
              climfa <- sub('nc',paste(analysis_scn,'nc',sep='.'),climfb)
              if (!file.exists(file.path(dsno,climfa))) {
                system(paste('cp -f',file.path(dsno,climfb), file.path(dsno,climfa))) }                           # do not preserve timestamp
                  #}
            }
            # note: for Gaussian Anamorphosis on dynamic variables & standard ETKF, forward transformations should be done here
            # and analysis inverted after the assimilation

            # DA analysis
            #for (ia in ias) {
            #  analysis_scn <- paste('a',ia,sep='')
            #  gauDA[[1]]$r <- gauDA1r / 10^(ia-1)
            Elst_a <- analyseUG(G=NULL, gLON=gLON, gLAT=gLAT, 
                                fit=fit, prm=prm, X=X[iv], yls=NULL, gauDA=gauDAT, 
                                gauTS=NULL, aug=aug, ana=ana,
                                dsn=file.path(CASEROOT_0,'DA'), debugmode=TRUE, retdy=FALSE, mpi=FALSE, 
                                analysis_scn=analysis_scn, theta=theta)
            if (!isensemble) { 
              theta <- Elst_a                       # slots: PHT,HPHT,K,a,Pthetaa
              theta$MCgpar <- MCgpar
              Pthetaa      <- theta$Pthetaa                                                                            # obtain by any method
              saveRDS(theta, file=file.path(dsnrest, paste(CASE_0,"theta",timenowStr,ilStr,"rds",sep=".")))
              rm(Elst_a)
              return(theta) # break loop :: no need to do it more than once           
            }

            # write analysis to netCDF files
            if (SAVE_NETCDF) {
              xboo <- as.logical(Elst_a$xdf[,'xtime'] == timelab & Elst_a$xdf[,'xKIND'] == vKIND)
              if (sum(xboo) != length(TLONG$val))
                stop('CESM_DA:: length(xboo) != length(TLONG$val')
              for (im in 1:m) {
                dsno <- file.path(DOUT_S_ROOT[im],com,'post')
                if (!is.null(ilStr)) {
                  dsno <- file.path(dsno,ilStr) }
                climfb <- paste(envcase$CASE[im],coh,timelab,'nc',sep='.')
                climfa <- sub('nc',paste(analysis_scn,'nc',sep='.'),climfb)
                if (!file.exists(file.path(dsno,climfa)))
                  next
                nci <- nc_open(file.path(dsno,climfa), write=TRUE)
                ncvar_put(nci,xKINDs[iv], Elst_a$E[xboo,im], start=start, count=count)
                nc_close(nci)
              }
            }

            # get updated parameters
            xids <-  match(rownames(MCgpar),Elst_a$xdf[,'xKIND'])
            if (length(xids) != nrow(MCgpar))
              stop('CESM_DA:: ---ERR005---')
            MCgparTa <- Elst_a$E[xids,]
            MCgpara  <- MCgpar
            for (i in 1:nrow(MCgpar)) {
              if ( all(is.na(MCgparTa[i,])) )                     # not updated
                next
              if (isensemble && toGA[i]) {
                MCgpara[i,] <- approxM(MCgparAnam[i], MCgparTa[i,], forward=FALSE)                 # anamorphosis inverse
              } else {
                MCgpara[i,] <- MCgparTa[i,]
              }
            }
            # rowMeans(MCgpara, na.rm=TRUE)
            # cbind(rowMeans(MCgpara, na.rm=TRUE), apply(MCgpara,MARGIN=1,sd,na.rm=TRUE))
            saveRDS(MCgparTa, file=file.path(dsnrest, paste(CASE_0,"MCgparTa_ponly",timenowStr,ilStr,"rds",sep="."))) # non-transformed variables
            saveRDS(MCgpara, file=file.path(dsnrest, paste(CASE_0,"MCgpara_ponly",timenowStr,ilStr,"rds",sep="."))) # non-transformed variables

            rm(Elst_a)
            X[[iv]]$val[[timelab]] <- list()                                                       # release memory
          } # end for iz 
        } # end for itv
      } # end  for iv

  } else {# end if(GET_ANALYSIS)
    MCgpara <- NULL
  }
  return(MCgpara)
} # end function cesmDA()
