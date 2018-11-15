# this file serves as template example showing observation structure for 'p'-format (FMT) observations.
# 'p'-observations are multi-point single-time. The major difference with 'g'-observation FMT is that
# 'p' FMT has possibly a multi-timestamp. However, it is not a timeseries as 'g'-observations, but a distributed timestamp.
# 'p'-observations & 'g'-observations will be assimilated by augmentation. That is, for each observation y_i and assimilation step,
#  $\delta y = y_i^0 - y_i^b$ is a scalar calculated previously (externally) to the ensemble constructor analyse() and passed into the constructor within a
# a gauDA structure. Note augmentation of the ensemble state vector is not strictly required (as just dy & HA are needed from the observation side). The augmentation, 
# however, allows to monitor the effect of the assimilation on the Hx (the 'ensemble observations' ---state vector on the observation space---).

# 'p'-observations will be fractionally assimilated [running pMFS/pIKS] in time if their flag 'fract' is set to TRUE
# it is assumed that R ($\sigma^2$) refers to the observation as a result of all given individual observation times
# Thus is ytime has N entries, the variance of a fraction containing e.g. k elements in ytime will be assumed as ($\frac{N}{k}sigma^2\$)
# This assumes equilibrium conditions and is independent of the R inflation conducted for pMFK [Garcia-Pintado_Paul2018]

foof <- function(CASEy, DOUT_S_ROOTy, fname, y_class=NULL) {
  # get MARGO data but set data slot $y as empty holder to be filled later by a 'true' simulation
  # y_class: generic y structure

  # this add-hoc function is not for general Hx operations on CESM, but very specific to get TEMP at 10m depth - as MARGO assumption

  library(akima)

  yKIND <- 'KIND_ocn_sst'                                             # vector with observation KINDs
  yls <- readRDS(fname)                                               # load MARGO data just to get locations 
 
  yTYPEs <- names(yls[[yKIND]])
  yFMT  <- 'p'                                                        # available formats are 'g','s','p'

  # substitute old timelabs '1980-1910...' by '1980-1909...' stored in processed MARGOlabs for 20-yr mean
  # TODO from here
  for (i in 1:length(yTYPEs)) {
    yTYPE <- yTYPEs[i]
    yls[[yKIND]][[yTYPE]]$p$data$timelab <- gsub('1890-1910','1890-1909',yls[[yKIND]][[yTYPE]]$p$data$timelab)
  }

  for (i in 1:length(yTYPEs)) {
    yTYPE <- yTYPEs[i]
    yls[[yKIND]][[yTYPE]]$p      <- c(yls[[yKIND]][[yTYPE]]$p,y_class)
  }

  POP.zlrange <- c(1,2) # range of vertical layers to get input from POP2
  POP2vname <- 'TEMP'
  anaVname  <- paste('POP2',POP2vname,sep='.')
#browser()
  # group unique timelabels - then open needed .nc files and populate $y for all available proxies
  timelabs <- unique(sapply(yls[[yKIND]],FUN=function(x){unique(x$p$data$timelab)}))

  for (i in 1:length(yTYPEs)) {
    yTYPE <- yTYPEs[i]
    yls[[yKIND]][[yTYPE]]$p$data$yLGM <- yls[[yKIND]][[yTYPE]]$p$data$y
    yls[[yKIND]][[yTYPE]]$p$data$y <- rep(NA,nrow(yls[[yKIND]][[yTYPE]]$p$data$pos))
  }

  for (it in 1:length(timelabs)) { # H time operator is simply 1-to-1 via timelabs
    cat('processing observations: ',timelabs[it],'\n')
    fname <- paste(CASEy, 'pop.h',timelabs[it],'nc',sep='.')
    fnamep <- file.path(DOUT_S_ROOTy,'ocn','post', fname)
    nci <- nc_open(fnamep)

    # process ocean at 10-m depth   
    suppvar <- c('TLONG','TLAT','REGION_MASK')             # support variables, each: [nlon,nlat]
    for (i in suppvar) {
      eval(parse(text=paste(i,' <- list(); ',i,'$att <- ncatt_get(nci,"',i,'");',i,'$val <- ncvar_get(nci,"',i,'")',sep='')))
    }; rm(suppvar)
    TLONG180 <- TLONG$val 
    TLONG180[TLONG180 > 180] <-  TLONG180[TLONG180 > 180] - 360 # in [-180,180]

    # reconstruct X for this timelab
    X <- list()                                                                  # variables for analysis
    X[[anaVname]] <-  list()  
    X[[anaVname]]$att <- ncatt_get(nci,POP2vname)
    X[[anaVname]]$val <- list()
    X[[anaVname]]$val[[1]] <- ncvar_get(nci,POP2vname, start=c(1,1,POP.zlrange[1],1), count=c(-1,-1,diff(POP.zlrange)+1,-1))
    # y variable - ocean potential TEMP at 10m depth
    X$KIND_ocn_sst <- list()                                                             # a) TEMP interpolation to 10m depth
    X$KIND_ocn_sst$att <- X$POP2.TEMP$att
    X$KIND_ocn_sst$att$coordinates <- "TLONG TLAT time"
    X$KIND_ocn_sst$val <- list()                                                         # as many members as timesteps
    X$KIND_ocn_sst$val[[1]] <-  apply(X$POP2.TEMP$val[[1]][,,1:2], c(1,2), mean, na.rm=TRUE)

    # interpolate into MARGO locations
    isna <- is.na(X$KIND_ocn_sst$val[[1]])
    for (i in 1:length(yTYPEs)) {
      yTYPE <- yTYPEs[i]
      p <- nrow(yls[[yKIND]][[yTYPE]]$p$data$pos)
      y <- rep(NA,p)                                                          # they do not necessarily all have the same timelab
      ylonrange <- range(yls[[yKIND]][[yTYPE]]$p$data$pos[,1])
      if (min(ylonrange) < 0) {
        pboo <- yls[[yKIND]][[yTYPE]]$p$data$pos[,1] < 0
        yls[[yKIND]][[yTYPE]]$p$data$pos[pboo,1] <-  yls[[yKIND]][[yTYPE]]$p$data$pos[pboo,1] + 360
      }
      ylonrange <- range(yls[[yKIND]][[yTYPE]]$p$data$pos[,1])
      if (ylonrange[1] < 0 || ylonrange[2] > 360)
        stop('ypos range not in [0,360]')

      ypos <- yls[[yKIND]][[yTYPE]]$p$data$pos
      tboo <- yls[[yKIND]][[yTYPE]]$p$data$timelab == timelabs[it]
      pboo <- ypos[,1] >= 90 & ypos[,1] <= 270
      boo0 <- tboo & pboo
      y[boo0] <- interpp(TLONG$val[!isna],TLAT$val[!isna],X$KIND_ocn_sst$val[[1]][!isna], xo=ypos[boo0,1],yo=ypos[boo0,2])$z # b) bivariate linear interpolation

      boo180  <- ypos[,1] > 180
      ypos180 <- ypos
      ypos180[boo180,1] <- ypos180[boo180,1] - 360             # in [-180,180]
      pboo <- ypos180[,1] > -90 & ypos180[,1] < 90
      boo0 <- tboo & pboo
      y[boo0] <- interpp(TLONG180[!isna],TLAT$val[!isna],X$KIND_ocn_sst$val[[1]][!isna], xo=ypos180[boo0,1],yo=ypos180[boo0,2])$z # b) bivariate linear interpolation
      yls[[yKIND]][[yTYPE]]$p$data$y[!is.na(y)] <- y[!is.na(y)] # fill in these yTYPE observations for this timelab
    }
    nc_close(nci)
  } # end for it
  # example plot sampled locations:
  if (1 > 2) { # original locations as used in the forecast and DA analysis
    library(colorRamps)
    yKIND <- 'KIND_ocn_sst'
    yTYPE <- 'MARGO_fora_LGM_SST_ann'
    vals <- X[[yKIND]]$val[[1]]
    vals[isna] <- mean(vals,na.rm=TRUE)
    vlim <- range(vals)
    color <- approxRGB(matlab.like(100),seq(vlim[1],vlim[2],length=100), vals)[[2]]
    color[isna] <- 'black'
    X11(); plot(TLONG$val,TLAT$val,col=color, pch=15, cex=0.5, main='KIND_ocn_sst -10m') 
    color <- approxRGB(matlab.like(100),seq(vlim[1],vlim[2],length=100), yls[[yKIND]][[yTYPE]]$p$data$y)[[2]]
    ypos <- yls[[yKIND]][[yTYPE]]$p$data$pos
    points(ypos[,1],ypos[,2],col=color)
  }

  fname <- paste(CASEy, 'pop.h.y.rds',sep='.')
  fnamep <- file.path(DOUT_S_ROOTy,'ocn','post', fname)
  saveRDS(yls, file=fnamep) 
  return(yls)
}
# to process y from truth
CASEy        <- 'b.e12.B1850CN.f45_g37.1850_DAtest.000.h01'
DOUT_S_ROOTy <- file.path('/gfs2/work/hbkjgpin/cesm1_2_2/archive',CASEy)

yfoo <- foof(CASEy, DOUT_S_ROOTy, fname='/home/h/hbkjgpin/docs/palmod/data/MARGO/margo_LGM_SST.rds', CF$y_class)
# else for general ensemble use:
#yfoo <- readRDS(file.path(DOUT_S_ROOTy,'ocn','post',paste(CASEy, 'pop.h.y.rds',sep='.')))


if (is.null(CF[['y']])) {
  CF$y <- yfoo
} else {
  CF$y <- c(CF[['y']],yfoo)
}

rm(foof,yfoo)

# -------------------------------------
 if (1 > 2) { # NOT RUN
   #ystaTStr <-  "1920-01-01 00:00:00"                           # synthetic observations for the last 30 years of simulation
   #yendTStr <-  "1950-01-01 00:00:00"
   ystaTStr <-  "1920-01-01 00:00:00"                            # synthetic observations for the last 30 years of simulation
   yendTStr <-  "1950-01-01 00:00:00"
 
   ystaT    <-  as.POSIXct(ystaTStr, tz='GMT')
   yendT    <-  as.POSIXct(yendTStr, tz='GMT')
   ytime <- seq(ystaT,yendT,by='month')
   ytimeJFM <- ytime[months(ytime) %in% c('January','February','March')]
   ytimeJAS <- ytime[months(ytime) %in% c('July','August','September')]

   yls <- CF$y

   seasons <- substr(yTYPEs,nchar(yTYPEs)-2,nchar(yTYPEs))
   if (!all(seasons %in% c('JFM','AMJ','JAS','OND','ann')))
     stop('seasons not properly identified')

   for (i in 1:length(yTYPEs)) {
     yTYPE <- yTYPEs[i]
     if ( is.null(CF$y[[yKIND]][[yTYPE]][[yFMT]]))
       stop('readOb :: observation KIND-TYPE-FMT not present in CF')

     if (!CF$y[[yKIND]][[yTYPE]][[yFMT]]$use)
       next
 
     yls[[yKIND]][[yTYPE]][[yFMT]]$data        <- MARGO[[yKIND]][[yTYPE]][[yFMT]]
     yls[[yKIND]][[yTYPE]][[yFMT]]$data$y      <- MARGO[[yKIND]][[yTYPE]][[yFMT]]$WOA1998     # preliminary substitution by preindustrial observations - just for checks                                                -25,20), byrow=TRUE, ncol=2)
     if (seasons[i]=='JFM') {
       yls[[yKIND]][[yTYPE]][[yFMT]]$data$time <- ytimeJFM
     } else {
       yls[[yKIND]][[yTYPE]][[yFMT]]$data$time <- ytimeJAS
     }
     yls[[yKIND]][[yTYPE]][[yFMT]]$data$sensor <- 'MARGO locations synthetic SST'           # not used by DA
   }
 } # END NOT RUN
