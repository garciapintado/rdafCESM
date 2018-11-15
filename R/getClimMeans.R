getClimMeans <- function(tlimStr, DOUT_S_ROOT, CASE, getMonths=TRUE, allowNA=TRUE, comps=NULL, loop=NULL) {

  # tlimStr: limits for the statistics
  # DOUT_S_ROOT :: [m]   CHARACTER: path to CESM output archive
  # CASE        :: [m]   CHARACTER: name of CESM CASE
  # getMonhts   :: [1]   LOGICAL: if monthly means have been previous obtained. This can be set to FALSE
  # allowNA     :: [1]   LOGICAL: TRUE to stop in model archive output is not found
  # comps       :: [>=1] CHARACTER of model components, over which to conduct means. NULL for all model components below
  # loop        :: [1]   CHARACTER: If not NULL, output for each component $com will be stored in subfolder '$DOUT_S_ROOT/$com/post/$loop'  
  #
  # get climatological months, climatological seasons, and climatological years as netCDF file
  # this is a wrapper around the NCO command ncra
  # getMonths:: if FALSE, allows for reconstructing seasonal and annual means from previously generated climatic months

  Sys.MOVE   <- 'mv -fv'   
  allcomps <- c("atm",   "ice",     "lnd",     "ocn",  "rof")
  allcomph <- c("cam.h0", "cice.h", "clm2.h0", "pop.h","rtm.h0") # monthly input: $CASE.$comph.AAAA-MM.nc

  if (is.null(comps))
    comps <- allcomps
  if (!all(comps %in% allcomps))
    stop('getClimMeans:: input comps not defined in CESM compset')  

  comph <- allcomph[match(comps, allcomps)]

  tstaT    <-  as.POSIXct(tlimStr[1], tz='GMT')
  tendT    <-  as.POSIXct(tlimStr[2], tz='GMT')-1         # 1 second before the upper bound
  seq.month <- seq(tstaT,tendT,by='month')                # select a subset for analysis
  seq.mStr  <- substr(datePOSIXct(seq.month),1,7)

   seasons <- list()
   seasons$JFM <- 1:3
   seasons$AMJ <- 4:6
   seasons$JAS <- 7:9
   seasons$OND <- 10:12

  # get climatological months/seasons/years
  cwd <- getwd()
  yearRange <- range(substr(seq.mStr,1,4))
  m <- length(DOUT_S_ROOT)

  for (im in 1:m) {
    cat('getClimMeans:',DOUT_S_ROOT[im],'\n')

    for (ic in 1:length(comps)) {
      com <- comps[ic]
      coh <- comph[ic]      

      dsni <- file.path(DOUT_S_ROOT[im],com,'hist')
      if (!file.exists(dsni)) {
        if (allowNA)
          next
        else
          stop('getClimMeans: simulation not found for im: ',CASE[im])
      } 
      dsno <- file.path(DOUT_S_ROOT[im],com,'post')
      if (!is.null(loop))
        dsno <- file.path(dsno,loop)
    
      if(!file.exists(dsno))
        dir.create(dsno, recursive=TRUE)

      if (getMonths) {                                                          # montly means over time period
        for (imon in 1:12) {
          monStr <-  formatC(imon,width=2,flag='0')
          dnameo <- paste(paste(yearRange,collapse='-'),monStr,sep='-')
          seqids <- grep(paste('-',monStr,sep=''),seq.mStr)
          fnames <- paste(CASE[im],coh,seq.mStr[seqids],'nc',sep='.')
          setwd(dsni)
          climf  <- paste(CASE[im],coh,dnameo,'nc',sep='.')
          syscmd <- paste('ncra',paste(fnames, collapse=' '),climf,sep=' ')     # NCO: climatological months
          system(syscmd)
        }
        syscmd <- paste(Sys.MOVE,' ',CASE[im],'.',coh,'.',paste(yearRange,collapse='-'),'* ',dsno,sep='')
        system(syscmd)
      } # end if (getMonths)

      setwd(dsno)

      for (ise in 1:length(seasons)) {                                          # get climatological seasons
        dnamei <- paste(paste(yearRange,collapse='-'),formatC(seasons[[ise]],width=2,flag='0'),sep='-')
        dnameo <- paste(paste(yearRange,collapse='-'),names(seasons)[ise],                     sep='-')
        fnames <- paste(CASE[im],coh,dnamei,'nc',sep='.')
        climf  <- paste(CASE[im],coh,dnameo,'nc',sep='.')
        if (file.exists(file.path(dsno,climf)))
          file.remove(file.path(dsno,climf))
        syscmd <- paste('ncra',paste(fnames, collapse=' '),climf,sep=' ')  
        system(syscmd)
      }

      dnamei <- paste(paste(yearRange,collapse='-'),names(seasons),sep='-')
      dnameo <- paste(paste(yearRange,collapse='-'),'ann',sep='-')              # get climatological year
      fnames <- paste(CASE[im],coh,dnamei,'nc',sep='.')
      climf  <- paste(CASE[im],coh,dnameo,'nc',sep='.')
      if (file.exists(file.path(dsno,climf)))
        file.remove(file.path(dsno,climf))
      syscmd <- paste('ncra',paste(fnames, collapse=' '),climf,sep=' ')  
      system(syscmd)
    } # end for ic
  } # end for im
  setwd(cwd)
}

# example
# ------
if (1 > 2) { # NOT RUN
 HOME     <- Sys.getenv('HOME')                        # e.g. Creds:  user:hbkjgpin  group:hbkjgpin  account:hbk00056  class:mpp2q
 WORK2    <- Sys.getenv('WORK2')
 MODEL    <- 'cesm1_2_2'
 DAR      <- file.path(HOME,'docs','DA','R')

 RES      <- 'f45_g37'
 event    <- '1850_DAtest' # > preindustrial - DART sequential simulations
 COMPSET  <- 'B1850CN'
 nnn      <- '000'         # unique test identifier - do not confuse with m-member extension - which will be appended as a three digit code (.mmm) to nnn for each member

 source(file.path(DAR, 'utils', 'tlag.R'))
 source(file.path(DAR, 'utils', 'datePOSIXct.R'))
 source(file.path(HOME,MODEL,'scripts','R','utils','getClimMeans.R'))

 tlimStr     <- c("1890-01-01 00:00:00" ,"1910-01-01 00:00:00")                                         # 20 years of simulation for average statistics

 # e.g. single simulation, only atmospheric history files
 CASE <- paste('b.e12',COMPSET,RES,event,nnn,'000',sep='.')
 DOUT_S_ROOT <- file.path(WORK2,MODEL,'archive',CASE)
 loop <- NULL                                                                                           # not iterated

 # e.g. set of simulations, only atmospheric history files
 imsStr <- formatC(1:100,width=3,flag='0')
 CASE <- paste('b.e12',COMPSET,RES,event,nnn,imsStr,sep='.')
 DOUT_S_ROOT <- file.path(WORK2,MODEL,'archive',CASE)
 loop <- NULL                                            # not iterated

 # e.g. set of simulations, only atmospheric history files
 imsStr <- formatC(2:11,width=3,flag='0')
 nnn <- 'pIK' # large perturbations iterated Kalman smoother
 CASE <- paste('b.e12',COMPSET,RES,event,nnn,imsStr,sep='.')
 DOUT_S_ROOT <- file.path(WORK2,MODEL,'archive',CASE)
 loop <- 'f03'                                           # iterated: iteration label

 getClimMeans(tlimStr, DOUT_S_ROOT, CASE, comps='ocn', loop=loop)

 # e.g. set of m=1 simulations
 imsStr <- c('000','h01')
 for (imStr in imsStr) {
  CASE <- paste('b.e12',COMPSET,RES,event,nnn,imsStr,sep='.')
  DOUT_S_ROOT <- file.path(WORK2,MODEL,'archive',CASE)
  loop <- NULL                                           # iterated: iteration label
  

  getClimMeans(tlimStr, DOUT_S_ROOT, CASE, comps=NULL, loop=loop)
 }
 
 # additional
 dsn <- file.path(WORK2,MODEL,'archive','atm_post')
 if (!file.exists(dsn))
   dir.create(dsn)
 for (im in 1:length(imsStr)) {
   dsni <- file.path(WORK2,MODEL,'archive',CASE[im],'atm')
   dsno <- file.path(dsn,CASE[im],'atm')
   if (!file.exists(dsno)) {
     dir.create(dsno, recursive=TRUE)
   }
   if (file.exists(dsni)) {
     syscmd <- paste('cp -r ', dsni,'/post ',dsno,sep='')
     system(syscmd)
   }
 }
 setwd(file.path(WORK2,MODEL,'archive'))
 system('tar -czvf atm_post.tar.gz atm_post/')
 syscmd  <- paste('scp atm_post.tar.gz atomdisk:/mnt/marumfs2/jgp/cesm1_2_2/archive',sep='')
 syscmd  <- paste('scp atm_post.tar.gz hlogin:/gfs2/work/hbkjgpin/cesm1_2_2/archive', sep='')
 # end additional

}

