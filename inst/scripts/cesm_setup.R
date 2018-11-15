#!R

# Author :     Javier Garcia-Pintado [JGP]
# Institution: MARUM, University of Bremen
# ---------------------
# Purpose
# ---------------------
# cesm_setup high-level R interface

# Notes:
# adapted for CESM v1.2.2 - B COMPSETS

#
# This is designed to set-up a single-instance ensemble runs of CESM with a B compset,
# where CAM, POP, and CLM are all active, with a CESM hybrid setup.
#
# workflow:: - this script modifies CESM_DART_config and sends it to ${CASEROOT}
#            - modified ${CASEROOT}/CESM_DART_config is automatically run and inserts
#              a few dozen lines into the ${CASE}.run script after it makes a backup copy.
#  
# The methodology here reflects that paleoclimate observations are sparse in time. All of CESM
# stops at specific assimilation times for the DA/forecast sequence.
#
#  non interactive: submit from simulation folder as:
#  R CMD BATCH --no-save --no-restore-data --no-readline $HOME/cesm1_2_2/scripts/R/DA/CESM1_2_1_setup_pmo.R &
                 
# ==============================================================================

library(rdafCESM)
    
Sys.setenv(TZ='GMT')
Sys.setlocale('LC_ALL','C')
set.seed(1)                                                 # for reproducibility

if (!exists('ilR'))                                         # restart loop
  ilR <- 1

if (!exists('POST_PROCESS'))
 POST_PROCESS <- FALSE

# R-environment :: CONSTANTS & global variables

ppm <- 1.0e-06       # [-] parts per million
ppb <- 1.0e-09       # [-] parts per billion
ppt <- 1.0e-12       # [-] parts per trillion

km    <- 1000        # [m]
Sv    <- 1.0e+06     # [m3/s] sverdrup e.g. the global input of fresh water from rivers to ocean is ~1.2 Sv
                     #        the largest ocean current is the Antartic Circumpolar Current (ACC) with 100-150 Sv
hour  <- 3600        # [s]
day   <- 24*hour     # [s]

# HRLN system command default modifications

Sys.MOVE   <- 'mv -fv'                                                                           # The FORCE options are not optional.  
Sys.COPY   <- 'cp -f --preserve=timestamps' 
Sys.LINK   <- 'ln -fs'
Sys.REMOVE <- 'rm -fr'

# ==============================================================================
                                                                      # hlrnquota
USER     <- system('whoami', intern=TRUE)                             # e.g. 'hbkjgpin'
HOME     <- Sys.getenv('HOME')                                        
WORK     <- Sys.getenv('WORK')                                        # 3TB  <- lfs quota -u hbkjgpin /gfs1
WORK2    <- Sys.getenv('WORK2')                                       # 20TB <- lfs quota -u hbkjgpin /gfs2

MODEL    <- 'cesm1_2_2'
codebase <- 'e12'                                                     # stands for cesm1_2_x

# definition of variables that must be defined in the caller
# 

# set R environment
dsn      <- file.path(HOME,MODEL,'data')                              # HOME - data main input parent folder
dsndat   <- file.path(WORK2,MODEL,'data')                             # WORK2
source('readCASE.R')                                                  # shared among all members
region   <- RES                                                       # in readCASE
scn      <- file.path(COMPSET,nnn)
dsnscn   <- file.path(dsn, region, event, scn)

# CASE [name follows CESM casename conventions]
CASE_0 <- paste(tolower(substr(COMPSET,1,1)),                       # common for all members
                codebase,
                COMPSET,         # compset shortname
                RES,             # res shortname
 #              paste(event,'pmo',sep='_'),           # == opt_desc_string
                event,
                nnn,sep='.')
CASEROOT_0 <- file.path(dsndat,CASE_0)                              # common for all members
cat('CASEROOT_0:',CASEROOT_0,'\n')

# load general configuration parameters
source(file.path(dsnscn,'readCF.R'),       local=TRUE )               # make CF
source(file.path(dsnscn,'header_mc.R'),    local=TRUE)                # parameters for initial perturbations - matching M$ components
source(file.path(dsnscn,'readprm.R'),      local=TRUE)

# inherit [synchronise] available Monte Carlo constraints into CF$ana
Pthetab <- Pthetaa <- NULL                                            # [ntheta,ntheta], where ntheta == sum(CF$MC$flag)

for (i in 1:length(CF$ana)) {
  vname <- names(CF$ana)[i]
  if (vname %in% rownames(CF$MC)) {
    CF$ana[[i]]$min <- CF$MC[vname,'min']
    CF$ana[[i]]$max <- CF$MC[vname,'max']
  }
}

CF$hpc <- makeHPClist(CF$hpc)
isensemble <- ifelse(CF$m> 1 && !(prm$method %in% c('pIKF','pMKF')),TRUE,FALSE)

if (sum(CF$MC$flag) != CF$m && !isensemble)
  stop('main:: check ensemble size')

prm$m <- CF$m
if (prm$method %in% c('pIKF','pMKF'))
 prm$m <- CF$m + 1                     # background + required for ensemble

if (!prm$method %in% c('ETKF','pIKF','pMKF','oloop'))
  stop('unknown prm$method')

if (prm$method == 'oloop')
 prm$maxiter <- 0

source(file.path(dsnscn,'sourceMods.R'),   local=TRUE)                # SourceMods() called later after the CASE is created

if (prm$method != 'oloop') {
  if (!all(rownames(CF$MC[CF$MC$flag,]) %in% names(CF$ana)))
    stop('CESM-DA:: uncertain parameters [control variables] not included in analysis definition')
}

if (CF$makeTruth) {
  CASE <- paste(CASE_0, CF$truthext, sep='.')
} else { 
  CASE <- paste(CASE_0, formatC(1:prm$m,width=3,flag="0",format="d"), sep='.') # [m]
}

# DART & CESM environment variables
envcase  <- list(CASE=CASE,                                           # -> env_case.xml
                 CASEROOT=file.path(dsndat,CASE),                     # $CASE and $CASEROOT are vectors for CF$m > 1
                 CCSMROOT=file.path(HOME,MODEL),
                 MACH='HLRN3')
if (!is.null(CF$yfnames)) {
  for (iy in 1:length(CF$yfnames)) {
    source(file.path(dsnscn,CF$yfnames[iy]),        local=TRUE)                # read observations into CF$y
  }
}
source(file.path(dsnscn,'readRuntimes.R'), local=TRUE)                # read assimilation as   CF$runtimes
source(file.path(dsnscn,'envmachpes.R'), local=TRUE)                  # -> env_mach_pes.xml :: for each single ensemble member
source(file.path(dsnscn,'envbuild.R'),  local=TRUE )                  # -> env_build.xml :: length(envbuild$EXEROOT) == CF$m 

CF$RESUBMIT <- getNrestarts(c(CF$runtimes,CF$endT), CF$dtREST)        # vector: number of automatic model restarts for every runtime

# init times -> envrun$STOP_N
nrt <- length(CF$runtimes)                                            # number of integration times
it <- 1
CF$it <- it                                                           # just required for debug mode in analyse
CF$timenow <- CF$runtimes[it]                                         # required for initial envrun setup
if (it < nrt) {
  CF$timenex <- CF$runtimes[it+1]                                     # (POSIXct)
} else {
  CF$timenex <-  CF$endT
}

source(file.path(dsnscn,'envrun.R'),  local=TRUE )                    # -> env_run.xml :: length(...$DOUT_L_MSROOT, ...$DOUT_S_ROOT, ...$RUNDIR) == CF$m

M <- list()                                         # initial parameters. Parameter subject to uncertainty are specifically indicated in header_mc, which will overwrite values here
cat('reading DRV input\n')
source(file.path(dsnscn,'readDRV.R'),  local=TRUE ) # M:: CESM coupler v7
cat('reading CAM input\n')
source(file.path(dsnscn,'readCAM.R'),  local=TRUE ) # M:: CAM Community Atmosphere Model
cat('reading CICE input\n')
source(file.path(dsnscn,'readCICE.R'), local=TRUE)  # M:: Los Alamos sea ice model
cat('reading CLM input\n')
source(file.path(dsnscn,'readCLM.R'),  local=TRUE)  # M:: CLM4 Community Land Model
cat('reading RTM input\n')
source(file.path(dsnscn,'readRTM.R'),  local=TRUE)  # M:: river transport model
cat('reading POP2 input\n')
source(file.path(dsnscn,'readPOP2.R'), local=TRUE)  # M:: POP2 Parallel Ocean Program :: length(M$POP2$overflows_infile) == CF$m

  # I )  make the ensemble case working for both a) branch from a common simulation and b) branch from an ensemble
  # II)  then set up a run for all ensemble members so they are able to run in loops including assimilation updates via restart files

  # ==============================================================================
  # case options:
  # 
  # ==============================================================================

env <- list()                                                           # take elements as defined in CESM1_2_1_setup_pmo - system environment variables
  if (1 > 2) { # this group unneeded
    env$case       <- envcase$CASE                                        # [m] DART? possibly unneeded
    env$compset    <- COMPSET                                             # DART? possibly unneeded
    env$resolution <- RES                                                 # DART? possibly unneeded
    env$cesmtag    <- MODEL                                               # DART? possibly unneeded :: The version of the CESM source code to use when building the code.

    # ==============================================================================
    # machines and directories:
    #
    # comments in CESM1_2_1_setup_mpo
    # ==============================================================================

    env$mach      <-  envcase$MACH
    env$cesmroot  <-  envcase$CCSMROOT
    env$caseroot  <-  envcase$CASEROOT                                      # [m]
    env$cesmdata  <-  envrun$DIN_LOC_ROOT
    env$rundir    <-  envrun$RUNDIR                                         # [m]
    env$exeroot   <-  envbuild$EXEROOT                                      # [m]
    env$archdir   <-  envrun$DOUT_S_ROOT                                    # [m]

    #env$dartroot  <-  '/home/h/hbkjgpin/docs/DART/trunk'                    #           DART? 

    # ==============================================================================
    # configure settings:
    #
    # comments in CESM1_2_1_setup_mpo
    # ==============================================================================

    env$run_refcase <- envrun$RUN_REFCASE
    env$run_refdate <- envrun$RUN_REFDATE # specific date in the reference case space  [as 'yyyy-mm-dd' string]
    env$reftod      <- envrun$RUN_REFTOD  # time of day in seconds [as 'sssss' string]
    # a m-vector envrun$RUN_REFCASE can well do env$SingleInstanceRefcase work - so the latter is possibly unneeded
    env$SingleInstanceRefcase <- 1                                          # CESM multi-instance have an instance [ensemble member] number in filenames
                                                                            # as DART - set 1 for single instance [single member initial conditions]
                                                                            # 1  -> restart files have no member numbers
                                                                            # 0  -> restart fiels have '0001'- like member string in their names 
    env$TRUTHinstance         <- NULL                                       # If SingleInstanceRefcase == 1, this specific member is used --- unused by now
  
 
    # ==============================================================================
    # runtime settings:
    #
    #
    # If the long-term archiver is off, you get a chance to examine the files before
    # they get moved to long-term storage. You can always submit $CASE.l_archive
    # whenever you want to free up space in the short-term archive directory.
    # ==============================================================================

    env$run_startdate       <- envrun$RUN_STARTDATE                             # generally equal to the reference case date. 
    env$start_tod           <- envrun$START_TOD
    env$short_term_archiver <- envrun$DOUT_S                                    # [L] Copies the files from each job step to a 'rest' directory.
    env$long_term_archiver  <- envrun$DOUT_L_MS                                 # [L] Puts the files from all completed steps on tape storage.
    env$resubmit            <- envrun$RESUBMIT                                  # How many job steps to run on continue runs (should be 0 initially - adapt to obtain full intregations between assimilation steps)
    env$stop_n              <- envrun$STOP_N                                    # Number of time units in all subsequent forecasts
    env$stop_option         <- envrun$STOP_OPTION                               # Units for determining the forecast length between assimilations
    env$first_stop_n        <- 12                                               # DART- Number of time units in the first forecast 
    env$ACCOUNT <- CF$hpc$account_name
  } # end unneeded  

# useful combinations of time
env$reftimestamp <- paste(envrun$RUN_REFDATE,envrun$RUN_REFTOD,sep='-')                               # [yyyy-mm-dd-sssss]
env$stagedir     <- file.path(envrun$DIN_LOC_ROOT,'ccsm4_init', envrun$RUN_REFCASE, env$reftimestamp) # path to envrun$RUN_REFCASE restart files as prestage will be manual (envrun$GET_REFCASE==FALSE)
                                                                                                      # this will be (R script-managed) updated at each model restart time
                                                                                                      # single simulation for common initial conditions or m-length vector for sequential simulations
                                                                                                      # or externally generated initial conditions with random perturbations

  #clm_dtime <- 1800                                                          # [s] clm_dtime     CLM dynamical output timestep (in seconds) ... 1800 is the default
  # M$CLM$dtime # use instead of clm_dtime
  if (envrun$STOP_OPTION == 'nhours') {
    h1nsteps <-  envrun$STOP_N * hour / M$CLM$dtime                          # [s/s]this run integration time (including extra time over timenex] divided by CLM timestep
  } else if (envrun$STOP_OPTION == 'ndays') {                                #      h1nsteps      is the number of time steps to put in a single CLM .h1. file
    h1nsteps <-  envrun$STOP_N * day /  M$CLM$dtime                          #                    DART needs to know this and the only time it is known is during
  } else if (envrun$STOP_OPTION == 'nmonths') {                                                                   #               this configuration step. Changing the value later has no effect.
    h1nsteps <- as.numeric(difftime(tlag(CF$timenow, mlag=envrun$STOP_N), CF$timenow, units='secs')) / M$CLM$dtime
  } else {
    stop('main:: ---envrun$STOP_OPTION ERR001---')                 # TODO: here I need to evaluate further use of h1nsteps - as it may be daunting for paleoclimate
  }                                                                          # e.g. use h1nsteps for a F90 or NCO or CDO command to do time integration over monthly timesteps

  # ==============================================================================

  #set nonomatch       # suppress "rm" warnings if wildcard does not match anything

  # ==============================================================================

#for ( dname in c(envcase$CCSMROOT, env$dartroot, env$stagedir) ) {
#for ( dname in c(envcase$CCSMROOT, env$stagedir) ) {
#  if (!file.exists(dname))
#    stop('CESMsetup:: ---ERR002---')
#}; rm(dname)

  # ==============================================================================
  # Create the case - this creates the CASEROOT directory.
  #
  # For list of the pre-defined component sets: ./create_newcase -list
  # To create a variant compset, see the CESM documentation and carefully
  # incorporate any needed changes into this script.
  # ==============================================================================

if (!exists('MAKE_CASE'))
  MAKE_CASE <- CF$it_restart == 0 

if (MAKE_CASE) {                                                                                    # create CASEs & link to specific F90 modules
  if (getwd() %in% c(envcase$CASEROOT, envbuild$EXEROOT, envrun$RUNDIR))
    stop('CESM_setup:: --ERR003: setup script located in overwritten dirs--
          process halted to prevent removal')
 
  # if pre-existing, delete [truth/ensemble] case:
  for ( dname in c(envcase$CASEROOT, envbuild$EXEROOT, envrun$RUNDIR) ) {
    if (file.exists(dname)) {
    #  stop('CESM_setup:: non-empty [envcase$CASEROOT, envbuild$EXEROOT, envrun$RUNDIR]')
    }
    #unlink(dname, recursive=TRUE)          # R
    #system(paste(Sys.REMOVE,dname))         # shell
  }

  if (!file.exists(file.path(CASEROOT_0,'rest')))                                          # once at the start
    dir.create(file.path(CASEROOT_0,'rest'), recursive=TRUE)
  
  setwd(dsndat)
  syscmd <- paste(file.path(envcase$CCSMROOT,'scripts','create_newcase'),
                  '-case',   envcase$CASE,
                  '-res',    RES,
                  '-compset',COMPSET,
                  '-mach',   envcase$MACH)  # [m]
  sapply(syscmd, function(x) {system(x)})   # create true case [CF$makeTruth==TRUE] or ensemble of cases [CF$makeTruth==FALSE]

  # place specific Sourcemods
  for (im in 1:length(envcase$CASEROOT)) {
    if (CF$IMAU_ON) {   # - hosing
      system(paste(Sys.LINK,file.path(envcase$CCSMROOT,'mySourceMods/src.pop2/*'), file.path(envcase$CASEROOT[im],'SourceMods/src.pop2/')))
    }
    sourceMods()
  }
} # if MAKE_CASE

if (!POST_PROCESS) {

  # manually get CESM environment variables into shell
  if (MAKE_CASE) {
    for (im in 1:length(envcase$CASEROOT)) {
      setwd(envcase$CASEROOT[im])  # need to go into the specific folder
      system('Tools/ccsm_getenv')  # it appear all this changes is the two variables LINES (57 -> 42) & COLUMNS (211 -> 181)
                                   # and load cmake thus there is no need to run it for each ensemble member at this stage
    }; setwd(CASEROOT_0)
  }

  CF$sm <- rep(TRUE,prm$m)                                         # init surviving [stable] members
  
  MCgpar  <- NULL
  Pthetaa <- NULL                # initialize analysis parameter covariance

  # start looping
  for (it in 1:nrt) {                                                       # integrate in time batches ---with possible assimilation---
    thetal <- list()                                                        # list to store all theta lists for each it - just for posterior analysis
    itStr  <- formatC(it, width=4, flag='0')
    for (il in ilR:(prm$maxiter+1)) {
      ilStr <-  paste('f',formatC(il,width=2,flag='0'),sep='')
      cat('Loop number: ',it,' | ',ilStr,'\n')
      cat('surviving members:',sum(CF$sm),'out of',prm$m,'\n',sep=' ')
      cat('survivors:',which(CF$sm),'\n')
      cat('casualties:',which(!CF$sm),'\n')

      if (prm$method %in% c('ETKF','pMKF') && il <= prm$maxiter) {
        prm$rfactor <- ((prm$maxiter:1) * sum(1/(prm$maxiter:1)))[il]      # [1] scaling for R for fractional filters
      } else {
        prm$rfactor <- 1        # [1] scaling for R for fractional filters
      }

      CF$it <- it                                                          # just required for debug mode in analyse
      CF$timenow <- CF$runtimes[it]                                        # (POSIXct)
      if (it < nrt) {
        CF$timenex <- CF$runtimes[it+1]                                    # (POSIXct)
        CF$freerun <- FALSE
      } else {
        CF$timenex <-  CF$endT
        CF$freerun <- TRUE
      }
      if (CF$it < CF$it_restart)                                           # CF$it_restart=0 for initial runs
        next

      RESTART <- CF$it == CF$it_restart                                    # FLAGS
      if (MAKE_CASE) {
        BUILD <- TRUE
        MAKE_CASE <- FALSE
      } else {
        BUILD <- FALSE
      }
      cat('CESM setup:: BUILD:',BUILD,'\n')

      timenowStr  <- paste(datePOSIXct(CF$timenow),'00000',sep='-')
      timenexStr  <- paste(datePOSIXct(CF$timenex),'00000',sep='-')
      timenowMStr <- paste(datePOSIXct(CF$timenow+CF$tdiff),'00000',sep='-')   # model space
      timenexMStr <- paste(datePOSIXct(CF$timenex+CF$tdiff),'00000',sep='-')   # model space

      dsnrest <- file.path(CASEROOT_0,'rest', timenowMStr) # assumes short-term archiving | path to MC generated sets
      #if (length(thetal) == 0 && il > 1 && prm$method %in% c('pIKF','pMKF'))
      #  thetal  <- readRDS(file.path(dsnrest,paste(CASE_0,'thetal',timenowStr,'rds',sep='.')))
      #if (length(thetal) == 0)
      #   Pthetab <- Pthetaa                          # calculate Pthetaa always at the end of each il loop - except for 'oloop' for which it does not evolve
      #else
      #   Pthetab <- thetal[[il-1]]$Pthetaa    

      if (!CF$makeTruth) {
        if (RESTART) {
          # load previous parameter ensemble
          if (prm$method %in% c('oloop','pIKF','pMKF'))
            gparTheta <- readRDS(file.path(dsnrest,paste(CASE_0,'gparTheta',timenowStr,ilStr,'rds',sep='.')))        # save to be able to restart 
          MCgpar    <- readRDS(file.path(dsnrest,paste(CASE_0,'MCgpar.b',timenowStr,ilStr,'rds',sep='.')))         # .b stands for background
          #    MCgpar_recovered    <- readRDS(file.path(dsnrest,paste(CASE_0,'MCgpar.b',timenowStr,'rds',sep='.')))  
        } else { # !RESTART
          if (BUILD) {                                                                 # just applies to it == 1 && initial ensemble runs)
            # we do not include random initial conditions
            # we could either perturb the initial condition from a common restart file set, or use an ensemble of initial conditions restarts 
            if (length(thetal) == 0)
             thetal[[1]] <- list()     

            if (prm$method %in% c('oloop','pIKF','pMKF')) {
              gparTheta <- orthoEnsemble(CF$p, CF$MC, CF$m, CF$MCcons, prm$sdfac)    # background plus one list per uncertain parameter
              thetaNam  <- gparTheta$thetaNam                                        # ntheta <- length(thetaNam)
              MCgpar <- do.call(cbind,gparTheta[c('b',thetaNam)])                    # just background for m==1
              colnames(MCgpar) <- c('b',rep(thetaNam, each=CF$m/length(thetaNam)))
            } else if (prm$method == 'ETKF') {
              MCgpar <- mcEnsemble(CF$p, CF$MC, CF$m, CF$MCcons, Pthetab)   
              thetaNam <- rownames(MCgpar)[which(CF$MC$flag)]                        # [ntheta]
            } else {
              stop("DA method not in ['ETKF','pIKF','pMKF']")
            }
          } else {                                            # !BUILD :: if (it > 1 || il > 1)
            if (prm$method %in% c('oloop','pIKF','pMKF')) {
              if (il == ilR) {
                thetal  <- readRDS(file.path(dsnrest,paste(CASE_0,'thetal',timenowStr,'rds',sep='.')))
              }
              if (prm$method == 'pMKF')
                Pthetab <- thetal[[il-1]]$Pthetaa
              else             
                Pthetab <- thetal[[1]]$Pthetab                                       # do not modify
              p <- CF$p
              vKINDs <- names(thetal[[1]]$b)
              for (iv in 1:length(vKINDs)) {
                vKIND <- vKINDs[iv]
                p[[vKIND]][1] <- thetal[[il-1]]$a[iv]     # mean
                p[[vKIND]][2] <- sqrt(Pthetab[iv,iv])     # sd
              }
              #prm$sdfac <- abs(prm$sdfac) * sign(as.numeric(thetal[[il-1]]$a - thetal[[il-1]]$b)) # take previous increment signs to guide perturbations
              # thetaTrue <- c(0.8, 0.91, 0.0035, 0.0035, 1.0E-06, 791.6*ppb, 284.7*ppm, 0.3, 0.68, 0.16, 4.0E+07, 0.0075) 
              # prm$sdfac <- (thetaTrue - as.numeric(theta$a)) / sqrt(diag(Pthetab))
              # prm$sdfac[3] <- -2.0 # the above crashed for im=48
              prm$sdfac[prm$sdfac == 0] <- 1
              gparTheta <- orthoEnsemble(p, CF$MC, CF$m, CF$MCcons, prm$sdfac)
              thetaNam  <- gparTheta$thetaNam                                        # [ntheta]
              MCgpar <- do.call(cbind,gparTheta[c('b',thetaNam)])                    # just background for m==1
              colnames(MCgpar) <- c('b',rep(thetaNam, each=CF$m/length(thetaNam)))
              p <- NULL                                                              # NULLIFY
            } else if (prm$method == 'ETKF') {                                       # MCgpar explicitly carried out
              # thetaNam <- rownames(MCgpar)[which(CF$MC$flag)]                        # [ntheta]
            }
          } # end if !BUILD
        } # if !RESTART

        if (!file.exists(dsnrest))
          dir.create(dsnrest)
        if (!RESTART) {                                                                                                 # else, no need to overwrite
          if (prm$method %in% c('oloop','pIKF','pMKF'))
            saveRDS(gparTheta, file=file.path(dsnrest,paste(CASE_0,'gparTheta',timenowStr,ilStr,'rds',sep='.')))        # save to be able to restart 
          saveRDS(MCgpar,      file=file.path(dsnrest,paste(CASE_0,'MCgpar.b',timenowStr,ilStr,'rds',sep='.')))         # save MCgpar as background parameter space
        }

        if (prm$method %in% c('oloop','pIKF','pMKF')) {                    # parameter only estimate - 1st ensemble member is background
          CF$sm[] <- TRUE                                                  # sample is regenerated  
          theta    <- list()  
          theta$b  <- gparTheta$b[thetaNam]
          theta$d  <- gparTheta$dtheta                                     # NULL for m==1 | [ntheta] perturbation vector  for parameters to be estimated
          theta$MCgpar <- MCgpar

          if (!is.null(Pthetab)) {
            theta$Pthetab <- Pthetab                                       # [ntheta,ntheta]
          } else {
            theta$Pthetab <- diag(sapply(CF$p[thetaNam],FUN=function(x){x[2]})^2)
          }
          if (il == 1) {       # only valid for it==1
         #   thetal[[1]]$b        <- c(0.75, 0.88, 0.0202, 0.0202, 5.0E-06, 800E-09, 300E-06, 0.19, 4.2E+07, 0.0)
            thetal[[1]]$b <- theta$b 
            names(thetal[[1]]$b) <- names(theta$b)
            thetal[[1]]$Pthetab  <- diag(sapply(CF$p[thetaNam],FUN=function(x){x[2]})^2)
            # temporary to recover original background in the synthetic scenario preindustrial CESM test
            # thetalB0 <- readRDS(file.path('/gfs2/work/hbkjgpin/cesm1_2_2/data/b.e12.B1850CN.f45_g37.1850_DAtest.pF1/rest/1850-01-01-00000',
            #                               'b.e12.B1850CN.f45_g37.1850_DAtest.pF1.thetal.1850-01-01-00000.rds'))
            #theta$b0 <- thetalB0[[1]]$b0 
            #theta$b0 <-  c(0.75, 0.88, 0.0202, 0.0202, 5.0E-06, 800E-09, 300E-06, 4.0e-01, 7.3e-01,  0.19, 4.2E+07, 0.0)
          }
          theta$b0       <- thetal[[1]]$b
          theta$Pthetab0 <- thetal[[1]]$Pthetab
        } else {
          theta <- list()                                                    # ETKF
        }

      } # end  if (!CF$makeTruth)

      # b.1 - update CF, envrun, env - truth and ensemble
      if (it > 1 && il == 1) {                                                         # update initial input - TODO: check if this is also neede at each il
        envrun$REST_N        <- elapsed_months(CF$timenow,CF$timenex) 
        envrun$RUN_REFCASE   <- envcase$CASE
        envrun$RUN_REFDATE   <- datePOSIXct(CF$timenow)    # restart files from last integration
        envrun$RUN_REFTOD    <- todPOSIXct(CF$timenow+CF$tdidd)
        envrun$RUN_STARTDATE <- envrun$RUN_REFDATE
        envrun$START_TOD     <- envrun$RUN_REFTOD
        envrun$STOP_N        <- elapsed_months(CF$timenow,CF$timenex)+1
        #envrun$RUN_TYPE      <- 'branch'
        envrun$RUN_TYPE      <- 'hybrid' # initial for CICE - so time is synchronized with coupler time?
        envrun$BRNCH_RETAIN_CASENAME <- TRUE
   
        env$reftimestamp <- paste(envrun$RUN_REFDATE,envrun$RUN_REFTOD,sep='-') # [yyyy-mm-dd-sssss string] DART? 
        if (envrun$DOUT_L_MS) {
          env$stagedir    <- file.path(envrun$DOUT_L_MSROOT, 'rest', env$reftimestamp)
          env$stagedirNex <- file.path(envrun$DOUT_L_MSROOT, 'rest', timenexStr)
        } else if (envrun$DOUT_S) {
          env$stagedir    <- file.path(envrun$DOUT_S_ROOT, 'rest', env$reftimestamp)
          env$stagedirNex <- file.path(envrun$DOUT_S_ROOT, 'rest', timenexStr)
        } else {
          stop('path to restart files not properly identified')
        }
      }

      # Save a copy of .xml files for debug purposes (just for initial timestep)
      if (BUILD) {
        for (im in 1:length(envcase$CASEROOT)) { # for truth or ensemble
          setwd(envcase$CASEROOT[im])            # need to go into the specific folder
          fnames <- dir(pattern='xml$')
          fnameo <- paste(fnames,'original',sep='.')
          for (i in 1:length(fnames)) {
            if (!file.exists(fnameo[i]))
              system(paste(Sys.COPY,fnames[i],fnameo[i])) 
          }
        }; setwd(CASEROOT_0)
      }

      # NOTE: 'env_mach_pes.xml' is locked by 'cesm_setup', unless setup is cleaned as:
      #        system('cesm_setup -clean') - just write now for all simulation time
      m  <- length(envcase$CASEROOT)               # != CF$m
      sm    <- rep(TRUE,m)                         # for this loop
      mdone <- rep(FALSE,m)                        # for this loop
#------------
#---------------------------------------------------
      if (!RESTART) { # else, jump into assimilation stage
        cat('CESM setup: setting env_mach_pes.xml and env_run.xml\n')
        for (im in 1:m) {                            # for truth & ensemble
          setwd(envcase$CASEROOT[im])
          #system(paste(Sys.COPY,'env_run.xml.original',     'env_run.xml')) 
          if (BUILD) {
            if (length(envmachpes) > 0) # as it can be an empty list for defautl values
              xmlchanges('env_mach_pes.xml', envmachpes)
          }

          xmlchanges('env_run.xml', envrun, im)
        }; setwd(CASEROOT_0)

        if (BUILD) {
          cat('CESM setup: invoking the cesm_setup script\n')
          for (im in 1:m) {                                                 # for truth & ensemble
            setwd(envcase$CASEROOT[im])
            system('cesm_setup')                                            # create: Macros, user_nl_xxx, $CASE.run, CaseDocs/ & env_derived
          };  setwd(CASEROOT_0)
        }

        # prestage
        if (!envrun$GET_REFCASE) {                                          # GET_REFCASE == TRUE for cesm-automated pre-staging
          if (envrun$RUN_TYPE != 'startup') {
            cat('CESM setup: R-managed prestaging for it==1 & envrun$RUN_REFCASE:',envrun$RUN_REFCASE,'\n')
            if (length(env$stagedir) == 1 && m > 1) {  # just applies to it==1 && ensemble case
              env$stagedir <- rep(env$stagedir, m)     # rep(,m)
            }
            if (length(envrun$RUN_REFCASE) == 1 && m > 1) {  # just applies to it==1 && ensemble case
              envrun$RUN_REFCASE <- rep(envrun$RUN_REFCASE, m)     # rep(,m)
            }
            for (im in 1:m) {
              system(paste(Sys.REMOVE,file.path(envrun$RUNDIR[im],'rpointer.*')))
              system(paste(Sys.REMOVE,file.path(envrun$RUNDIR[im],paste(envcase$CASE[im],'*',sep='.'))))
              sm[im]     <- file.exists(env$stagedir[im])
              if (!is.null(env$stagedirNex)) {
                mdone[im]  <- file.exists(env$stagedirNex[im]) }
              if (sm[im] && !mdone[im]) {
                system(paste(Sys.LINK,  file.path(env$stagedir[im],'*'),envrun$RUNDIR[im],sep=' '))
                system(paste(Sys.REMOVE,file.path(envrun$RUNDIR[im],'rpointer.*')))
                system(paste(Sys.COPY, file.path(env$stagedir[im],'rpointer.*'),envrun$RUNDIR[im]))
                pointerforce <- dir(envrun$RUNDIR[im], pattern='force$')
                if (length(pointerforce) > 0) {
                  for (ipf in 1:length(pointerforce)) {
                     system(paste(Sys.COPY, file.path(envrun$RUNDIR[im],pointerforce[ipf]),file.path(envrun$RUNDIR[im],gsub('.force','',pointerforce[ipf]))))
                  } 
                }
              }
            }
            cat('it:', CF$it,'| il:',il,'| casualties:',which(!sm),  '\n')
            cat('it:', CF$it,'| il:',il,'| mdone:',     which(mdone),'\n')
          } # end if (startup)
        } # end if (!envrun$GET_REFCASE)

        if (BUILD) { 
          # NOTE: 'env_build.xml' and 'Macros' are locked when model is built, unless built is cleaned as:
          #       system(paste(CASE,'clean_build -clean',sep='.'))
          for (im in 1:m) {
            if (!mdone[im]) {
              setwd(envcase$CASEROOT[im])
              # system(paste(envcase$CASE[im],'clean_build',sep='.')) 
              xmlchanges('env_build.xml', envbuild, im)
            }
          }; setwd(CASEROOT_0)
        } # end if (BUILD)

        # map random parameters in MCgpar into M [ensemble input provided as lists] -- needs to be done at each DA/forecast loop
        if (!CF$makeTruth) {
          for (k in 1:nrow(MCgpar)) {
            mcomvar <- rownames(MCgpar)[k]
            mcomvar <- strsplit(mcomvar,'.', fixed=TRUE)[[1]] # c(mcomp, variable)
            if (length(mcomvar) > 2) # defined as namelist.varname
              mcomvar <- c(mcomvar[1],paste(mcomvar[-1],collapse='.'))
            M[[mcomvar[1]]][[mcomvar[2]]] <- as.list(MCgpar[k,])
          }
        }

        # user modifications of user_nl_xxx       - $CCSMROOT/models/drv/bld/build-namelist         |  valid for truth & ensemble                           
        # M$ has possibly been modified by the assimilation
        modifyCESM(M$DRV, 'DRV', envcase$CASEROOT, 'user_nl_cpl') 
        modifyCESM(M$CAM, 'CAM', envcase$CASEROOT, 'user_nl_cam') # -> SCRATCH/$CASE/run/atm_in
        if (envrun$RUN_TYPE == 'startup') {
          modifyCESM(M$CLM, 'CLM', envcase$CASEROOT, 'user_nl_clm', except=CLMexceptions)                # exceptions described in user_nl_clm 
        } else {
          modifyCESM(M$CLM, 'CLM', envcase$CASEROOT, 'user_nl_clm', except=c(CLMexceptions,'finidat'))   # finidat given by RUN_REFCASE/RUN_REFDATE/RUN_REFTOD  
        }
        modifyCESM(M$RTM, 'RTM', envcase$CASEROOT, 'user_nl_rtm')
        modifyCESM(M$CICE,'CICE',envcase$CASEROOT, 'user_nl_cice')
        modifyCESM(M$POP2,'POP2',envcase$CASEROOT, 'user_nl_pop2', except=POP2exceptions)

        if (CF$IMAU_ON) {
          # specific user modification of user_nl_pop2 for hosing input
          addIMAU(M$POP2$IMAU, envcase$CASEROOT)
 
          # add initial/updated forcing files for the hosing input
          ncmsk   <- nc_open(M$POP2$imau_filename_msk, write=TRUE)
          FW_mask <- ncvar_get(ncmsk,'FW_mask')
          if (it == 1 && CF$makeTruth) {
            system(paste(Sys.COPY, M$POP2$imau_filename_wei, M$POP2$IMAU$imau_filename ))
            nc <- nc_open(M$POP2$IMAU$imau_filename, write=TRUE)   
            FW_liquid <- ncvar_get(nc,'FW_liquid')
            FW_liquid[FW_mask==1] <-  FW_liquid[FW_mask==1]*M$POP2$imau_gis   # recycles FW_mask | TODO: make generic to apply multipliers to specific hosing classes
            ncvar_put(nc,'FW_liquid',vals=FW_liquid)
            nc_close(nc)
          }
          if ((!CF$makeTruth) && (it == 1 || CF$ana$POP2.imau_gis$u)) {
            for (im in 1:length(envcase$CASEROOT)) {
              system(paste(Sys.COPY, M$POP2$imau_filename_wei, M$POP2$IMAU$imau_filename[im]))
              nc <- nc_open(M$POP2$IMAU$imau_filename[im], write=TRUE)   
              FW_liquid <- ncvar_get(nc,'FW_liquid')
              FW_liquid[FW_mask==1] <-  FW_liquid[FW_mask==1]*M$POP2$imau_gis[[im]]   # recycles FW_mask | TODO: make generic to apply multipliers to specific hosing classes
              ncvar_put(nc,'FW_liquid',vals=FW_liquid)
              nc_close(nc);
              #system(paste(Sys.LINK, M$POP2$IMAU$imau_filename[im],M$POP2$IMAU$imau_filename_prev[im])) - no need : same file
              #system(paste(Sys.LINK, M$POP2$IMAU$imau_filename[im],M$POP2$IMAU$imau_filename_next[im]))
            }
          }
          nc_close(ncmsk)
        } # end if CF$IMAU_ON

        # CH3. for it==1 build executable : create component namelists, check dataset (download if required)
        #                      create utility libraries, and component libraries & create the model executable
        # The namelist files created in the CaseDocs/ are there only for user reference and SHOULD NOT BE EDITED
        for (im in 1:m) {                 # for truth or ensemble
          setwd(envcase$CASEROOT[im])                            # need to go into the specific folder
          if (BUILD) {
            cat('CESM setup:: building model\n')
            system(paste(envcase$CASE[im],'build',sep='.'))      # include automatic prestaging (if envrun$GET_REFCASE == TRUE) 
          } else {                                               # manually get CESM environment variables into shell
            system('preview_namelists')                          # namelists should not be edited manually - they are overwritten however, every time $CASE.build or $CASE.run are called.
            sysenv <- Sys.getenv()
          }
        }; setwd(CASEROOT_0)

        # modify $CASE.run job submission parameters, initially set in .../scripts/ccsm_utils/mkbatch.HLRN3
        if (envrun$STOP_OPTION == 'nmonths') {
          WALLTIME <- envrun$STOP_N * 31 * 24 / CF$MThroughP + 1 # [h] computing walltime | + 1h to initialise model  
        } else if (envrun$STOP_OPTION == 'ndays') {
          WALLTIME <- envrun$STOP_N *      24 / CF$MThroughP  + 1 # [h]
        } else {
          cat('cesm_main:: prepare walltime for declared STOP_OPTION\n')
        }
        if (WALLTIME > 12) {
          stop('walltime > 12h --- re-schedule CF$runtimes') }

        CF$hpc$walltime <- paste(formatC(round(deg2gms(WALLTIME)), width=2, flag=0),collapse=':')
        CF$hpc$feature  <- ifelse(WALLTIME <= 1.0, 'mpp2:test','mpp2')
        CF$hpc$q        <- ifelse(WALLTIME <= 1.0, 'mpp2testq','mpp2q')

        err <- setHLRNrun(envcase, CF$hpc)
        if (err != 0)
          stop('main:: ---setHLRNrun error: ERR004---')

        # CH4. run model
        mrun <- m
        if (il > prm$maxiter)
         mrun <- 1 # just integrate the first member - even if more members parameters have been generated
        for (im in 1:mrun) {
         if (sm[im] && !mdone[im]) {
            setwd(envcase$CASEROOT[im])
            system(paste(envcase$CASE[im],'submit',sep='.'))
          }
        }; setwd(CASEROOT_0)
        # in case the session is interrupted and want to go on fron here:
        # save.image(file=file.path(dsnscn,'2018-05-11.RData'))
        # check job completion
        jobdone <- FALSE
        ctini <- Sys.time() # computer time - initial

        while (!jobdone) {
          total_ctime <- difftime(Sys.time(),ctini, units='mins')
          cat('running job...| total [queued+computing] elapsed time:',total_ctime,'[mins]\n')
          showq <- system("showq | grep 'hbk'", intern=TRUE) # all jobs by group
          # to delete a job see jobid with qstat- Then qdel joibid; e.g. qdel 861867.hbatch.hsn.hlrn.de
          # checkjob -c jobid (---jobid as shown by qshow--- for job details)
          if (length(grep(pattern='hbkjgpin', showq)) == 0) {
            jobdone <- TRUE }
          Sys.sleep(60)
        }; cat('cesm_setup:: jobdone!\n')

        # I think this is just for LGM -- CHECK!
        if (it == 1) {
          Sys.sleep(30)                      # allow some time for initial built up in this run
          M$POP2$init_ts_file_fmt <- 'nc'    # after initial binary T,S fields, there seems to be a bug, so RESTART_FMT=nc in rpointer.ocn.restart is overwritten by this and an error is given
          modifyCESM(M$POP2,'POP2',envcase$CASEROOT, 'user_nl_pop2', except=POP2exceptions) # thus this need to be modified for the next ccsm_continue run type
          modifyCESM(M$CAM, 'CAM', envcase$CASEROOT, 'user_nl_cam') # -> SCRATCH/$CASE/run/atm_in
        }

        if (CF$delete_ocn_daily) { # release some storage -- assume this won't be used in our analyses
          for (im in 1:mrun) {
            system(paste(Sys.REMOVE,file.path(envrun$DOUT_S_ROOT[im],'ocn','hist',
                         paste(envcase$CASE[im],'pop.h.nday*',sep='.'))))
          }
        }

        #if (!CF$makeTruth) {
        #  for (im in 1:length(envcase$CASEROOT)) {
        #    dsnrest <- file.path(envrun$DOUT_S_ROOT[im],'rest',paste(datePOSIXct(CF$timenex),'00000',sep='-')) # assumes short-term archiving
        #    dsnrb <- paste(dsnrest,'bckgrnd',sep='_')
        #    dir.create(dsnrb)
        #    system(paste(Sys.COPY,'-r',dsnrest,dsnrb))  # copy restart as background files [also serves as backup]
        #  }; rm(dsnrest)
        #}
      } # end if (!RESTART)

      if (!CF$makeTruth) { # assimilation steps
        if (!RESTART) {
          getClimMeans(tlimyStr, envrun$DOUT_S_ROOT, envcase$CASE, getMonths=TRUE, 
                       allowNA=ifelse(prm$method %in% c('pIKF','pMKF'),FALSE,TRUE), comps=NULL, loop=ilStr)
        }
        if (1 > 2) { # from synthetic truth
          getClimMeans(tlimyStr,  envrun$DOUT_S_ROOTy, CASEy, getMonths=TRUE, allowNA=FALSE, comps=NULL)
        }

        # re-generate gauDA for il==1 to get background cost-function
        #  Elst_a <- cesmDA(envcase, DOUT_S_ROOT, POP2vnamesU, tavgs, tlimyStr, timenowStr, 'f01', 
        #                   ana=CF$ana, MC=CF$MC, useGA=CF$useGA, yls=CF$y, theta=thetal[[1]], MCgpar=thetal[[1]]$MCgpar, analysis_scn=analysis_scn, 
        #                   GET_ANALYSIS=GET_ANALYSIS, dsnrest=dsnrest)
        #  gauDA <- readRDS(file.path(dsnrest, paste(CASE_0,"gauDA",timenowStr,'f01',"rds",sep=".")))
        #  costY     <- getCostY(gauDA)
        #  costTheta <- getCostTheta(thetal[[1]]) 

        #comps         <- list('atm'=c('TREFHT','PRECC','PRECL','CLDTOT'), #  TREFHT is "2 m temperature" or "near-surface air temperature"
        #                          'ocn'=c('TEMP','SALT','HMXL','MOC')) 

        comps <- list('ocn'=POP2vnamesU) # tmp: expand to any COMPONENT-xKIND

        Elst_a <- cesmDA(envcase, envrun$DOUT_S_ROOT, comps, tavgs, tlimyStr, timenowStr, ilStr, 
                         ana=CF$ana, MC=CF$MC, prm=prm, useGA=CF$useGA, useGAy=CF$useGAy, yls=CF$y, theta=theta, MCgpar=MCgpar, analysis_scn=analysis_scn, 
                         GET_ANALYSIS=GET_ANALYSIS, dsnrest=dsnrest, POP.zlrange=POP.zlrange)

        gauDA <- readRDS(file.path(dsnrest, paste(CASE_0,"gauDA_raw",timenowStr,ilStr,"rds",sep=".")))
        costY     <- getCostY(gauDA)
        costTheta <- getCostTheta(theta)        
        cost <- list(costY=costY, costTheta=costTheta)
        saveRDS(cost, file=file.path(dsnrest,paste(CASE_0,'cost',timenowStr,ilStr,'rds',sep='.')))  

 
        if (is.null(Elst_a))
          break

        if (!isensemble) { # specify operations for each prm$method
          theta <- Elst_a                                                                           # slots: d,b,a,Pthetab,Pthetaa,PHT,HPHT,K,MCgpar
          Pthetaa <- theta$Pthetaa
          Elst_a <- NULL                                                                            # deallocate memory
        } # cbind(theta$b,as.numeric(theta$a))

        # copy history files to specific fraction folder for iterated filters
        # WARNING! this creates a huge amount of output data. 
        # Note climate averages (but not evolutions) are already obtained by getClimMeans() ans stored in ilStr for each component  
        SAVE_ALL <- FALSE
        if (SAVE_ALL) {
          comp <- c('atm','cpl','dart','glc','ice','lnd','ocn','rest','rof','wav')
          for (im in 1:m) {
            if (!file.exists(file.path(envrun$DOUT_S_ROOT[im],ilStr)))
              dir.create(file.path(envrun$DOUT_S_ROOT[im],ilStr))
            for (icomp in 1:length(comp)) {
              system(paste(Sys.COPY,'-r',file.path(envrun$DOUT_S_ROOT[im],comp[icomp]),file.path(envrun$DOUT_S_ROOT[im],ilStr)))  
            }
          }
        }
        saveRDS(theta, file=file.path(dsnrest,paste(CASE_0,'theta',timenowStr,ilStr,'rds',sep='.')))  
        
        #rm(dsnrest)
      } # end if (!CF$makeTruth)
      thetal[[il]] <- theta
      saveRDS(thetal, file=file.path(dsnrest,paste(CASE_0,'thetal',timenowStr,'rds',sep='.')))  
      # data.frame(b=thetal[[1]]$b,a.f01=as.numeric(thetal[[1]]$a),a.f02=as.numeric(thetal[[2]]$a),a.f03=as.numeric(thetal[[3]]$a))
      # data.frame(vname=names(thetal[[1]]$b), sdb=sqrt(diag(thetal[[1]]$Pthetab)), sda.f01=sqrt(diag(thetal[[1]]$Pthetaa)), sda.f02=sqrt(diag(thetal[[2]]$Pthetaa)), sda.f03=sqrt(diag(thetal[[3]]$Pthetaa)))
    } # end for il
  } # end for (it in 1:nrt)
} # end if (!POST_PROCESS)


  # --- Postjob evaluation ---
  # Note: everything from here is a temporary code.
  # I need to evaluate how feasible is to conduct simple DA experiments whithin R workspace
  # and the best strategy to post process CESM integrations. NCO? CDO? NCL?
  # as well as the restart details and capabilities
if (1 > 2) {
 library(ncdf4)
 im    <- 1
 imStr <- '000' # truth here
 cmp   <- 'ocn'
 seq.month <- seq(CF$staT,CF$timenow,by='month')                 # select a subset for analysis
 seq.mStr  <- substr(datePOSIXct(seq.month),1,7)
 dsno <- file.path(envrun$DOUT_S_ROOT[im],cmp,'hist')            # SHORT term archiving
 fnames <- paste(envcase$CASE[im],'pop.h',seq.mStr,'nc',sep='.') # monthly history files

 nci <- nc_open(file.path(dsno,fnames[1]))
 VVEL     <- ncvar_get(nci,'VVEL')                               # "Velocity in grid-y direction"
 VVEL.att <- ncatt_get(nci,'VVEL')
 glon <- nci$var$VVEL$dim[[1]]$vals # gridded lons [not degrees but order]
 glat <- nci$var$VVEL$dim[[2]]$vals # gridded lons [not degrees but order]
 z_t  <- nci$var$VVEL$dim[[3]]$vals # gridded lons [not degrees but order]  
 tim  <- nci$var$VVEL$dim[[4]]$vals
}
#  source('getMOC.R')
# evaluate: streamfunction
#           geopotential heights
#           winds
#           surface temp 

#
#if (!file.exists('CESM_DART_config'))
# system(paste(COPY, env$dartroot, '/models/CESM/shell_scripts/CESM_DART_config .',sep=''))
#
#system(paste("perl -pi -e 's#BOGUS_DART_ROOT_STRING#",env$dartroot,"#g' CESM_DART_config",sep=""))
#system(paste("perl -pi -e 's#HISTORY_OUTPUT_INTERVAL#",envrun$STOP_N,"#g' CESM_DART_config",sep=""))          # hist_nhtfrq
#system('chmod 755 CESM_DART_config')

# TODO from here ---------- check against CESM1_2_1_setup_pmo for ensemble management: 
# ==============================================================================
# Configure the case.
# ==============================================================================

# MAX_TASKS_PER_NODE is defined in CCSMROOT/scripts/ccsm_utils/Machines/config_machines.xml [e.g. 24 for HLRN3]
#@ ptile = $MAX_TASKS_PER_NODE / 2
#@ nthreads = 1

# This is a decent layout for a single instance run.

# check MPI in env_run.xml - not neeeded for HLRN
# if (xmlquery(envcase$CASEROOT[1], 'MPI_RUN_COMMAND', valonly=TRUE) == 'UNSET')
 
# echo "task partitioning ... perhaps ... atm // ocn // lnd+ice+glc+rof"

# COUPLING discussion.
# http://bugs.cgd.ucar.edu/show_bug.cgi?id=1740
# "In summary, users should ensure that the following is true,
#  ATM_NCPL = LND_NCPL = ICE_NCPL >= ROF_NCPL >= OCN_NCPL"
#
# OCN_NCPL == 4 sets the ocean coupling time to 6 hours.
# All related namelist settings are based on this value.
# OCN_NCPL is # coupling intervals per NCPL_BASE_PERIOD (default 'day')
# Since OCN_NCPL and GLC_NCPL default to the same value, I am
# keeping them the same.

# ==============================================================================
# Update source files.
#    Ideally, using DART would not require any modifications to the model source.
#    Until then, this script accesses sourcemods from a hardwired location.
#    If you have additional sourcemods, they will need to be merged into any DART
#    mods and put in the SourceMods subdirectory found in the 'caseroot' directory.
# ==============================================================================


# ==============================================================================
# Modify namelist templates for each instance.
#
# In a hybrid run with CONTINUE_RUN = FALSE (i.e. just starting up):
#
# CAM has been forced to read initial files - specified by namelist var:ncdata
# POP reads from pointer files
# CICE reads from namelist variable 'ice_ic'
# CLM builds its own 'finidat' value from the REFCASE variables.
# RTM reads from namelist variable 'finidat_rtm', but rtm.buildnml.csh also is buggy.
#
# All of these must later on be staged with these same filenames.
# OR - all these namelists can be changed to match whatever has been staged.
# MAKE SURE THE STAGING SECTION OF THIS SCRIPT MATCHES THESE VALUES.
# ==============================================================================

   # ===========================================================================
   # LAND Namelist
   # With a RUN_TYPE=hybrid the finidat is automatically specified
   # using the REFCASE/REFDATE/REFTOD information. i.e.
   # finidat = ${stagedir}/${refcase}.clm2${inst_string}.r.${reftimestamp}.nc
   #
   # This is the time to consider how DART and CESM will interact.  If you intend
   # on assimilating flux tower observations (nominally at 30min intervals),
   # then it is required to create a .h1. file with the instantaneous flux
   # variables every 30 minutes. Despite being in a namelist, these values
   # HAVE NO EFFECT once CONTINUE_RUN = TRUE so now is the time to set these.
   # Check the value of h1nsteps considering STOP_N.
   # See page 65 of:
   # http://www.cesm.ucar.edu/models/cesm1.2/clm/models/lnd/clm/doc/UsersGuide/clm_ug.pdf
   # equivalently, Example 1-4 in:
   # http://www.cesm.ucar.edu/models/cesm1.2/clm/models/lnd/clm/doc/UsersGuide/x1867.html
   #
   # DART's forward observation operators for these fluxes just reads them
   # from the .h1. file rather than trying to create them from the subset of
   # CLM variables that are available in the DART state vector.
   #
   # Making a (compact) .h0. file is a good idea, since the clm restart files
   # do not have all the metadata required to reconstruct a gridded field.
   #
   # For a HOP TEST ... hist_empty_htapes = .false.
   # For a HOP TEST ... use a default hist_fincl1
   #
   # Every 6 hours
   # echo "hist_mfilt  = 1"              >> ${fname}
   # echo "hist_nhtfrq = -$stop_n"       >> ${fname}
   # Every month
   # echo "hist_mfilt  = 1"              >> ${fname}
   # echo "hist_nhtfrq = 0"              >> ${fname}

   # ===========================================================================
   # POP Namelist
   # init_ts_suboption = 'data_assim'   for non bit-for-bit restarting (assimilation mode)
   # init_ts_suboption = 'rest'         --> default behavior
   #
   # README:
   # Configuring the contents of the history files for POP is best explained in
   # the section marked "POP2: TAVG Settings" in the cesm1_2_1 pop2 namelist documentation
   # http://www.cesm.ucar.edu/models/cesm1.2/cesm/doc/modelnl/nl_pop2.html
   #
   # and the CESM-specific documentation for the tavg output variables in the pop2
   # online documentation:
   # http://www.cesm.ucar.edu/models/cesm1.2/pop2/doc/users/node78.html
   #
   # In CESM1_2_1 keep the values for tavg_file_freq_opt and tavg_freq_opt identical.
   # pop2/trunk_tags/cesm_pop_1_1_20130412  explains the issue.
   #
   # DEFAULT values for these are:
   # tavg_file_freq_opt = 'nmonth' 'nmonth' 'once'
   # tavg_freq_opt      = 'nmonth' 'nday'   'once'
   # The  first entry indicates we get a monthly average once a month.
   # The second entry indicates we get a monthly average as it is being created.
   # The  third entry indicates  we get a daily timeslice
   #
   # Default copies of SourceMods/src.pop2/ocn.*.tavg.csh files are provided in the
   # DART_SourceMods_cesm1_2_1.tar bundle.
