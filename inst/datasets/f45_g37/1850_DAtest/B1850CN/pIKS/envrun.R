getEnvrun <- function(WORK2,CASE) { # env_run.xml parameters
  # comment taken from 'https://bb.cgd.ucar.edu/node/1001997'
  # A hybrid run is set up like an initial run for both the CAM and CLM components. So both those components need to have initial files specified, 
  # while the pop and ice components will start from restart files. That said, since we [i.e. UCAR] converted the restart files to the netcdf format, CAM is now
  # able to use a restart file in place of an initial file. But whether and initial or restart field is used, that file still needs to be specified
  # using CAM's ncdata namelist variable (full pathname of initial atmospheric state dataset ---NetCDF format---).
  #
  # An initial run for CAM only requires the file specified by ncdata (which can be either an initial ".i" file or a restart ".r" file. None of the other files generated from
  # a previous run are needed for a CAM initial run.
  #
  # DA strategy: for paleoclimate we assume the minimum time footprint of an observation is one month. Thus, let say we
  # have a monthly averaged observation for March 2016. Then we set timenex to 2016-03-01, and set
  # model restart file output to timenex, and model stop time to timenex+1 month. So we have the background
  # information to get the model-observations (h0 history files) averaged for the last month (March 2016), and we can get the innovations 
  # (& their statistics). However the state is estimated for timenex (which is 2016-03-01-00000), which is the restart time
  # of the following integration.

  arg <- ls()
				       
  BRNCH_RETAIN_CASENAME <- FALSE                                      # allow same branch casename as reference casename
  CONTINUE_RUN  <- FALSE                                              # TRUE implies a continuation run [likely needed to be set to TRUE between assimilation steps]
  DIN_LOC_ROOT  <- file.path(WORK2,'cesm1_2_2','inputdata')
  DIN_LOC_ROOT_CLMFORC <- DIN_LOC_ROOT                                # general ccsm inputdata directory for CLM datm forcing. Just used for I compsets and only dor datm forcing data
  DOUT_L_MS     <- FALSE                                              # turn on/off long term archiving of output data
  DOUT_L_MSROOT <- file.path(WORK2,'cesm1_2_2','long_archive',CASE)   # $DOUT_L_MSROOT
  DOUT_S        <- TRUE                                               # turn on/off short term archiving of output data
  DOUT_S_ROOT   <- file.path(WORK2,'cesm1_2_2','archive',CASE)        # $DOUT_S_ROOT
  GET_REFCASE   <- FALSE                                              # flag or automatically prestaging the refcase restart dataset - set to FALSE & do manually after the 1st asimilation step
  INFO_DBUG     <- 1                                                  # level of debug output, 0=minimum, 1=normal, 2=more, 3=too much, valid values: 0,1,2,3 (integer)
  PIO_TYPENAME  <- 'pnetcdf'
  RESUBMIT      <- CF$RESUBMIT                                        # > 0 for case to automatically resubmit [continue runs]. Init to 0
  #REST_DATE     <- gsub('-','',datePOSIXct(CF$timenex))              # drv_in:: &seq_timemgr_inparm:restart_option
  REST_DATE     <- -999
  REST_N        <- CF$REST_N                                          # elapsed_months(CF$timenow,CF$timenex)
  REST_OPTION   <- 'nmonths'
  RUNDIR        <-  file.path(WORK2,'cesm1_2_2','SCRATCH',CASE,'run') # in env_run.xml
  #RUN_REFCASE   <- 'b40.1850.track1.2deg.003'                        # Reference case for hybrid or branch runs (char*256) " -->    [this case copied from B1850CN example run]
  #RUN_REFDATE   <- '0501-01-01'                                      # Reference date for hybrid or branch runs (yyyy-mm-dd) (char*10)
  RUN_REFCASE   <- 'b.e12.B1850CN.f45_g37.001'                        # Reference case for hybrid or branch runs (char*256) " -->    [Andre's simulations]
  RUN_REFDATE   <- '1550-01-01'                                       # Reference date for hybrid or branch runs (yyyy-mm-dd) (char*10)
  RUN_REFTOD    <- '00000'
  RUN_STARTDATE <- datePOSIXct(CF$timenow+CF$tdiff)                   # Run start date (yyyy-mm-dd). Only used for startup or hybrid runs. Generally = RUN_REFDATE, but can be different to stat this run as if it was a different time
  RUN_TYPE      <- 'hybrid'                                           # TRUE for restart run
  START_TOD     <- todPOSIXct(CF$timenow+CF$tdiff)
  STOP_DATE     <- -999
  if (RESUBMIT == 0) {
    STOP_N      <- elapsed_months(CF$timenow,CF$timenex)+1             # add always one month xtratime to gather last-month statistics
  } else
    STOP_N      <- CF$dtREST 
  STOP_OPTION   <- 'nmonths'
  #CLM_CO2_TYPE  <- 'diagnostic'                                      # clm co2 type, valid values: constant,diagnostic,prognostic (char)


  lnames <- ls()
  lnames <- lnames[!(lnames %in% c('arg',arg))] 
  lstgen <- paste(lnames,'=',lnames,sep='',collapse=', ')
  lstgen <- paste('x <- list(',lstgen,')',sep='')
  eval(parse(text=lstgen))
  return(x)
}

envrun <- getEnvrun(WORK2,CASE); rm(getEnvrun)
