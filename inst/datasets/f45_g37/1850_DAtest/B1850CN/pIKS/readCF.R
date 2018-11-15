# notes:
# I will round timestamp for paleoclimate analysis to months. All model IO will be in months as a basic timestep
# Thus corresponding POSIXt will always end in "01-01 00:00:00" and will be UTC time
# this is IKP scenario for paper on methodological KS schemes.
CF <- list()
CF$path <- list()
CF$path$data  <- dsndat
CF$region     <- region
CF$event      <- event
CF$scn        <- scn
CF$model      <- MODEL
CF$m          <- 10                     # ensemble members - match number on control variables for local sensitivity filters
CF$useGA      <- TRUE                   # use [univariate, marginal, ensemble-based] Gaussian anamorphosis for gauDA[c('HE','y'] & MCgpar
CF$useGAy     <- FALSE                  # cesmDA: whether to conduct the (~x,~y*) transformation (Amezcua 2012 notation): i.e. use x ensebmle to marginally transform y

CF$MThroughP  <-  24.0 * 365            # [y/d] -> [d/d] == [h/h] Model Throughput [-] taken from a previous run for this resolution & compset and PES - used later to estimate walltime to request computing resources

CF$mpi        <- FALSE                  # mpi for R-DA

CF$mcmode     <- 1                      # 1 for MonteCarlo simulation | 0 for parameter inheritance
CF$inh        <- list()                 # non-empty list for mcmode == 0
CF$makeTruth  <- FALSE                  # integrate a 'true' member instead of an ensemble. TODO: remove
#CF$truthext  <- '000'                  # truth with no hosing                           | label to append to CASE_0 to identify a 'true' run
CF$truthext   <- 'h01'                  # truth with hosing at GIS ~0.0075 Sv
CF$it_restart <- 0                      # 0 for startup runs | >= 1 for restart - 1 does not build but runs from step 1 and jumps into assimilation at it=1
CF$delete_ocn_daily <- TRUE             # in case history files for ocn are produced - delete these?

# just for temporary check - hosing routines with no freshwater input - M$POP$imau_gis <- 0
#CF$staTStr   <- "1850-01-01 00:00:00"   # paste(envrun$RUN_STARTDATE,'00:00:00 GMT', sep=' ')
#CF$endTStr   <- "1851-01-01 00:00:00"   # total integration times - including DA/forecast steps
#CF$makeTruth <- TRUE                   # integrate a 'true' member instead of an ensemble
#CF$truthext  <- 'h00'                  # truth with hosing at 0.0 everywhere Sv

CF$hpc        <- list()                 # definition of resources requested to HLRN
CF$hpc$account_name <- 'hbk00059'       # project account
CF$hpc$nodes        <- 6                # ppn:24

# map time into POSIX second times for DA analyses
CF$staTStr    <- "1850-01-01 00:00:00"   # paste(envrun$RUN_STARTDATE,'00:00:00 GMT', sep=' ')
CF$endTStr    <- "1910-01-01 00:00:00"   # total integration times - including DA/forecast steps [just 60 years to compare with the ETKF previously truncated integrations]
CF$tdiff      <- 0
CF$dtREST     <- 12*10                   # [nmonths] NULL for no interruption [non continuation runs between sequential assimilation steps]
CF$REST_N     <- 12*10                   # >= (and exact multiple of) dtREST

CF$staT   <- as.POSIXct(CF$staTStr, tz='GMT')
CF$endT   <- as.POSIXct(CF$endTStr, tz='GMT')  
CF$staTM  <- CF$staT + CF$tdiff       # start from year 1000 - equivalent model time
CF$endTM  <- CF$endT + CF$tdiff

# observations: definition of observation yKIND,yTYPE,yFMT, where yFMT %in% c('g','p','s')
# y$KIND$TYPE$g -                 time-dense  [gauge-like] 
#             s -                 space-dense [satellite-like]
#             p -                 space-dense multi-time [assimilated via augmentation - fractional assimilation]

CF$y_class <- list()                      # observations: generic structure
CF$y_class$fname <- ''                    # If empty string [''], it must be set in readOb.R
CF$y_class$nto   <- Inf                   # maximum number of times - for p_gen always Inf: there is just 1 observation/location
CF$y_class$qc    <- 'none'                # QC method
CF$y_class$r     <- NA                    # scalar observation variance
CF$y_class$twin  <- Inf                   # [s] maximum assimilation window from present to past [Inf: all integration run]
CF$y_class$use   <- TRUE                  # assimilate or not
CF$y_class$w     <- FALSE                 # TRUE for weigthed observations

CF$yfnames    <- c('y_MARGOloc_KIND_ocn_sst.R') # syntethic SST at MARGO locations
CF$gauDAfname <- file.path(CASEROOT_0,'DA','gauDA_MARGOpos_KIND_ocn_sst.rds')

#CF$yfnames <- c('y_szon.R','y_forceAlbedos.R') 
#CF$yfnames <- c('y_PAGES2k_coral.R')
#CF$gauDAfname <- file.path(CASEROOT_0,'DA','gauDA_PAGES2K_coral.rds')

CF$IMAU_ON    <- TRUE # activate/deactive hosing into POP2 via IMAU FW

#y <- list()                           # empty list for no observations
#y$KIND_atm_sft$proxy1$s     <- list() # atm - surface temperature
#y$KIND_atm_sft$proxy1$s$use <- FALSE

ana_class <- list()                # generic analysis item
ana_class$u      <- TRUE           # whether this item is updated by the analysis
ana_class$ll     <- 0.0            # 0.0 for global filtering
ana_class$infac  <- 1.0            # 1.0 for no inflation <-> 0.0 for maximum inflation [recovery of background covariance]
ana_class$min    <- -Inf           # no lower bound: used in transformation (anamorphosis) and post-asimilation checks
ana_class$max    <- Inf            # no upper bound: ditto
ana_class$pos    <- c(NA,NA)       # [NA,NA] => no localisation
ana_class$trf    <- 'GA'           # 'none' for no transformation | 'autoGA' triggers Gaussian Anamorphosis based on Wilk-Shapiro test | 'GA' for Gaussian Anamorphosis
                                   # not used for local sensitivity filters (based on marginal perturbations)

# parameters to check in connection to:
# atm.   co2 forcing
#        cloud emmisivity
#        horizontal transport of sensible heat & horizontal transport of latent heat
#        precipitation rates (e.g. threshold of relative huidity to start precipitating clouds)
#        something to discrimate between low and high level clouds.
# ice.   sea   albedo
# land.  snow albedo
# ocn.   vertical (background values in KPP) and horizontal (isopycnal in current models) diffusion (Gent equation) [what is turbulence closure?]


ana <- list()  # variables to analyse 
 #ana$KIND_albicei      <- ana_class
 #ana$KIND_albicei$trf  <- 'GA'
 #ana$KIND_albicev      <- ana_class
 #ana$KIND_albicev$trf  <- 'GA'

 #ana$KIND_coral        <- ana_class
 ana$KIND_ocn_sst      <- ana_class       # gridded variable matching MARGO dataset
 
 #ana$KIND_ocn_sst$trf  <- 'GA'     
 #ana$KIND_ocn_sst$ll  <- 5000*km

 #ana$KIND_szon         <- ana_class
 #ana$KIND_szon$min     <- 0.0
 #ana$KIND_szon$trf     <- 'GA'
 
 # parameters                            model global parameters - must be synchronised with header_mc.R
 ana$CAM.cldfrc_rhminh  <- ana_class      # [min,max] CAM4:high stable cloud critical relative humidity  [truth] in CAM4 was 0.80 -  range as Jacson_al2008
 ana$CAM.cldfrc_rhminl  <- ana_class      # [min,max] CAM4:low stable cloud critical relative humidity  [truth] in CAM4 was 0.91  -  range as Jacson_al2008
 ana$CAM.ch4vmr         <- ana_class      # [mu,sd] CAM: CH4 volume mixing ratio.  Invariant surface value of CH4 if no time varying values are specified
 ana$CAM.co2vmr         <- ana_class      # [mu,sd] CAM: CO2 volumne mixing ratio. Invariant surface value of CO2 if no time varying values are specified
 ana$CAM.zmconv_c0_lnd  <- ana_class      # [min,max] as Yang_al2013 for CAM5 | truth 0.0035 | Deep convective precipitation efficiency over land  [m^{-1}]
 ana$CAM.zmconv_c0_ocn  <- ana_class      # [min,max] as Yang_al2013 for CAM5 | truth 0.0035 | Deep convective precipitation efficiency over ocean [m^{-1}] 
 ana$CAM.zmconv_ke      <- ana_class      # [min,max] as Yang_al2013 for CAM5 | truth 1.0E-06| Evaporation efficiency of precipitation [(kg m^{-2} s^{-1})^{-1/2}s^{-1}] 

 ana$CICE.albicei       <- ana_class
 ana$CICE.albicei$u     <- FALSE        
 ana$CICE.albicev       <- ana_class
 ana$CICE.albicev$u     <- FALSE

 ana$POP2.hmix_gm_nml.ah <- ana_class
 ana$POP2.ah_bolus       <- ana_class     # constrained to == POP2.hmix_gm_nml.ah
 ana$POP2.bckgrnd_vdc1 <- ana_class
 ana$POP2.bckgrnd_vdc2 <- ana_class
 ana$POP2.bckgrnd_vdc2$u <- FALSE      # constant parameterization along vertical
 ana$POP2.imau_gis     <- ana_class       # reported as POP2.freshwater_gis
 #ana$POP2.imau_gis$trf  <- 'autoGA'

 ana$CICE.albicei$min <- 0.0
 ana$CICE.albicev$min <- 0.0
 ana$CICE.albicei$max <- 1.0
 ana$CICE.albicev$max <- 1.0

 #This should be replaced by all variable names in POP2.
 # Specific analysed variables are in vector POP2vnamesU declared in the main caller
 POP2vnames <- c('FW','HMXL','IFRAC','PREC_F','SALT','TAUX','TAUY','TEMP','UVEL','VVEL','WVEL')
 
 for (vname in POP2vnames) {
  ana[[paste('POP2',vname,sep='.')]] <- ana_class
  ana[[paste('POP2',vname,sep='.')]]$trf <- 'none'
  #ana[[paste('POP2',vname,sep='.')]]$ll <- 5000*km
 }

 ana$POP2.SALT$min   <- 0
 ana$POP2.SALT$trf   <- 'GA'
 ana$POP2.PREC_F$min <- 0
 ana$POP2.PREC_F$trf <- 'GA'
 ana$POP2.HMXL$min   <- 0
 ana$POP2.HMXL$trf   <- 'GA'
 ana$POP2.IFRAC$min  <- 0
 ana$POP2.IFRAC$trf  <- 'GA'

CF$ana <- ana; rm(ana)
CF$ana_class <- ana_class; rm(ana_class)
