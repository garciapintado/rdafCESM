#!/R
readPOP2 <- function(DIN_LOC_ROOT, RUNDIR, CASEROOT) {
  # read POP2 components
  arg <- ls()

  # pop2_in: &grid_nml      [$CASEROOT/Builconf/pop2conf]                                                     By default these 3 are set by CESM scripts based on ocean grid   
  #region_info_file <- file.path(RUNDIR,'gx1v6_region_ids')                                             # POP2: Grid settings:: Input file with region identification info.
  #if (length(region_info_file) > 1)
  #  region_info_file <- as.list(region_info_file)  # ENSEMBLE input has to converted into list to avoid clash with multivariate input
  #region_mask_file <- file.path(DIN_LOC_ROOT,'ocn','pop','gx1v6','grid','region_mask_20090205.ieeei4') # POP2: Grid settings:: Input file for reading region mask.
  #topography_file  <- file.path(DIN_LOC_ROOT,'ocn','pop','gx1v6','grid','topography_20090204.ieeei4')  # POP2: Grid settings:: Input file for reading topography.

  # pop2_in: &tidal_nml      [$CASEROOT/Builconf/pop2conf]                                                     By default these 3 are set by CESM scripts based on ocean grid   
  #POP2$ltidal_mixing          <- TRUE # POP2: Tidal:: flag for tidal mixing [default TRUE]

  # pop2_in: &diagnostics_nml      [$CASEROOT/Builconf/pop2conf]                                                     By default these 3 are set by CESM scripts based on ocean grid   
  ldiag_velocity         <- TRUE # POP2: Diagnostics:: flag to compute velocity diagnostics [default TRUE]

  # pop2_in: &hmix_nml
  # hmix_momentum_choice = 'anis' # default - method for horizontal miximg of momentum
  # hmix_tracer_choice = 'gent'   # default (Gent-McWilliams) - method for horizontal mixing of tracers

  # pop2_in: &hmix_gm_nml - Gent-Williams isopycnic tracer diffusion
  # see: http://www.cesm.ucar.edu/models/cesm1.0/pop2/doc/users/node49.html
  hmix_gm_nml.ah  <- 4.0E07 # ah appears in several namelists, so it need to be written as ah&hmix_gm_nml in user_nl_pop2
  ah_bolus        <- 4.0E07

  # pop2_in: &vmix_kpp_nml      [$CASEROOT/Builconf/pop2conf]                                                     By default these 3 are set by CESM scripts based on ocean grid   
  bckgrnd_vdc1           <- 0.16   # POP2: KPP:: background diffusivity (Ledwell) [default 0.16]
  bckgrnd_vdc2           <- 0.0    # POP2: KPP:: variation in diffusivity [default 0.0]
  #bckgrnd_vdc_eq         <- 0.01
  #lhoriz_varying_bckgrnd <- TRUE  # POP2: KPP:: flag for allow for horizontally-varying background (need bckgrnd_vdc2=0.0 [default TRUE]

  # pop2_in: &overflows_nml      [$CASEROOT/Builconf/pop2conf]                                                     By default these 3 are set by CESM scripts based on ocean grid   
  overflows_on           <- TRUE # POP2: overflows:: flag for using parameterized flows [default TRUE for displaced pole grids, FALSE for tripole grids.] 
  overflows_interactive  <- TRUE # POP2: overflows:: flag for using interactive flows [default TRUE for displaced pole grids, FALSE for tripole grids.] 
  #overflows_infile <- file.path(RUNDIR,'gx1v6_overflow') # POP2: overflows:: file with overflow information [default set by CESM scripts based on ocean grids]
  #if (length(overflows_infile) > 1)
  #  overflows_infile <- as.list(overflows_infile)  # ENSEMBLE input has to converted into list to avoid clash with multivariate input

  # pop2_in: &history_nml      [$CASEROOT/Builconf/pop2conf]     - this refers to instantaneous output                                                By default these 3 are set by CESM scripts based on ocean grid   
  history_freq     <- 1          # POP2: History output:: frequency of writing history files [default 1]
  history_freq_opt <- 'never'   # POP2: History output:: units of history_freq (frequency of writing history files) [default: 'never'] - monthly output allows for seasonal analysis [e.g. glacial summer vs glacial winter]

  # pop2_in: &tavg_nml - time averaged output
  n_tavg_streams <- 3
  tavg_freq      <- c(1, 1, 1)
  tavg_freq_opt  <- c('nmonth','nday','once')
  tavg_file_freq <- c(1, 1, 1)
  tavg_file_freq_opt <- c('nmonth','nmonth','once')

  # exception into user_nl_pop - written into user_nl_pop by a specific IMAU function
  # todo - interact with MAKE to select routines with/without hosing according to whether hosing is conducted
  IMAU <- list()                                                         # IMAU is freswater forcing for hosing experiments conducted by special F90 MODULES forcing_coupled & forcing_tools
  IMAU$imau_filename          <- file.path(CASEROOT,'FW_GIS.nc') # later generated based on weight and total value
  IMAU$imau_filename_prev     <- IMAU$imau_filename
  IMAU$imau_filename_next     <- IMAU$imau_filename 
  IMAU$imau_data_type         <- 'monthly'      #  monthly, annual, none
  IMAU$imau_blanking          <- FALSE
  IMAU$imau_filename_blanking <- ''             #  empty string, as imau_blanking==FALSE
  imau_filename_msk <- file.path(HOME,'docs','palmod','data','IMAU_forcing','FWF_GIS_mask.nc')    # class==1 for Greenland coast
  imau_filename_wei <- file.path(HOME,'docs','palmod','data','IMAU_forcing','FWF_GIS_weights.nc') # sum(wei)==1 for Greenland coast
  imau_gis          <- 0.0075 # for a simulated truth including some hosing


  lnames <- ls()
  lnames <- lnames[!(lnames %in% c('arg',arg))] 
  lstgen <- paste(lnames,'=',lnames,sep='',collapse=', ')
  lstgen <- paste('x <- list(',lstgen,')',sep='')
  eval(parse(text=lstgen))
  return(x)
}

M$POP2 <- readPOP2(envrun$DIN_LOC_ROOT, envrun$RUNDIR, envcase$CASEROOT)
rm(readPOP2)

# we setup a 0.0 hosing by set to 0.0 the Fresh Water Forcing input grid  everywhere
# IMAU input is embedded into user_nl_pop2 with a special syntax appending
# '&forcing_imau_nml' to each IMAU variable . For example:
# imau_data_type&forcing_imau_nml       = 'monthly'
POP2exceptions <- c('IMAU','imau_filename_msk','imau_filename_wei','imau_gis')

