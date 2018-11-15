readCLM <- function(DIN_LOC_ROOT) {
  # read CLM components

  # hist_avgflag_pertape <- 'A'  # 'A' (average over history period) / I believe this is default anyway
  # hist_mfilt  # per tape series maximum number of time samples
  # hist_nhtfrq <- c(0,-24,-24,-24,-24,-24) # default 0:monthly

  arg <- ls()

  # lnd_in: &clm_inparm      [$CASEROOT/Builconf/clmconf]
  albice    <- c(0.80,0.55)             # CLM: Physics Settings:: Visible and Near-infrared albedo's for glacier ice
  #co2_type  <- 'diagnostic'              # CLM: Physics Settings:: ['constant','diagnostic' or 'prognostic'] 
  #co2_ppmv  <- 284.7                    # CLM: Physics Settings:: just used for co2_type==constant 
  create_crop_landunit <- FALSE         # CLM: Physics Settings:: If TRUE, separate the vegetated landunit into a crop landunit and a natural vegetation landunit
  dtime <- 1800                         # CLM: Physics Settings:: time step [s]
  #fatmlndfrc  <- file.path(DIN_LOC_ROOT,'share','domains','domain.lnd.fv1.9x2.5_gx1v6.090206.nc')        # CLM: Input Datasets:: land fraction | explicit user_nl_cl except
  #finidat     <- 'b40.1850.track1.2deg.003.clm2.r.0501-01-01-00000.nc'                                   # CLM: Input Datasets:: initial conditions file
  #fpftcon     <- file.path(DIN_LOC_ROOT,'lnd','clm2','pftdata','pft-physiology.clm40.c130424.nc')        # CLM: Input Datasets:: plant function type (PFT) constants
  #fsnowaging  <- file.path(DIN_LOC_ROOT,'lnd','clm2','snicardata','snicar_drdt_bst_fit_60_c070416.nc')   # CLM: Input Datasets:: SNICAR snow aging data
  #fsnowoptics <- file.path(DIN_LOC_ROOT,'lnd','clm2','snicardata','snicar_optics_5bnd_c090915.nc')       # CLM: Input Datasets:: SNICAR optical data
  #fsurdat     <- file.path(DIN_LOC_ROOT,'lnd','clm2','surfdata','surfdata_1.9x2.5_simyr1850_c091108.nc') # CLM: Input Datasets:: surface data file
  maxpatch_glcmec <- 0              # CLM: Physics Settings:: Number of  multiple elevation classes over glacier points.
  nsegspc     <- 20                 # CLM: Decomposition Settings:: [20 is default] number of segments per clump for decomposition
  urban_hac   <- 'ON_WASTEHEAT'     # CLM: Physics Settings:: ['OFF','ON','ON_WASTEHEAT']
  urban_traffic <- FALSE            # CLM: Physics Settings:: Currently NOT implemented
  # lnd_in: &ndepdyn_nml
  ndepmapalgo             <- 'bilinear' # CLM: input datasets:: mapping method for nitrogen deposition 
  #stream_fldfilename_ndep <- file.path(DIN_LOC_ROOT,'lnd','clm2','ndepdata','fndep_clm_hist_simyr1849-2006_1.9x2.5_c100428.nc') # CLM: input datasets:: filename of input stream for nitrogen deposition
  #stream_year_first_ndep  <- 1850  # CLM: input datasets:: First year to loop over for Nitrogen Deposition data

  lnames <- ls()
  lnames <- lnames[!(lnames %in% c('arg',arg))] 
  lstgen <- paste(lnames,'=',lnames,sep='',collapse=', ')
  lstgen <- paste('x <- list(',lstgen,')',sep='')
  eval(parse(text=lstgen))
  return(x)
}

M$CLM <- readCLM(envrun$DIN_LOC_ROOT)
rm(readCLM)

# exceptions described in user_nl_clm
CLMexceptions <- c('co2_ppmv','fatmlndfrc','dtime','fatmlndfrc','finidat','glc_grid','glc_smb','maxpath_glcmec')
