#!/R
# definition of uncertainty for global model parameters

# here just CESM parameters subjected to initial perturbation 
# all parameters here need to be included in M$... and in the ana structure
# elements in p$ with MC[,'flag'] == TRUE will overwrite corresponding values in M$

p <- list() # global parameters (min,max) || (mean,sd)

  # CAM: Physics - Cloud fraction
  p$CAM$cldfrc_rhminh   <- c(0.75,   0.05)       # [mu,sd] CAM4:high stable cloud critical relative humidity [truth] in CAM4 was 0.80 -  range as Jackson_al2008
  p$CAM$cldfrc_rhminl   <- c(0.88,   0.05)       # [mu,sd] CAM4:low stable cloud critical relative humidity  [truth] in CAM4 was 0.91  -  range as Jackson_al2008
  # CAM: Physics - Moist convection and microphysics
  p$CAM$zmconv_c0_lnd   <- c(0.0202, 0.012)      # [mu,sd] as Yang_al2013 for CAM5 | truth 0.0035 | Deep convective precipitation efficiency over land  [m^{-1}]
  p$CAM$zmconv_c0_ocn   <- c(0.0202, 0.012)      # [mu,sd] as Yang_al2013 for CAM5 | truth 0.0035 | Deep convective precipitation efficiency over ocean [m^{-1}] 
  p$CAM$zmconv_ke       <- c(5.0E-06,2.2E-06)    # [mu,sd] as Yang_al2013 for CAM5 | truth 1.0E-06| Evaporation efficiency of precipitation [(kg m^{-2} s^{-1})^{-1/2}s^{-1}] 
  # CAM: Species - Greenhouse Gases - Prescribed (CAM version)
  p$CAM$ch4vmr          <- c(800.00, 4.00)*ppb # [mu,sd] CAM: CH4 volume mixing ratio. Invariant surface value of CH4 if no time varying values are specified
  p$CAM$co2vmr          <- c(300.00, 3.00)*ppm # [mu,sd] CAM: CO2 volume mixing ratio. Invariant surface value of CO2 if no time varying values are specified

  # CICE: Albedos
  p$CICE$albicei        <- c(0.30, 0.02)           # [mu,sd] infrared ice albedo (CCSM3)
  p$CICE$albicev        <- c(0.68, 0.02)           # [mu,sd] visible ice albedo (CCSM3)

  # POP2:KPP - vertical diffusion
  p$POP2$bckgrnd_vdc1   <- c(0.19, 0.02)           # [mu,sd] [cm^2s^{-1}] KPP mixing:: base background vertical diffusivity [Menemelis estimates N(0.151,sd=0.001) ]
  p$POP2$bckgrnd_vdc2   <- c(0.00, 0.00)           # [mu,sd] [cm^2s^{-1}] KPP mixing:: variation in background vertical diffusivity. 0 implies constant diffusivity and vis 
  # POP2:Gent-McWilliams horizontal mixing namelist [&hmix_gm_nml] 
  p$POP2$hmix_gm_nml.ah <- c(4.20E07,2.0E06)       # [mu,sd] [cm^2s^{-1}] - biased 1sd deviation from truth :: diffusion coeff for Redi
  p$POP2$ah_bolus       <- c(4.20E07,2.0E06)       # [mu,sd] [cm^2s^{-1}] - biased 1sd deviation from truth :: difussion coeff for bolus

  # POP2 - freshwater hosing via the specific IMAU Sourcemods
  p$POP2$imau_gis      <- c(0.0,0.005)    # [mu,sd] [Sv] gis==GreenlandIceSheet (observation 0.03808 - CESM 0.0306 ) - thus we need a new truth with +0.0075 as hosing


  pls <- p          
  p   <- unlist(p, recursive=FALSE)

  MC <- data.frame(flag=rep(TRUE,length(p)), dis='rnorm', min=-Inf, max=Inf,
                    row.names=names(p), ispar=TRUE, stringsAsFactors=FALSE)

  # subset of parameters to overwrite default values and used as control variables
  MC['CICE.albicei',     'flag'] <- FALSE         #
  MC['CICE.albicev',     'flag'] <- FALSE         #   
  MC['POP2.bckgrnd_vdc2','flag'] <- FALSE         # always set to 0
  MC['POP2.ah_bolus',    'flag'] <- FALSE         # constrained later to equal POP2.hmix_gm_nml.ah   

  # add constraints [e.g. bounds for normally distributed parameters]
  # these are for the initial generation. In the analysis step [transformations as anamorphosis and updated values]
  # bounds are specified in the 'ana' analisis list defined in readRF.R, which inherit from this within cesm_setup  
  MC['CAM.cldfrc_rhminh',  c('min','max')] <- c(0.50,0.95)
  MC['CAM.cldfrc_rhminl',  c('min','max')] <- c(0.65,0.97)
  MC['CAM.zmconv_c0_lnd',  c('min','max')] <- c(0.001,0.05)
  MC['CAM.zmconv_c0_ocn',  c('min','max')] <- c(0.001,0.05)
  MC['CAM.zmconv_ke',      c('min','max')] <- c(0.5E-06,10E-06)
  MC['CICE.albicei',       c('min','max')] <- c(0.20,0.80)
  MC['CICE.albicev',       c('min','max')] <- c(0.50,0.90)
  MC['POP2.bckgrnd_vdc1',  c('min','max')] <- c(0.10,0.30)
  MC['POP2.hmix_gm_nml.ah',c('min','max')] <- c(1.E07,9.E07)
  MC['POP2.ah_bolus',      c('min','max')] <- c(1.E07,9.E07)

  # matrix of constraints [one contraint per row]
  # equality & inequality contraints - with this, the 'flag' for randomising POP2.ah_bolus can be set to FALSE [see above]
  MCcons <- matrix(c('POP2.ah_bolus','=','POP2.hmix_gm_nml.ah' ) , 1, 3)

  #MC$b0 <- NA   # for ilR > 1 : use above 'b' as initial values within a loop and the following as background

  #MC['CAM.cldfrc_rhminh',  'b0'] <- 0.75
  #MC['CAM.cldfrc_rhminl',  'b0'] <- 0.88
  #MC['CAM.zmconv_c0_lnd',  'b0'] <- 0.0202
  #MC['CAM.zmconv_c0_ocn',  'b0'] <- 0.0202
  #MC['CAM.zmconv_ke',      'b0'] <- 5.0E-06
  #MC['CAM.ch4vmr',         'b0'] <- 800.00*ppb
  #MC['CAM.co2vmr',         'b0'] <- 300.00*ppm
  #MC['CICE.albicei',       'b0'] <- 0.30
  #MC['CICE.albicev',       'b0'] <- 0.68
  #MC['POP2.bckgrnd_vdc1',  'b0'] <- 0.19
  #MC['POP2.bckgrnd_vdc2',  'b0'] <- 0.00 
  #MC['POP2.hmix_gm_nml.ah','b0'] <- 4.2E07
  #MC['POP2.ah_bolus',      'b0'] <- 4.2E07
  #MC['POP.imau_gis',       'b0'] <- 0.0

  CF$p      <- p
  CF$MC     <- MC
  CF$MCcons <- MCcons
			
  rm(p,MC,MCcons)       


