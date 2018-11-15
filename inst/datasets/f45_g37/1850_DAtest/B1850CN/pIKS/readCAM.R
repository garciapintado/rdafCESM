readCAM <- function(DIN_LOC_ROOT) {

  # avgflag_pertape <- 'A'         # 'A' for average is already default
  # values in header_mc with FLAG=TRUE overwrite these  

  # pre-industrial :: read CAM components
  arg <- ls()

  # atm_in: &cam_inparm        [$CASEROOT/Builconf/camconf]
  #bnd_topo <- file.path(DIN_LOC_ROOT,'atm','cam','topo','USGS-gtopo30_1.9x2.5_remap_c050602.nc') # CAM: Boundary Datasets
  bndtvghg     <- NULL       # CAM: Species - GHG - Prescribed (CAM version)
  #ghg_yearstart_data  <- 0   # CAM: Species - GHG - Prescribed (CAM version)
  #ghg_yearstart_model <- 0   # CAM: Species - GHG - Prescribed (CAM version)

  # atm_in: &cldfrc_nl
  cldfrc_rhminh <- 0.80       # default value in B1850CN - truth here 
  cldfrc_rhminl <- 0.91       # default value in B1850CN - truth here

  # atm_in: &chem_surfvals_nl
  ch4vmr   <- 791.6*ppb       # CAM: Species - GHG - Prescribed (CAM version) - truth here
  co2vmr   <- 284.7*ppm       # CAM: Species - GHG - Prescribed (CAM version) - truth here
  f11vmr   <- 12.48e-12       # CAM: Species - GHG - Prescribed (CAM version)
  f12vmr   <- 0.0             # CAM: Species - GHG - Prescribed (CAM version)
  n2ovmr   <- 275.68*ppb      # CAM: Species - GHG - Prescribed (CAM version)

  # atm_in: &zmconv_nl
  zmconv_c0_lnd <- 0.0035     # default value in B1850CN - truth here
  zmconv_c0_ocn <- 0.0035     # default value in B1850CN - truth here
  zmconv_ke     <- 1.0E-06    # default value in B1850CN - truth here

  # atm_in: &cam_inparm
  nhtfrq   <- c(0,-24,-24,-24,-24)      # CAM: History and Initial Conditions Output - (avgflag_pertape already defaults to A for average)
                                        #      nhtfrq(1) = 0 => the file will be a montly average
                                        #      nhtfrq(i) < 0 refers to frequency specified as number of hours
                                        # In paleoclimate our interest  is firstly on h0, as it is the only one giving monthly data
  mfilt    <- c(12*10,365,30,30,30)     # the 1st one up to 10 years - monthly time steps
                                        # Thus we'll do batchs of 10-years - for longer insterval change mfilt[1] accordingly
  lnames <- ls()
  lnames <- lnames[!(lnames %in% c('arg',arg))] 
  lstgen <- paste(lnames,'=',lnames,sep='',collapse=', ')
  lstgen <- paste('x <- list(',lstgen,')',sep='')
  eval(parse(text=lstgen))
  return(x)
}

M$CAM <- readCAM(envrun$DIN_LOC_ROOT)
rm(readCAM)
