readRTM <- function(DIN_LOC_ROOT) {
  # read RTM components

  arg <- ls()

  # rof_in: &rtm_inparm      [$CASEROOT/Builconf/rtmconf]                                                     By default these 3 are set by CESM scripts based on ocean grid   
  frivinp_rtm <- file.path(DIN_LOC_ROOT,'lnd','clm2','rtmdata','rdirc_0.5x0.5_simyr2000_slpmxvl_c120717.nc') # TRM: Control settings:: full pathname of input datafile for RTM
  ice_runoff  <- TRUE                          # TRM: Control settings:: If TRUE (default), river runoff will be split up into liquid and ice streams,
  rtmhist_nhtfrq <- c(0,-24,-24)               # monthly, daily, daily
  rtmhist_mfilt  <- c(12,30,30)

  lnames <- ls()
  lnames <- lnames[!(lnames %in% c('arg',arg))] 
  lstgen <- paste(lnames,'=',lnames,sep='',collapse=', ')
  lstgen <- paste('x <- list(',lstgen,')',sep='')
  eval(parse(text=lstgen))
  return(x)
}

M$RTM <- readRTM(envrun$DIN_LOC_ROOT)
rm(readRTM)
