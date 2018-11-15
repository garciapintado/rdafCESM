readCICE <- function(DIN_LOC_ROOT) {
  # read POP2 components
  arg <- ls()

  # ice_in: &setup_nml        [$CASEROOT/Builconf/ciceconf]
 
  ice_ic   <- 'default'       # setup settings:: Method of ice cover initialization. 'default'=>latitude and sst dependent
  # ice_in: &grid_nml
  #kmt_file <- file.path(DIN_LOC_ROOT,'ocn','pop','gx1v6','grid','topography_20090204.ieeei4') # grid settings:: input file for CICE grid info
  # ice_in: &ice_nml
  albicei <- 0.3              # | albedos:: infrared ice albedo for h > ahmax 
  albicev <- 0.68             # | albedos:: visible ice albedo for h > ahmax 
				       
  lnames <- ls()
  lnames <- lnames[!(lnames %in% c('arg',arg))] 
  lstgen <- paste(lnames,'=',lnames,sep='',collapse=', ')
  lstgen <- paste('x <- list(',lstgen,')',sep='')
  eval(parse(text=lstgen))
  return(x)
}

M$CICE <- readCICE(envrun$DIN_LOC_ROOT)
rm(readCICE)

