readDRV <- function() {

  arg <- list()

  # drv_in: &seq_infodata_inparm        [$CASEROOT/Builconf/cplconf]
  orb_iyear       <- 1990         # Orbital Control Settings:: years AD [e.g. -19050 [AD] to represent 21000 BP for LGM]
  orb_iyear_align <- 1990         # Orbital Control Settings:: model year associated with orb_iyear when orb_mode is variable_year.  
  orb_mode        <- 'fixed_year' # Orbital Control Settings::
  # orb_iyear       <- 1990                                                                    # alternatives for transient solar forcing 
  # orb_iyear_align <- 1990
  # orb_mode        <- 'variable_year'

  lnames <- ls()
  lnames <- lnames[!(lnames %in% c('arg',arg))] 
  lstgen <- paste(lnames,'=',lnames,sep='',collapse=', ')
  lstgen <- paste('x <- list(',lstgen,')',sep='')
  eval(parse(text=lstgen))
  return(x)
}

M$DRV <- readDRV()
rm(readDRV)
