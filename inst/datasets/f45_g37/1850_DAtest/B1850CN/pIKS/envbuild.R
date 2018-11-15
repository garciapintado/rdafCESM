getEnvbuild <- function(WORK,CASE) { # env_build.xml parameters
  arg <- ls()

  # start user section
  # CALENDAR       <- 'GREGORIAN' # gives a bug when branching CICE
  CALENDAR       <- 'NO_LEAP'
  EXEROOT        <- file.path(WORK,'cesm1_2_2','SCRATCH',CASE,'bld')
  DEBUG          <- FALSE
  COMP_INTERFACE <- 'MCT'  # ['MCT','ESMF'] try ESMF interface - in prep. for F90 DA interface  
  USE_ESMF_LIB   <- FALSE  # CESM-embedded ESMF time manager
  #ESMF_LIBDIR    <- Sys.getenv('ESMF_LIBDIR')      ESMF_LIBDIR or ESMFMKFILE                                    # previously set in .bashrc
  # end user section

  lnames <- ls()
  lnames <- lnames[!(lnames %in% c('arg',arg))] 
  lstgen <- paste(lnames,'=',lnames,sep='',collapse=', ')
  lstgen <- paste('x <- list(',lstgen,')',sep='')
  eval(parse(text=lstgen))
  return(x)
  #return(env2list(ls(),arg))
}

envbuild <- getEnvbuild(WORK2,CASE); rm(getEnvbuild)
