getEnvmachpes <- function() { # env_mach_pes.xml parameters

  arg <- list()

  NTASKS_ATM <- 144     
  NTHRDS_ATM <- 1
  ROOTPE_ATM <- 0      # MPI:0-143 procs:1-144

  NTASKS_CPL <- 132
  NTHRDS_CPL <- 1
  ROOTPE_CPL <- 0      # MPI:0-132 procs:1-132

  NTASKS_GLC <- 12
  NTHRDS_GLC <- 1
  ROOTPE_GLC <- 132     # MPI:132-143   procs:133-144

  NTASKS_ICE <- 96
  NTHRDS_ICE <-  1
  ROOTPE_ICE <-  0     # MPI:0-95  procs:1-96

  NTASKS_LND <- 24
  NTHRDS_LND <-  1
  ROOTPE_LND <- 96     # MPI:96-119  procs:97-120

  NTASKS_ROF <- 12
  NTHRDS_ROF <-  1
  ROOTPE_ROF <- 120    # MPI:120-131   procs:121-132

  NTASKS_WAV <- 12
  NTHRDS_WAV <-  1
  ROOTPE_WAV <- 132     # MPI:132-143   procs:133-144

  NTASKS_OCN <- 144
  NTHRDS_OCN <- 1
  ROOTPE_OCN <- 0      # MPI:0-11 procs:1-144

  #PES$TOTALPES   <- 144  # automatic - do not edit

  lnames <- ls()
  lnames <- lnames[!(lnames %in% c('arg',arg))] 
  lstgen <- paste(lnames,'=',lnames,sep='',collapse=', ')
  lstgen <- paste('x <- list(',lstgen,')',sep='')
  eval(parse(text=lstgen))

  return(x)
}

envmachpes <- getEnvmachpes(); rm(getEnvmachpes)
