#!/R
#
# test: FDS-IKS with 3 loops with synthetic case for comparison with the standard 1-loop ETKF
# this is done here manually for three loops.
library(rdafCESM)

HOME     <- Sys.getenv('HOME')
MODEL    <- 'cesm1_2_2'

RES      <- 'f45_g37'
event    <- '1850_DAtest' # > preindustrial - DART sequential simulations
COMPSET  <- 'B1850CN'
nnn      <- 'pIKS'          # unique test identifier - do not confuse with m-member extension - which will be appended as a three digit code (.mmm) to nnn for each member

MAKE_CASE    <- TRUE        # TRUE just for the first call
POST_PROCESS <- FALSE       # integrate model (TRUE) or just get metadata from cesm_setup.R (FALSE)

GET_ANALYSIS <- TRUE                    # CESM_DA:: if TRUE, conduct actual analysis [i.e. call analyseUG()]
SAVE_NETCDF  <- TRUE                    # CESM_DA:: if TRUE, stored in netcdf files - just .rds R format files saved otherwise 
MEMORY_SAVE  <- TRUE                    # analyse single layers of individual variables at a time

analysis_scn <- 'a1'       # synthetic data, no data transformation - no perturbation of observations

cwd <- getwd()
dsninp      <- file.path(HOME,MODEL,'data',RES,event,COMPSET,nnn)                              # HOME - data main input parent folder
setwd(dsninp)

# variables to be updated by the assimilation
# Note: if assimilation is to get parameter estimates there only need to update one variable
# as model parameters are always equally estimated by state augmentation for each variable 
#POP2vnames  <- c('FW','HMXL','IFRAC','PREC_F','SALT','TAUX','TAUY','TEMP','UVEL','VVEL','WVEL')               # variables to be analysed
POP2vnamesU <- 'TEMP'                                                         # variables to be analysed - one is enough for FDS schemes

POP.zlrange <- c(1,2)                          # range of vertical layers [from top to bottom] to be analyzed for POP2 (>=2)
tavgs       <- 'ann'                           # \in  c('JFM','JAS','ann')

tlimyStr    <- c("1890-01-01 00:00:00" ,"1910-01-01 00:00:00")                   # observation timestamp:: 20 years of simulation for average statistics

ilR <- 1                                                                              # Start from 0 

# substitute the following path by the path to the script 'cesm_setup.R' in the installation folder of this package
source(file.path(HOME,MODEL,'scripts','R','DA','cesm_setup.R'))                       # simulation general input
