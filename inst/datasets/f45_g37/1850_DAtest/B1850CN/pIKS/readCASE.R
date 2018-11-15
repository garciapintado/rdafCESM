# CASE description
# this is to conduct a simple test of Green's function and LETKF. The test creates the truth and also creates an m=50 ensemble with a subset of random parameters.
# the random parameters are such that the 1st member is the mean, the np next members are to create single-parameter perturbations, such that Green's function
# calibration can be directly applied with these members. The following m-(np+1) members are such that the required Q covariance of the parameter space is approximately 
# described by the complete ensemble. This should allow a) a simultaneous analysis of a LETKF with all the ensemble, and b) an analysis of a Green-LETKF (to be described
# y Garcia-Pintado et al. 2016)

# the 0 members contains a synthetic 'truth' to test the methods

COMPSET          <- 'B1850CN'
CCSM_LCOMPSET    <- '1850_CAM4_CLM40%CN_CICE_POP2_RTM_SGLC_SWAV'
#RES              <- 'f09_g16' # '0.9x1.25_gx1v6'                                  # DART example
     # atm: Finite volume regular dlat x dlon | land:        
#RES              <- 'f19_g16'             # Pepijn's experiments
#RES_longname     <- 'a%1.9x2.5_l%1.9x2.5_oi%gx1v6_r%r05_m%gx1v6_g%null_w%null' # atm: ~2 degree reg FV | lnd: as atm | ocean/sea ice: displaced pole ~1º version 6 | river: 1/2º | land mask: as ocean/sea ice | land-ice: null | wave - irrelevant in CESM 1.2


RES             <- 'f45_g37'  # Andre's current experiments
RES_longname    <- 'a%4x5_l%4x5_oi%gx3v7_r%r05_m%gx3v7_g%null_w%null'        # atm: ~4 degree reg FV | lnd: as atm | ocean/sea ice: displaced pole ~3º version 7 | river: 1/2º | land mask: as ocean/sea ice | land-ice: null | wave - irrelevant in CESM 1.2
 
event    <- '1850_DAtest' # > preindustrial - DART sequential simulations
if (!exists('nnn'))
  stop('readCASE:: ---ERR001: nnn does not exits---')
# nnn      <- '000'       # [given in call_CESM_DA.R] unique test identifier - do not confuse with m-member extension - which will be appended as a three digit code (.mmm) to nnn for each member
				       

