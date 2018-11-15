prm <- list()
prm$method  <- 'pIKS'                        # DA method ['ETKF','pIKS','pMKS','oloop'] : 'p' or 'parameter-space' refers to FDS schemes: pIKS==FDS-IKS, pMKS==FDS-MKS
prm$maxiter <- 3                             # maximum number of iterations for iterated methods
prm$ifrac   <- 1                             # this fraction [1 <= ifrac <= maxiter]
prm$rfactor <- 1                             # scalar multiplier (scaling) for R (observation error covariance) [rfactor > 1 for multistep filters]
prm$loc_boo      <- FALSE                    # whether to conduct localization [apply to all elements in the augmented state vector]. Adaptations are in 'ana' list
prm$loc_method   <- 'LA'                     # localization method. 'CF' (covariance filtering), or 'LA' (local analysis)
prm$loc_function <- 'Gaspari_Cohn'           # tag for the localisation function [calc_loccoeffs()]
prm$loc_distype  <- 'earth'                  # '2D' for Euclidean, or 'SG' for along network distances [apply to all elements in the augmented state vector]
prm$rotate       <- FALSE                    # 0 = do not rotate. 1 = rotate. The code has to prepared to set prm.rotate each some assimilation steps
prm$rotate_ampl  <- 1                        # always 1. Code not prepared for other values
prm$sdfac        <- 0.1                      # OPT, used only for parameter-space methods. Scaler of standard deviations for parameter perturbation.
