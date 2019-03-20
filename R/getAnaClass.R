getAnaClass <- function() {
 # return a structure with input to requested analysis for a variable KIND
 ans <- list()
 ans$u      <- TRUE           # whether this item is updated by the analysis
 ans$ll     <- 0.0            # 0.0 for global filtering
 ans$infac  <- 1.0            # 1.0 for no inflation <-> 0.0 for maximum inflation [recovery of background covariance]
 ans$min    <- -Inf           # no lower bound: used in transformation (anamorphosis) and post-asimilation checks
 ans$max    <- Inf            # no upper bound: ditto
 ans$pos    <- c(NA,NA)       # [NA,NA] => no localisation
 ans$trf    <- 'GA'           # 'none' for no transformation | 'autoGA' triggers Gaussian Anamorphosis based on Wilk-Shapiro test | 'GA' for Gaussian Anamorphosis
 return(ans)
}
