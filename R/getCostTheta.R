getCostTheta <- function(theta) {

  ntheta   <- length(theta$b)
  thetaNam <- names(theta$b)                                  # [ntheta]
  Pb       <- theta$Pthetab0                                  # [ntheta,ntheta]

  if (!all(thetaNam %in% rownames(theta$MCgpar)))
    stop('getCostTheta:: theta names could not be  identified in theta$MCgpar')
  MCtheta <- theta$MCgpar[thetaNam,,drop=FALSE]                                # [ntheta,m]
  m <- ncol(MCtheta)      

  isdiag <- all(Pb[lower.tri(Pb)] == 0, Pb[upper.tri(Pb)] == 0)
#browser()
  cost <- list(costB=rep(NA,m), costA=NA)   # theta$b first in vector for pKs methods
   
  for (im in 1:m) {
    dxb <- matrix(MCtheta[,im] - theta$b0)
    if (sum(dxb)==0) {
      cost$costB[im] <- 0
    } else {
       if (isdiag) {
         cost$costB[im] <- 0.5 * colSums(dxb^2/diag(Pb))
       } else {
         cost$costB[im] <- 0.5 * rowSums(crossprod(dxb,solve(Pb)) * t(dxb))
       }
    }
  }
    
  if (!is.null(theta$a)) {                       # only for the updated mean
    dxa <- matrix(theta$a - theta$b0) 
    if (isdiag) {
      cost$costA <- 0.5 * colSums(dxa^2/diag(Pb))
    } else {
      cost$costA <- 0.5 * rowSums(crossprod(dxa,solve(Pb)) * t(dxa))
    }
  }

  return(cost)
}
