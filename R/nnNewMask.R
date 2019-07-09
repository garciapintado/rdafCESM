nnNewMask <- function(z2D, imask, omask, newdef=0) {
  # simple index-based nearest neighbour
  # z2D   :: REAL    (:,:), input matrix
  # imask :: LOGICAL (:,:), input mask
  # omask :: LOGICAL (:,:), output mask
  # newdef:: REAL, default value for cropped areas
  
  # require(splancs)
  nx <- dim(z2D)[1]
  ny <- dim(z2D)[2]
  if (!all.equal(dim(imask),c(nx,ny)) || !all.equal(dim(omask),c(nx,ny))) {
    stop('nnNewMask:: input matrix sizes do not match')
  }

  z2D[!omask & imask]   <- newdef 
  imask[!omask & imask] <- FALSE                # speed up later nn search
  omask[imask]  <- FALSE                        # now only new locations
  if (sum(omask) == 0)
    return(z2D)
  
  xy <- cbind(rep(1:nx,ny),rep(1:ny,each=nx))
  nn <- splancs::n2dist(xy[imask,],xy[omask,])$neighs
  z2D[omask] <- z2D[imask][nn]

  return(z2D)
} # end function nnNewMask
