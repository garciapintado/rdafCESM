getCostY <- function(gauDA) {
  
  vKINDs <- names(gauDA)
  m <- ncol(gauDA[[1]]$HE)
  costY <- rep(0,m)
  for (iv in 1:length(vKINDs)) {
    vKIND <- vKINDs[iv]
    costY <- costY + colSums((gauDA[[iv]]$y - gauDA[[iv]]$HE )^2 / gauDA[[iv]]$r)
  }
 costY <- 0.5 * costY
 return(costY)
} # end function getCostY()
