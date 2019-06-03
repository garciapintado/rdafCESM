summaryPOP2var <- function(x, long, latg, zg, mask=NULL, z2Did=1) {
  # x     :: 2D or 3D
  # z2Did :: vertical layer index for 2D input
  if (is.null(mask))
    mask <- array(TRUE,dim=dim(x))
  maxv <- max(x[mask], na.rm=TRUE)
  maxi <- which(x==maxv, arr.ind=TRUE) # [lon,lat,z]
  if (is.matrix(maxi))
    maxi <- maxi[1,]                                      # anyone
  if (length(maxi) == 2) {maxi = c(maxi,z2Did)}           # 2D layer
  maxg <- POPid2geo(maxi, long, latg, zg)
  minv <- min(x[mask], na.rm=TRUE)
  mini <- which(x==minv, arr.ind=TRUE) # [lon,lat,z]
  if (is.matrix(mini))
    mini <- mini[1,]                                      # anyone
  if (length(mini) == 2) {mini = c(mini,z2Did)}           # 2D layer
  ming <- POPid2geo(mini, long, latg, zg)
  #browser()
  ans <- rbind(c(mini,ming,minv),
               c(maxi,maxg,maxv),
               c(rep(NA,6),mean(x[mask], na.rm=TRUE)),
               c(rep(NA,6),sd(x[mask], na.rm=TRUE))                                          )
  rownames(ans) <- c('min','max','mean','sde')
  colnames(ans) <- c('xi','yi','zi','x','y','z','val')
  return(ans)
}
