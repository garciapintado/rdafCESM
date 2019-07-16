ocnWmean <- function(nc, vname=NULL, x=NULL, mask=NULL, wsum=FALSE, getprofile=FALSE) {
 # weighted mean of an ocean variable over the ocean 3D grid
 # assumes the temperature grid for the volume weights
 #    
 # ncdf4 connection object            :: nc
 # CHARACTER                         , OPTIONAL :: vname                              
 # REAL,    DIMENSION(nlon, nlat, nz), OPTIONAL :: x
 # LOGICAL, DIMENSION(nlon, nlat, nz), OPTIONAL :: mask

 # details: either vname of x must be given   
 dz    <- ncvar_get(nc,'dz')         # [nz]        thickness of layer [cm]
 TAREA <- ncvar_get(nc,'TAREA')      # [nlon,nlat] area of T cells    [cm^2]
 nz     <- length(dz)
 ncell  <- prod(dim(TAREA))
 
 tvolume   <- array(NA,dim=c(dim(TAREA),nz))
 tvolume[] <- rep(as.numeric(TAREA),nz)*rep(dz,each=ncell)

 if (is.null(vname) && is.null(x))
   stop('ocnWmean: either a netcdf variable name of a 3D field must be provided')
 if (!is.null(vname))
   x <- ncvar_get(nc, vname)

 nt <- ifelse(length(dim(x)) == 3, 1, dim(x)[4])            # time dimension
 if (is.null(mask)) {                                       # get mask from the retrieved variable
   if (nt == 1) {
     mask <- !is.na(x)     
   } else {
     mask <- !is.na(x[,,,1])
   }
 }
 #tsumvol <- sum(tvolume[mask])   # total ocean volumen [cm^2] 

 tvolume[!mask] <- NA
 x[!mask]       <- NA            # for 4D x, mask is recycled for the time dimension
 xsumz <- matrix(NA,nz,nt)       # e.g. mapping of intensive variables (as concentration) into extensive [total mass]
 if (nt == 1) {
   xsumz[,1] <- apply(x*tvolume, MARGIN=3, sum, na.rm=TRUE)
 } else {
   for (it in 1:nt) {
     xsumz[,it] <- apply(x[,,,it]*tvolume, MARGIN=3, sum, na.rm=TRUE)
   }
 }
 if (wsum) {                             # only makes sense for intensive variables [mapped into extensive in the output xsumz]
   if (getprofile) {
     return(xsumz)
   } else {
     return(apply(xsumz, MARGIN=2, sum))   # total sum at each time step
   }
 } else {                                # get mean
   tvolz <- apply(tvolume, MARGIN=3, sum, na.rm=TRUE)   # total [masked] volume at each z level
   if (getprofile) {                     # z-dependent mean
     return(xsumz/tvolz)
   } else {                              # global mean
     return(apply(xsumz, MARGIN=2, sum) / sum(tvolz))          # [nt]
   }    
 }
} # end function ocnWmean
