ocnWmean <- function(nc, vname=NULL, x=NULL, mask=NULL) {
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

 nt <- ifelse(length(dim(x)) == 3, 1, dim(x)[4])
 if (is.null(mask)) { # get mask from the retrieved variable
   if (nt == 1) {
     mask <- !is.na(x)     
   } else {
     mask <- !is.na(x[,,,1])
   }
 }
 tsumvol <- sum(tvolume[mask])   # total ocean volumen [cm^2] 

 xbar <- rep(NA,nt)
 if (nt == 1) {
   xbar[1] <-  sum(x[mask]*tvolume[mask])/tsumvol   
 } else {
   for (it in 1:nt) {
     xbar[it] <- sum(x[,,,it][mask]*tvolume[mask])/tsumvol 
   }
 }
 return(xbar)
}
