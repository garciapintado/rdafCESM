ocnWmean <- function(nc, vname) {
 # "ncdf4" object
 # weighted mean of an ocean variable over the ocean 3D grid
 dz    <- ncvar_get(nc,'dz')         # [nz]        thickness of layer [cm]
 TAREA <- ncvar_get(nc,'TAREA')      # [nlon,nlat] area of T cells    [cm^2]
 nz     <- length(dz)
 ncell  <- prod(dim(TAREA))
 
 tvolume   <- array(NA,dim=c(dim(TAREA),nz))
 tvolume[] <- rep(as.numeric(TAREA),nz)*rep(dz,each=ncell)

 x <- ncvar_get(nc, vname)
 isna <- is.na(x) 
 tsumvol <- sum(tvolume[!isna])   # total ocean volumen [cm^2]

 sum(x[!isna]*tvolume[!isna])/tsumvol 

}
