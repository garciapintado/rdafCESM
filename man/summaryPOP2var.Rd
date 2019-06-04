\name{summaryPOP2var}
\alias{summaryPOP2var}
\title{Summary of a variable in a POP2 simulation}
\description{Get the maximum and minimum of a POP2 grid as well as the
  coresponding array indexes and geographical coordiantes, plus the
  global standard deviation and mean value}
\usage{
  summaryPOP2var(x, long, latg, zg, mask=NULL, z2Did=1)
}
\arguments{
  \item{x}{REAL [nlon,nlat,nz || 1] grid}
  \item{long}{REAL [nlon,nlat], longitude grid}
  \item{latg}{REAL [nlon,nlat], latitude grid}
  \item{zg}{REAL [nz], depth grid in [cm]}
  \item{mask}{LOGICAL [nlon,nlat,nz || 1] grid, matching the dimensions
    of \code{x}. NULL for no masking}
  \item{z2Did}{INTEGER, indicating the index within the depth grid
    \code{zg}, when the input grid \code{x} is a 2D layer. Defaults to
    1, so that for surface layers the returned geographical depth
    coordinate will be that of the uppermost layer in the depth grid,
    even if the variable refers to an ocean top surface variable [e.g. the
    skin layer]}
}
\details{
The mean value provided in this function is the direct mean for the,
possibly masked, grid values. A weighted mean that condisers the volumne
of each model cell is provided by \code{ocnWmean}.
}
\value{
A matrix with a global summmary of a POP2 variable. 
}
