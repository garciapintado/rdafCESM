\name{ocnWmean}
\alias{ocnWmean}
\title{volume-weighted average of ocean variables}
\description{Get the mean value of a scalar field in the ocean component}
\usage{
ocnWmean(nc, vname=NULL, x=NULL, mask=NULL)
}
\arguments{
  \item{nc}{\code{ncdf4} object as returned by the package ncdf4}
  \item{vname}{CHARACTER, variable name}  
  \item{x}{REAL, (nlon, nlat, nz), variable over which to get the statistics} 
  \item{mask}{LOGICAL, (nlon, nlat, nz), mask to select a 3D subgrid for
    the calculations}  
}
\details{
  The function assumes the variable uses the temperature grid. Is it
  thus an approximation for velocity of other fields which use the
  velocity grid. It is possible to use the function for any compliant
  input matrix \code{x} and its associated \code{mask}, if these are
  compliant with the grid in the \code{nc} netCDF file.
}
\value{
A scalar for a 3D grid, or a vector for multistep (4D) grid
}
