\name{ncvar_sample}
\alias{ncvar_sample}
\title{ sample specific locations from netCDF files }
\description{ simple wrapper to sample netCDF files at specific grid locations}
\usage{
ncvar_sample(fnamep, queryDF)
}
\arguments{
  \item{fnamep}{CHARACTER, [2], name, including path to a netCDF file}
  \item{queryDF}{ data.frame, with columns 'x','y','zl', 'vKIND',
    representing longitude, latitude, level in a 3D field, and variable name}
}
\value{
A vector of samples, one value per row in \code{queryDF}
}
