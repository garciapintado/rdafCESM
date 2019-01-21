\name{whichMaxAMOC}
\alias{whichMaxAMOC}
\title{ Maximum Meridional Overturning Circulation from CESM POP2 history files }
\description{Get maximum value of Meridional Overtuning Circulation for a requested
  NetCDF CESM POP2 history file. the default values get values for the
  atlantic Ocean}
\usage{
whichMaxAMOC(fname, dimbounds = NULL, moc_ic = 1, moc_itr = 2)
}
\arguments{
  \item{fname}{CHARACTER, input NetCDF file, with path included}
  \item{dimbounds}{list with components \code{lat_aux_grid} and
    \code{moc_z} expressing limits in physical units in which values are
    searched for. Latitude is expressed in degrees, depth is
    expressed in centimeters}
  \item{moc_ic}{INTEGER, index indicating one out of ["Eulerian Mean","Eddy-Induced (bolus)", "Submeso"]}
  \item{moc_itr}{INTEGER, index indicating one out of  ["Global Ocean - Marginal Seas",
                           "Atlantic Ocean + Mediterranean Sea + Labrador Sea + GIN Sea + Arctic Ocean + Hudson Bay"]
  }
}
\details{
  The function has default hardcoded values for latitude limits as
  [0,50] and depth as [500,Inf] meters.
}
\value{
A vector of length=3, with the names ['maxlat','maxz','maxval']
}
