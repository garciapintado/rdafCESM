\name{whichMaxAMOC}
\alias{whichMaxAMOC}
\title{ Maximum Meridional Overturning Circulation from CESM POP2 history files }
\description{Get maximum value of Meridional Overtuning Circulation for a requested
  NetCDF CESM POP2 history file. the default values get values for the
  atlantic Ocean}
\usage{
whichMaxAMOC(fname, dimbounds = NULL, moc_ic = 1, moc_itr = 2,
             getExp = 'S')
}
\arguments{
  \item{fname}{CHARACTER, input NetCDF file, with path included}
  \item{dimbounds}{list with components \code{lat_aux_grid} and
    \code{moc_z} expressing limits in physical units in which values are
    searched for. Latitude is expressed in degrees, depth is
    expressed in centimeters}
  \item{moc_ic}{INTEGER, index indicating one out of ["Eulerian Mean","Eddy-Induced (bolus)", "Submeso"]}
  \item{moc_itr}{INTEGER, index indicating one out of  ["Global Ocean - Marginal Seas",
                           "Atlantic Ocean + Mediterranean Sea +
			   Labrador Sea + GIN Sea + Arctic Ocean + Hudson Bay"]}
  \item{getExp}{REAL, vector of latitudes at which the maximum value of
    the MOC export is evaluated, or a CHARACTER vector with 'S','N' or
    both 'SN'. NULL for no calculation}
}
\details{
  The function has default hardcoded values for latitude limits as
  [0,50] and depth as [500,Inf] meters. Evaluation of water export at
  'S' is defined by the point at the tip of Africa for the Atlantic. 'N'
  is still not defined in this version.
}
\value{
A vector of length=3, with the names ['maxlat','maxz','maxval']
}
