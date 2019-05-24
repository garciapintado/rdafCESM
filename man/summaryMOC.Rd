\name{summaryMOC}
\alias{summaryMOC}
\title{Summary of Meridional Overturning Circulation from CESM POP2 history files }
\description{Get global maximum, global minimum, as well as southward export at
  South, Equator, and North of Meridional Overtuning Circulation for a requested
  NetCDF CESM POP2 history file. the default values get values for the
  atlantic Ocean}
\usage{
  summaryMOC(fname, dimbounds = NULL, moc_ic = 1, moc_itr = 2, it = NULL,
             fromPlotMOC = FALSE)
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
			   Labrador Sea + GIN Sea + Arctic Ocean +
			   HudsonBay"]}
  \item{it}{INTEGER, time index for multistep grids. NULL (default) get
    the mean values over all timesteps available in the netCDF file}
  \item{fromPlotMOC}{\code{plotMOC} function shares the loading of the
    netCDF with this function. This argument is to be set to TRUE only
    internally when called from \code{plotMOC}}
}
\details{
  The function has default hardcoded values for latitude limits as
  [0,50] and depth as [500,Inf] meters.
}
\value{
A matrix of MOC values plus corresponding latitude and depth  
}
