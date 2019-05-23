\name{plotMOC}
\alias{plotMOC}
\title{Plot Meridional Overturning Circulation from CESM POP2 history files }
\description{Plot a Meridional Overtuning Circulation image for a requested
  NetCDF CESM POP2 history file.}
\usage{
plotMOC(fname, pdffile=NULL, moc_ic=1, moc_itr=2,
        xlim=NULL, ylim=NULL, vlim=c(-10,30), supcol=NULL,
        nlevels=21, yscl=1.E-05, pretty_by=10, main='', xlab='Latitude',
        ylab='Depth [km]', it=1, add_bath=TRUE, bath_col='grey20', use_contour=TRUE)
}
\arguments{
  \item{fname}{CHARACTER, input NetCDF file, with path included}
  \item{pdffile}{CHARACTER, output PDF file}
  \item{moc_ic}{INTEGER, index indicating one out of ["Eulerian Mean","Eddy-Induced (bolus)", "Submeso"]}
  \item{moc_itr}{INTEGER, index indicating one out of  ["Global Ocean - Marginal Seas",
                           "Atlantic Ocean + Mediterranean Sea +
			   Labrador Sea + GIN Sea + Arctic Ocean +
			   HudsonBay"]}
  \item{xlim}{ x coordinate [latitude] limits}
  \item{ylim}{ x coordinate [longitude] limits}
  \item{vlim}{ variable limits. If NULL; limits are takem from dataset}
  \item{supcol}{vector of hexadecimal color names. NULL will take a divergence matlab-like palette}
  \item{nlevels}{ncolors+1}
  \item{yscl}{1.E-05 default to convert original [cm] to [km] in depth vector}
  \item{pretty_by}{increment for colorbar ticks. NULL for automated pretty ones}
  \item{main}{main plot title}
  \item{xlab}{label for x axis}
  \item{ylab}{label for y axis}
  \item{it}{INTEGER, for multistep grids, index of the requested time}
  \item{add_bath}{LOGICAL, whether to add bathymetry polygon}
  \item{bath_col}{CHARACTER, color code for bathymetry polygon}
  \item{use_contour}{LOGICAL, TRUE to use filled contours, FALSE for a gridded image}
}
\details{
 Plot an AMOC image and associated colorbar
}
\value{
A plot on an existing or new device
}
