\name{expandLMquery}
\alias{expandLMquery}
\title{ expand a structured lmQuery list into a Data Frame}
\description{ expansion of structures queries into a Data Frame}
\usage{
expandLMquery(lmQuery, climf)
}
\arguments{
  \item{lmQuery}{LIST, where each element, with free name, represents a
    subquery}
  \item{climf}{name of netCDF file to which the query is addressed to}
}
\details{
Each subquery in lmQuery needs to match fields in a netCDF file, as this
function is oriented to finally query netCDF variables at specific
locations via \code{ncvar_sample}. The example below illustrates the
use. The function cycles around each layer and positions to expand the query.
}
\value{
An expanded LIST, where for each subquery, each row represent a specific query
}
\examples{

lmQuery <- list()
lmQuery$WNA       <- list()
lmQuery$WNA$longname <- "West North Atlantic, South of Greenland, latitude of Newfoundland"
lmQuery$WNA$pos      <- c(323,52) 
lmQuery$WNA$vKIND    <- c('POP2.TEMP','POP2.HMXL')
lmQuery$WNA$z        <- c(5,15)    # [m] first 3 layers in POP
lmQuery$ENA       <- lmQuery$WNA
lmQuery$ENA$longname <- "East North Atlantic, around Faroe Islands"
lmQuery$ENA$pos      <- c(348,61) 

 if (1 > 2) { # example: NOT RUN
   climf <- "b.e12.B1850CN.f45_g37.1850_DAtest.000.001.pop.h.1890-1910-ann.b.nc"
   climf <- file.path(envrun$DOUT_S_ROOT[1],'ocn','post',climf)
   lmQueryDF <- expandLMquery(lmQuery, climf)
 }
}
