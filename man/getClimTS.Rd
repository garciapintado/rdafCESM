\name{getClimTS}
\alias{getClimTS}
\title{ Get time series of climate means from CESM history files }
\description{Get climate means from CESM history files for the requested
components through recursive calling to NCO command ncra. All variables
in the original files are included in the temporal means.}
\usage{
getClimTS(tlimStr = NULL, DOUT_S_ROOT, CASE, by = "year", allowNA = TRUE,
             comps = NULL, loop = NULL, overwrite = FALSE)
}
\arguments{
  \item{tlimStr}{CHARACTER, [2], close,open interval [tlimyStr)
    representing the time-window for the model climatic averages. The format
  is AAAA-MM-DD HH:MM:SS. NULL will result in climatic TS over all
  available files in input folders}
  \item{DOUT_S_ROOT}{CHARACTER, [m], path to model data storage for
    IO, as defined in CESM documentation. It is assumed that input
    NetCDF files are given on a monthly basis, and that for each 'im'
    ensemble member and $com component, they are stored in
    \code{$DOUT_S_ROOT[im]/$com/hist}. Out is then at
    \code{$DOUT_S_ROOT[im]/$com/post}, unles \code{loop} below is not null}
  \item{CASE}{CHARACTER, [m], as defined in CESM documentation}
  \item{by}{CHARACTER or REAL, increment of the sequence, given as input
  to {seq.POSIXt} function. See details.}
  \item{allowNA}{LOGICAL, whether it is allowed that some ensemble
    members are not present}
  \item{comps}{CHARACTER, [>=1],  vector of  model components to be analysed, in: ["atm",   "ice",     "lnd",     "ocn",  "rof"]}
  \item{loop}{CHARACTER, if not NULL, output for each component $com will be stored in subfolder '$DOUT_S_ROOT[im]/$com/post/$loop'  }
  \item{overwrite}{LOGICAL, if FALSE, only non existing output fiels
    will be recalculated}
}
\details{
 If \code{comps} is NULL, all components shown above are obtained. This
 function is a wrapper around the NCO command \code{ncra}. See the
 \code{by} argument in \code{seq.POSIXt} for specification details. The
 default is to create annual time series. For
 example, \code{by="3 months"} makes a seasonal time series, or
 \code{by="10 years"} makes a decadal time series.
}
\value{
Writes a series of netCDF files of climatic averages, out of CESM history files.
}
\author{
Javier Garcia-Pintado
}
