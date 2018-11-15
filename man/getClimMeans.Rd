\name{getClimMeans}
\alias{getClimMeans}
\title{ Get climate means from CESM history files }
\description{Get climate means from CESM history files for the requested
components through recursive calling to NCO command ncra. All variables
in the original files are included in the temporal means.}
\usage{
getClimMeans(tlimStr, DOUT_S_ROOT, CASE, getMonths = TRUE, allowNA = TRUE,
             comps = NULL, loop = NULL)
}
\arguments{
  \item{tlimStr}{CHARACTER, [2], close,open interval [tlimyStr)
    representing the time-averaged footprint for the model climatic averages. The format
  is AAAA-MM-DD HH:MM:ss}
  \item{DOUT_S_ROOT}{CHARACTER, [m] path to model data storage for
    IO, as defined in CESM documentation}
  \item{CASE}{CHARACTER, [m], as defined in CESM documentation}
  \item{getMonths}{LOGICAL, whether mean momthly climatic averages have been
    previously calculated}
  \item{allowNA}{Is allowed that some ensemble member has failed}
  \item{comps}{[>=1] CHARACTER of  model component to be analysed, in:  c("atm",   "ice",     "lnd",     "ocn",  "rof")}
  \item{loop}{[1]   CHARACTER: If not NULL, output for each component $com will be stored in subfolder '$DOUT_S_ROOT/$com/post/$loop'  }
}
\details{
 If \code{comps} is NULL, all components shown above are obtained.
}
\value{
Write netCDF files of climatic averages, out of CESM history files, for
the requested range
}
\author{
Javier Garcia-Pintado
}
