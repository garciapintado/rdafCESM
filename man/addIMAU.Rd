\name{addIMAU}
\alias{addIMAU}
\title{ add a freshwater source to POP2 via IMAU }
\description{ add a freshwater source to the file 'user_nl_pop2' model POP2 in CESM.  }
\usage{ addIMAU(x, dsn) }
\arguments{
  \item{x}{CHARACTER, \code{M$POP2$IMAU} vector}
  \item{dsn}{CHARACTER, \code{envcase$CASEROOT} vector}
}
\details{
 The 'inst' folder in the package provides an example of how to use this function
}
\value{
 Writes a freshwater inflow to 'user_nl_pop2' CESM files 
}
