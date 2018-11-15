\name{getNrestarts}
\alias{getNrestarts}
\title{Subdivide long assimilation windows into integration times}
\description{A input vector represents the bound of a sequence of,
  possibly with irregular timespans,
  assimilation windows (DAWs). To comply with computational time limits
  in High Performance Computers each DAW is subdivided in a number of
  restart times, in which CESM is stopped and restarted. The
  assimilation is then done when the complete DAW has been integrated.}
\usage{
getNrestarts(times, dtREST = NULL)
}
\arguments{
  \item{times}{[nt] POSIXct vector representing DAW limits. This is all
    DAW start times plus the end of the last DAW}
  \item{dtREST}{ Scalar constant monthly interval to restart a run }
}
\details{dtREST would come from previous evaluation of model throughput for a
specific model configuration and HPC, as well as the computational time
limits of the specific queue}
\value{
An [nt-1] INTEGER vector indicating the number of restarts needed to simulate each DAW.
}
\author{
Javier Garcia-Pintado
}
