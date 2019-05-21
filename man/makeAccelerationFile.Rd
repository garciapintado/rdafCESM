\name{makeAccelerationFile}
\alias{makeAccelerationFile}
\title{make a depth-dependent tracer acceleration input file for POP2}
\description{Make a depth-dependent tracer acceleration input file for
  POP2. This file, can then be used as input for the startd CESM tracer acceleration}
\usage{
makeAccelerationFile(supDF=NULL, fname=NULL)
}
\arguments{
  \item{supDF}{\code{data.frame} with fields \code{z} (depth [m]), and
    \code{a} (acceleration factor) to be ussed as support data for the
    smoothed depth-dependent interpolation}
  \item{fname}{CHARACTER, variable name}  
}
\details{
 The function creates a smoothed depth-dependent acceleration
 file. Factors further than the depth bounds indicated in \code{supDF},
 take the corresponding bound acceleration factors.
 Default values is approaches the profile evaluated in Danabasoglu
 (2004). Note that Danabasoglu (2004) indicates that it is better to do
 a momentum-based acceleration in CESM than depth-dependent acceleration.   
}
\value{
An ASCII file with an acceleration factor for each of the 60 ocean model
layers in CESM-POP2.
}
\examples{
  supDF <- data.frame(z=c(0,1000,2000,3000,4000,5500),
                      a=c(1,1,   5,   10,  20  ,20))
  acc <- makeAccelerationFile(supDF)
}