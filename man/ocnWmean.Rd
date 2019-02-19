\name{ocnWmean}
\alias{ocnWmean}
\title{volume-weighted average of ocean variables}
\description{Get the mean value of a scalar field in the ocean component}
\usage{
ocnWmean(nc, vname)
}
\arguments{
  \item{nc}{\code{ncdf4} object as returned by the package ncdf4}
  \item{vname}{variable name}
}
\details{
  The function assumes the variable uses the temperature grid. Is it
  thus an approximation for velocity of other fields which use the
  velocity grid
}
\value{
A scalar
}
