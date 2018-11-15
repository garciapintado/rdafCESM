\name{xmlchanges}
\alias{xmlchanges}
\title{Wrapper around the xmlchange CESM script}
\description{
 Conduct changes in xml files needed for CESM run.
}
\usage{
xmlchanges(xmlf, x, im = NULL)
}
\arguments{
  \item{xmlf}{CHARACTER, xml filename}
  \item{x}{LIST with named elements and values to be assigned in the xml
  file}
  \item{im}{for ensemble input, im indicates the ensemble member}
}
