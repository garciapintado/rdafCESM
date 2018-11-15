\name{getCostY}
\alias{getCostY}
\title{
 Get the observation term of a cost function
}
\description{
 Get the observation term of a cost function to be minimised. That is
 the differences between the model estimate of the observations an
 actual observations, with the norm given by the observation error covariance.
}
\usage{
getCostY(gauDA)
}
\arguments{
  \item{gauDA}{LIST, as defined in rDAF}
}
\value{
A scalar
}
\author{
 Javier Garcia-Pintado
}
