\name{POPid2geo}
\alias{POPid2geo}
\title{Get Geographical Coordinates of a POP2 3D Grid Cell}
\description{Get the geographical coordinates in a POP2 3D grid cell
  given by the 3D array indices}
\usage{
  POPid2geo(ids, long, latg, zg)
}
\arguments{
  \item{ids}{INTEGER [3], vector of array indices}
  \item{long}{REAL [nlon,nlat], longitude grid}
  \item{latg}{REAL [nlon,nlat], latitude grid}
  \item{zg}{REAL [nz], depth grid in [cm]}
}
\value{
 A vector with longitude, latitude, and depth [cm] of the grid cell
}
