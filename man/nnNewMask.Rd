\name{nnNewMask}
\alias{nnNewMask}
\title{ grid-based nearest neighbour interplation into new mask }
\description{Get a nearest neighbour interpolation of a 2D map from a
  current mask into a new one}
\usage{
nnNewMask(z2D, imask, omask, newdef=0)
}
\arguments{
  \item{z2D}{REAL (:,:), input matrix}
  \item{imask}{LOGICAL (:,:), input mask}
  \item{omask}{LOGICAL (:,:), output mask}
  \item{newdef}{REAL, default value for cropped areas. That is, areas
  where imask is TRUE but omask is FALSE}
}
\details{
  This functions is to ease adaptation to new masks in CESM where both
  the input and the output share the grid. For example this has been
  used for interpolation from a preindustrial
  simulation to be used as initial conditions for a LGM climate
  situation, where the lans-sea mask has been modified.
  z2D, imask and omask are matrices or the same size.
}
\value{
 A 2D matrix
}
\author{
Javier Garcia-Pintado
}
\examples{
  library(rdafCESM)
  nx  <- 100
  ny  <- 200
  oo  <- c(50,100)
  sig <- 30
  xmat <- matrix(1:nx,nx,ny)
  ymat <- matrix(rep(1:ny,each=nx),nx,ny)
  dis2oosq <- (xmat - oo[1])^2 + (ymat - oo[2])^2
  A     <- matrix(exp(-dis2oosq/(2*sig^2)),nx,ny)
  imask <- matrix(FALSE,nx,ny)
  omask <- matrix(FALSE,nx,ny)
  imask[50:90,50:100] <- TRUE
  omask[40:80,60:120] <- TRUE
  A[!imask] <- 0
  Ao <- nnNewMask(A, imask, omask)
  if (1 > 2) {
    rdafPlot::imageF(A)
    rdafPlot::imageF(Ao)
    rdafPlot::imageF(Ao-A)
  }
}