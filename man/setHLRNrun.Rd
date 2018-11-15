\name{setHLRNrun}
\alias{setHLRNrun}
\title{template function for setting HPC job submission }
\description{
 This template was used for HLRN III, the North Germany Supercomputer
 Center. It serves as example that should be adapted for other HPCs.
}
\usage{
setHLRNrun(envcase, hpc)
}
\arguments{
  \item{envcase}{LIST with '$CASE' information in CESM terminology}
  \item{hpc}{LIST with supercomputer queue and job parameter}
}
\value{Automated modifications of the *.run CESM 1.2.2 script for job
  submission }
