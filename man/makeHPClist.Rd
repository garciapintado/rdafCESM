\name{makeHPClist}
\alias{makeHPClist}
\title{ template for default HPC job parameters
}
\description{ Template function for default HPC job parameters. This
  template is for HLRN III in Nort Germany, with a PBS queue system.
  general user for a general HPC would copy this function and make a
  modification in his/her own system adapted to the default parameter in
  the specific HPC. Note a template is not strictly needed, as all
  parameter given in the input list \code{hpc0} overwrite default
  parameters.
}
\usage{
makeHPClist(hpc0)
}
\arguments{
  \item{hpc0}{LIST, where available components overwrite the default
    values in this function
}
}
\value{
A LIST with 
  \item{account_name }{to which computing quota this job is allocated to}
  \item{feature}{queue name}
  \item{nodes}{number of nodes}
  \item{ppn}{number of procesors per node}
  \item{walltime}{time requested for the job, which should comply with
    the HPC limits}
}
