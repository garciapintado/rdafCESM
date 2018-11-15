\name{modifyCESM}
\alias{modifyCESM}
\title{ Modification of CESM model components }
\description{ Modification of CESM model components directly via the R workspace
}
\usage{
modifyCESM(x, mcom, dsn, fname, xnames = NULL, except = "", clean = TRUE)
}
\arguments{
  \item{x}{LIST with simulation parameters to be modified for the specific model component}
  \item{mcom}{model component. One of DRV, CAM, CLM, RTM, CICE, POP2}
  \item{dsn}{path to output file, or '$CASEROOT' in CESM terminology }
  \item{fname}{'user_nl_*' file where the component parameters are
    written to}
  \item{xnames}{OPT, CHARACTER vector, If available, subset of variable names to write to file}
  \item{except}{OPT, CHARACTER vector, If available, subset of variable
    names which are not written to file}
  \item{clean}{remove previous parameter set before writing}
}
\details{The 'inst' folder contains the 'data' and 'scripts' folder with
an example}
\author{
Javier Garcia-Pintado
}
