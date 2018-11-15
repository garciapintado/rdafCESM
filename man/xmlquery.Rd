\name{xmlquery}
\alias{xmlquery}
\title{ Wrapper to CESM xmlquery }
\description{
Conduct a xmlquery directly from R
}
\usage{
xmlquery(dsn, id, fileonly = FALSE, valonly = FALSE)
}
\arguments{
  \item{dsn}{CHARACTER, path to the xml file}
  \item{id}{xml variable}
  \item{fileonly}{only print the filename where the variable is}
  \item{valonly}{only prints the variable value}
}
\details{
returns the content of a variable in a xml CESM file directly into an R
workspace. This is only to provide a minimal way of syncrhonising R
workspace variable and xmk variables for the purpose of preparing the assimilation setup. See the CESM documentation of tool about the much more
flexible system xmlquery command.
}
\value{
CHARACTER, the result of a variable in a xml CESM configuration file
}
