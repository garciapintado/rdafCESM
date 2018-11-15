\name{elapsed_months}
\alias{elapsed_months}
\title{
  Number of ellapsed months between two POSIXct times
}
\description{
 The integer number of complete months between two POSIXct timestamps is obtained
}
\usage{
elapsed_months(t0, t1)
}

\arguments{
  \item{t0}{POSIXct timestamp}
  \item{t1}{POSIXct timestamp}
}
\details{
  The integer number of complete ellapsed months between two POSIXct timestamps.
}
\value{
An integer
}
\examples{
 t0 <- Sys.time()
 t1 <- t0 + 60*60*24*365*500       # nearly 500 years later
 dmonth <- elapsed_months(t0, t1)
}