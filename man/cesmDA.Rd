\name{cesmDA}
\alias{cesmDA}
\title{
 Climate data assimilation with CESM
}
\description{This is the main function acting as interface between CESM
  and the assimilation via the package \code{rDAF}. Input here is very
  specific to CESM, and the user is referrred to CESM 1.2.2
  documentation for those item specifically marked as CESM standard names.

  The "inst" folder provides the folder "scripts" with the file
  cesm_setup.R, along with the folder "example_input". For a specific
  simulation, the file main.R is the starting point, showing an example of this package use.
}
\usage{
cesmDA(envcase, DOUT_S_ROOT, comps, tavgs, tlimyStr, timenowStr,
       ilStr = NULL, ana, MC, prm, useGA, useGAy, yls, theta, MCgpar,
       analysis_scn = "a0", GET_ANALYSIS = TRUE, MEMORY_SAVE = TRUE,
       SAVE_NETCDF = TRUE, dsnrest = NULL, POP.zlrange=1)
}
\arguments{
  \item{envcase}{[>=3] list (CCSMROOT , CASE[m], CASEROOT[m])}
  \item{DOUT_S_ROOT}{[m]   CHARACTER; path to model data storage for IO. See CESM documentation}
  \item{comps}{ [>=1] list with named elements. Names are model component to be analysed: in:  c("atm",   "ice",     "lnd",     "ocn",  "rof").
                Then, each element contains a  [>=1] CHARACTER vector of variables to be analysed, matching variables archived in history files}
  \item{tavgs}{label to tag seasonal climate averages, in ("JFM","JAS","ann")}
  \item{tlimyStr}{ [2]   CHARACTER, close,open interval [tlimyStr)
    representing the time-averaged footprint of the observations. The format
  is AAAA-MM-DD HH:MM:ss}
  \item{timenowStr}{[1] CHARACTER, current timestamp in the above format}
  \item{ilStr}{CHARACTER, as two-digit loop index. For example '01' for
    an initial loop in an iterated scheme}
  \item{ana}{LIST, analysis variables, as defined in the package rDAF}
  \item{MC}{Data frame, as defined in the package rDAF}
  \item{prm}{LIST, assimilation global parameters, as defined in the package rDAF
}
  \item{useGA}{LOGICAL whether to use Gaussian anamorphosis in analysed
    variables for Ensemble schemes}
  \item{useGAy}{LOGICAL, whether to also transform observations}
  \item{yls}{LIST, observations, as defined in the package rDAF}
  \item{theta}{LIST, see the cesm_setup.R in 'inst' folder in the
    package for an example}
  \item{MCgpar}{REAL, as defined in the package rDAF }
  \item{analysis_scn}{CHARACTER, label to identify the output files in
    the current assimilation scenario}
  \item{GET_ANALYSIS}{LOGICAL, whether to actually do the assimilation
    or only the data preparation and preliminar sensitivity analysis}
  \item{MEMORY_SAVE}{LOGICAL, whether to analysis spatial data layer by
    layer. For a few layer, this may be slower that analysing all layers
  at once, but commonly this should be set to TRUE so there is no limit
  in the number of variables and layer analysed regarding the saturation
  of memory in the R workspace}
  \item{SAVE_NETCDF}{LOGICAL, whether in addition to the R binary files,
    the netCDF files are to be rewritten. This is normally FALSE for
    single-step analysis, but TRUE for sequences of Data Assimilation
    Windows (DAWs)}
  \item{dsnrest}{CHARACTER, path to store update control variable files
    for restarting the simulation. See the example in the 'inst' folder}
  \item{POP.zlrange}{INTEGER, range of ocean layers to analyse, with the
  index 1 being the surface layer}
}
\details{
 The best way to understand how to use (adapt, extent) this function is
 via the example provided in the 'inst' folder in this package. Users
 are encouraged to contribute to the testing and development via the Github repository.
}
\value{
 The function writes updated variables of CESM as system files or as
 netCDF files. In adition, an updated matrix of the control variables is
 returned by the function to the calling environment.
}
\references{
Garcia-Pintado and Paul, 2018, GMD.
}
\author{
Javier Garcia-Pintado
}


