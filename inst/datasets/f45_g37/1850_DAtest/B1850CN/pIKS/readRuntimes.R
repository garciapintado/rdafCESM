# runtimes indicates the start of model integrations and assimilation times (runtimes[-1])
# Each model integration itself may be composed of several sub-integration batches for computational reasons
# That is handled elsewhere.
# Note that as the base timestep is considered to be 1 month for the analysis steps, model integrations need to advance 1 month further
# than the next assimilation time so average background (from model) quantities can be obtained for the last month (possibly with some existing monthly-averaged observation). Then sequential integration steps will start at months indicated in runtimes + 1 month delays.

#CF$runtimes <- seq(CF$staT,CF$endT,by='10 years') #  [POSIXct] include all observation in the last integration
#CF$runtimes <- CF$runtimes[!is.na(CF$runtimes)] 
CF$runtimes <- CF$staT       
