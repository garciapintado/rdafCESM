makeHPClist <- function(hpc0) {
 # template for default computing job parameters in HLRN   
 # default for mpp2 in HLRN   
 hpc <- list()
 hpc$account_name <- 'hbk00059'
 hpc$feature      <- 'mpp2'
 hpc$nodes        <- 15
 hpc$ppn          <- 24        
 hpc$walltime     <- '12:00:00'

 if (!is.null(hpc0)) {
   for (i in 1:length(hpc0)) {
     vname <- names(hpc0)[i]
     if (vname %in% names(hpc))
       hpc[[vname]] <- hpc0[[vname]]
   }
 }
 return(hpc)
}
