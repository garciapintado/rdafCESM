getOV <- function() {
 # return a structure with overflow paramaterization
 #!R
# this file makes a structure with overflow parameterizations in physical space
# these are extracted from the Technical report by Danabasoglu et al. (2010)
# TODO: only Denmark strait is included here. Remaining overflows have to be included. 

 # constants
 cm <- 1.0E+02

 OV <- list()
 ovdimnames <- list(c('NW','NE','SW','SE'),c('lon','lat'))

 i <- 1 
 OV[[i]] <- list()
 OV[[i]]$name <- 'Denmark Strait'
 OV[[i]]$int <- matrix(c(327.68, 65.30,
                         331.68, 65.57,
                         328.42, 62.57,
                         332.82, 62.84), 4, 2, byrow=TRUE, dimnames=ovdimnames)
 OV[[i]]$src <- matrix(c(334.11, 67.94,
                         340.88, 68.96,
                         334.76, 67.14,
                         341.92, 68.18), 4, 2, byrow=TRUE, dimnames=ovdimnames)
 OV[[i]]$ent <- matrix(c(329.28, 65.39,
                         331.68, 65.57,
                         329.83, 63.83,
                         332.37, 64.00), 4, 2, byrow=TRUE, dimnames=ovdimnames)
 OV[[i]]$zi  <- c(33,33,39)
 OV[[i]]$z   <- c(504,504,928)*cm
 OV[[i]]$injection <- matrix(c(331.73, 63.36, 1483*cm,
                               330.87, 63.30, 1863*cm,
                               329.93, 63.44, 2075*cm,
                               329.24, 62.81, 2298*cm,
                               328.26, 63.15, 2530*cm,
                               327.44, 62.91, 2768*cm,
                               325.12, 60.32, 3011), 7, 3, byrow=TRUE,
                               dimnames=list(1:7,c('lon','lat','z')))
 return(OV)
}
