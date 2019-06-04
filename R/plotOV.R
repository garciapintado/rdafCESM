plotOV <- function(OV) {
 # plot overflow data from a physical space structure as provided in the above list
 # this simple function just plots polygon as straight lines, not great circle paths
 clkw <- c('NW','NE','SE','SW')  # clockwise vertices 
 for (i in 1:length(OV)) {
    polygon(OV[[i]]$int[clkw,1],OV[[i]]$int[clkw,2], border='cyan')
    polygon(OV[[i]]$src[clkw,1],OV[[i]]$src[clkw,2], border='navyblue')
    polygon(OV[[i]]$ent[clkw,1],OV[[i]]$ent[clkw,2], border='darkgreen')
 }
 lines(OV[[i]]$injection[,1],OV[[i]]$injection[,2], col='darkred')
}
