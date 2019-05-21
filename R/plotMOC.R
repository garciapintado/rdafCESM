plotMOC <- function(fname, pdffile=NULL, moc_ic=1, moc_itr=2,
                    xlim=c(-35,88), ylim=NULL, vlim=c(-10,30), supcol=NULL,
                    nlevels=21,
                    yscl=1.E-05, pretty_by=10, main='', xlab='Latitude',
                    ylab='Depth [km]', it=1) {

  # fname     :: POP2 NetCDF file, including path, from which AMOC is obtained
  # pdffile   :: if not NULL, a new PDF is created
  # moc_ic    :: index within ["Eulerian Mean","Eddy-Induced (bolus)", "Submeso"]
  # moc_itr   :: index within ["Global Ocean - Marginal Seas",
  #                          "Atlantic Ocean + Mediterranean Sea + Labrador Sea + GIN Sea + Arctic Ocean + Hudson Bay"]
  # xlim      :: x coordinate [latitude] limits 
  # vlim      :: if NULL; limits are takem from dataset
  # supcol    :: vector of hexadecimal color names. NULL will take a divergence matlab-like palette 
  # nlevels   :: ncolors+1
  # yscl      :: 1.E-05 default to convert original [cm] to [km] in depth vector
  # pretty_by :: increment for colorbar ticks. NULL for automated pretty ones.
  # it        :: for multistep grids, index of the requested time   
  nci <- nc_open(fname)
  vnames <- c('TLONG','TLAT','ULAT','REGION_MASK','lat_aux_grid','moc_z', 'MOC')             # support variables, each: [nlon,nlat]
  for (vname in vnames) {
    eval(parse(text=paste(vname,' <- list(); ',vname,'$att <- ncatt_get(nci,"',vname,'");',
                                               vname,'$vals <- ncvar_get(nci,"',vname,'")',sep='')))
  }; rm(vnames)
                    
  dimnames <- c('lat_aux_grid','moc_z')  
  MOC$coordinates <- strsplit(MOC$att$coordinates,' ', fixed=TRUE)
  if (!all(names(dimnames) %in% MOC$coordinates))
    stop('whichMaxAMOC:: ---ERR001---')

  vKIND <- 'MOC'
  MOC$dim  <- nci$var[[vKIND]]$dim
  if (length(dim(MOC$vals)) == 5)
    MOC$vals <- MOC$vals[,,,,it]
  layer    <- as.numeric(MOC$vals[,,moc_ic,moc_itr]) # [n_auxlat,n_zt] = [nx,ny], where:

  ncDimnames <- sapply(nci$var[[vKIND]]$dim, function(x){x$name}) # == coordinates

  xseq <- MOC$dim[[1]]$vals          # lat_aux_grid
  yseq <- MOC$dim[[2]]$vals*yscl     # moc_z
  nx   <- length(xseq)               # [105]
  ny   <- length(yseq)               # [61]

  if (is.null(xlim))
    xlim <- range(xseq)
  if (is.null(ylim)) {
    ylim <- rev(range(yseq))                  # reverse for MOC plots
  }
  if (is.null(vlim))
    vlim <- range(layer, na.rm=TRUE)
  if (is.null(supcol))
    supcol <- rdafPlot::matlabLike0()         # 0-white centered divergent colorscale
  #browser()
  if (is.null(pretty_by)) {
    vpretty <- pretty(vlim)
  } else {
    vpretty <- seq(vlim[1],vlim[2],by=pretty_by)
  }
  vlim <- range(vpretty)                  # readjust for pretty colorscale

  if ("#FFFFFF" %in% supcol) {            # assume 0-white centered divergent palette 
    supval <- rdafPlot::getSupval0(supcol, vlim)              # 0-white centered
  } else {
    supval <- seq(vlim[1],vlim[2],length=length(supcol))
  }
  colpal <- list(val=supval, col=supcol); class(colpal) <- "palette"

  nc  <- 1
  nr  <- 1
  mai.main          <- c(1.6,1.6,1.0,0.5)/2.54                  # [in] margins of main figure
  mai.legend        <- c(1.6,1.6,0.2,0.5)/2.54                  # [in] margins of legend    
  width.main.plot   <- 5      #1 + G$cols/107                   # [in] width of figure without margins. Good for png and ncol about 1000 in maps
  width.main        <- width.main.plot+mai.main[2]+mai.main[4]  # [in] width of figure including margins
  width.legend      <- width.main*nc                            # one legend stretched over the two plots                              
  height.legend     <- 2.4/2.54                                 # [in]                                                                                          
  aspect <- 0.6
  height.main.plot  <- width.main.plot*aspect
  height.main       <- height.main.plot + mai.main[1] + mai.main[3]
  width.paper       <- nc * width.main #+ width.legend                  # [in] width of paper
  height.paper      <- nr * height.main + height.legend

  mat <- matrix(c(1,2,3,3),nrow=nr+1,ncol=nc,byrow=TRUE)
  widths  <- rep(width.main,nc)  * 2.54                                                                                                    
  heights <- c(rep(height.main,nr),height.legend) * 2.54                                                                                                      

  if (!is.null(pdffile)) {
    pdf(file=pdffile, width = width.paper, height = height.paper, pointsize=12)               
  } else {
    dev.new(width=width.paper, height = height.paper)
  }
  # dev.size(units='in')
  layout(mat=mat, widths=lcm(widths*0.99), heights=lcm(heights*0.99), respect=TRUE)  
  #layout.show()
  # layer <- matrix(ol[[i]]$MOC.Ge[,ip], nx, ny)
  par(mai=mai.main)
  par(mgp=c(2.3,1,0))
  plot(0, 0, xlim=xlim, ylim=ylim, type='n', xaxs='i', yaxs='i',
       main=main, xlab=xlab, ylab=ylab, font.lab=2, las=1)
    # plot from top to bottom
  #browser()                                           
  rdafPlot::callFilledContour(xseq, yseq, matrix(layer,nx,ny), zlim=vlim,
                              nlevels=nlevels, colpal=colpal)
  contour(xseq, yseq, matrix(layer, nx, ny), add=TRUE , level=0, col='grey50')

  #polygon(AMOCbath$x,AMOCbath$y/100000,col='grey20',border='grey10')

  # legend
  par(mai=mai.legend)
  par(mgp=c(3.5,1,0))
  label <- 'Sverdrups' 
  rdafPlot::colorbarG(mai=mai.legend, zlim=vlim, zpretty=vpretty, text=label,
                      supcol=colpal$col, supval=colpal$val, nlevels=nlevels,
                      layout='h', leftarrow=TRUE, rightarrow=TRUE, col0div='grey50')

  if (!is.null(pdffile))
    dev.off()
} # end function plotMOC
