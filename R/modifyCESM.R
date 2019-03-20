modifyCESM <- function(x, mcom, dsn, fname, xnames=NULL, except='', clean=TRUE) {
  # standard modification of CESM model components
  # x     :: list containing elements to be modified
  # mcom  :: model component. One of DRV, CAM, CLM, RTM, CICE, POP2
  # dsn   :: path to output file
  # fname :: output file
  # xnames:: OPTIONAL. If available, subset of variable names to write to file
  # except:: OPTIONAL. vector of variable names, which are exceptions to write
  # clean :: remove previous parameter sets

  m <- length(dsn)
  if (m > 1)
    cat('modifyCESM:: --ensemble input | m=',m,'--\n',sep='')

  if (!(mcom %in% c('DRV','CAM','CLM','RTM','CICE','POP2','DOCN')))
    stop('modifyCESM:: --unknown mcom--')

  if (is.null(x)) {
    cat('modifyCESM:: NULL request\n')
    return(0)
  }

  # check ensemble length
  islist <- sapply(x,is.list)
  islist[names(islist) %in% except] <- FALSE
  if (any(islist)) {
    if (any(sapply(x[islist],length) != m))
     stop('modifyCESM:: --found ensemble input list with length != m--')
  }

  for (im in 1:m) {
    # a) remove previous parameter
     fnamep <- file.path(dsn[im],fname)
     if (!file.exists(fnamep)) {
       cat('file:',fname,'does not exist. No modification applies\n')
       next
     }
     if (clean) {
     foo <- system(paste('grep -vn ^!',fnamep))
     if (foo != 1) {
       l2delete <- as.numeric(sapply(strsplit(system(paste('grep -vn ^!',fnamep),intern=TRUE),':',fixed=TRUE), 
                        function(x){x[1]}))
       system(paste("sed -i '",min(l2delete),",",max(l2delete),"d' ",fnamep,sep=''))
     }
    }
    
    zo <- file(fnamep, 'a')

    vnames <- names(x)
    if (is.null(xnames))                              # write all
      xnames <- vnames
   
    for (i in 1:length(x)) { # write variables for this ensemble member
      if (is.null(x[[i]]) || !(vnames[i] %in% xnames) || vnames[i] %in% except)
        next
    
      if (is.list(x[[i]]))            # ensemble
        val <- x[[i]][[im]]           # possibly multivariate
      else 
        val <- x[[i]]                 # possibly multivariate

      vname <- vnames[i]
      hasnamelist <- length(grep('.',vname,fixed=TRUE))
      if (hasnamelist > 0) {
        if (hasnamelist > 1)
          stop('unidentified namelist names in variable')
        vname <- paste(rev(unlist(strsplit(vname,split='.',fixed=TRUE))),collapse='&')
      }

      if (is.character(val)) {
        if (length(val) == 1)
          cat(vname," = '",val,"'",           "\n",sep='', file=zo)
        else
          cat(vname," = '",paste(val,collapse="','"),"'\n",sep="", file=zo)                     # multivariate input
      } else if (is.logical(val)) {
        if (length(val) == 1)
          cat(vname," = .",val,".",           "\n",sep='', file=zo)
        else
          cat(vname," = .",paste(val,collapse=".,."),".\n",sep="", file=zo)                     # multivariate input
      } else if (is.numeric(val)) {
        if (length(val) == 1)
          cat(vname,' = ',val,              '\n',sep='', file=zo)   
         else
          cat(vname,' = ',paste(val,collapse=','),'\n',sep='', file=zo)
      } else {
       stop('modifyCESM:: unknown variable TYPE')
      }    
    } # end for i
    close(zo)
  } # end for im
  return(0)
} # end modify CESM
