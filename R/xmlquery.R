xmlquery <- function(dsn, id, fileonly=FALSE, valonly=FALSE) {
 # wrapper to xmlquery
 # dsn: folder holding .xml CESM files and corresponding xmlquery utility
 
 cwd <- getwd()
 setwd(dsn)
 
 if (length(id) > 1)
  id <- paste(id,collapse=',')

 if (fileonly && valonly)
   stop('xmlquery:: valonly and fileonly modes can NOT both be set ')

 syscmd <- paste('xmlquery ',id)
 if (fileonly)
   syscmd <- paste(syscmd,'-fileonly')
 if (valonly)
   syscmd <- paste(syscmd,'-valonly')
 ans <-  system(syscmd, intern=TRUE)

 if (valonly)
   ans <- strsplit(ans, split=' = ', fixed=TRUE)[[1]][2]
 setwd(cwd)
 return(ans)
}


