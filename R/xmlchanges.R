xmlchanges <- function(xmlf,x,im=NULL) {
 # wrapper to xmlchange - list input
 # for ensemble input, im indicates the ensemble member
 n <- length(x)
 for (i in 1:n) {
   cat('writing ',xmlf,':',names(x)[i],'\n')
   m <- length(x[[i]])
   if (m > 1) {
     if (is.null(im) || im > m || im < 1)
       stop('xmlchanges:: check im input')
   }
   syscmd <- paste('xmlchange ',
                  '-file',xmlf,
                  '-id',  names(x)[i],
                  '-val', ifelse(m == 1, x[[i]], x[[i]][im]))
   system(syscmd)
 }
 return(0)
}

# e.g. system call and re-submission of a continuation run
# hlogin2:/...data/CASE$ xmlchange -file env_run.xml -id RESUBMIT -val 20
# hlogin2:/...data.CASE$ CASE.submit
