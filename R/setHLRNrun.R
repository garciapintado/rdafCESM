setHLRNrun <- function(envcase, hpc) {
  # +++ purpose +++
  # modify #PBS directives in $CASE.run file/s
  # auth:    Javier G.P. 
  # created: 2018-03-01 
 
  m <- length(envcase$CASE)
  cwd <- getwd()
  for (im in 1:m) {
    setwd(envcase$CASEROOT[im])
   #system(paste("perl -pi -e 's/.*/#PBS -A ",hpc$account_name,"/ if /#PBS -A/' ",           envcase$CASE[im],".run",sep=""))      #PBS -A  
   #system(paste("perl -pi -e 's/PBS -A \\w{8}/PBS -A ",hpc$account_name,"/' ",              envcase$CASE[im],".run",sep=""))      #PBS -A  
    system(paste("perl -pi -e 's/feature=.+/feature=",hpc$feature,"/' " ,         envcase$CASE[im],".run",sep=""))      #PBS -l feature
    system(paste("perl -ni -e 'print unless /PBS -q /' ",                                   envcase$CASE[im],".run",sep=""))      # HLRN3 recomends not to use -q. Inherits from feature
    system(paste("perl -pi -e 's/nodes=\\d{1,}/nodes=",hpc$nodes,"/' ",                      envcase$CASE[im],".run",sep=""))                     #PBS -l nodes 
    system(paste("perl -pi -e 's/walltime=\\d{2}:\\d{2}:\\d{2}/walltime=",hpc$walltime,"/' ",envcase$CASE[im],".run",sep=""))      #PBS -l walltime

    if (file.exists(paste(envcase$CASE[im],"st_archive",sep="."))) {
      # for CESM1.3, where $CASE.st_archive is submitted an an independent job
      # note: HLRN3 feature=data time limit is 03:00:00. If this is exceeded one need to select feature=prepost
      system(paste("perl -pi -e 's/feature=.+/feature=prepost/' " ,                            envcase$CASE[im],".st_archive",sep=""))      #PBS -l feature
      system(paste("perl -pi -e 's/nodes=\\d+:ppn=\\d+/nodes=4:ppn=1' ",                       envcase$CASE[im],".st_archive",sep=""))      #PBS -l nodes 
      system(paste("perl -pi -e 's/walltime=\\d{2}:\\d{2}:\\d{2}/walltime=","00:30:00","/' ",  envcase$CASE[im],".st_archive",sep=""))      #PBS -l walltime
    }
  }
  setwd(cwd)
  return(0)
}
