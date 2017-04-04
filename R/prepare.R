#----------------------------------------------------------------------------------------------------
#- Prepare the MEGA folder. This includes loading libraries and copying files over from the share drive
#----------------------------------------------------------------------------------------------------


#----------------------------------------------------------------------------------------------------
#- check if the data and output directories exist. If they don't, create them.
dir.create(file.path("data"),showWarnings=F)
dir.create(file.path("data/harvests"),showWarnings=F)
dir.create(file.path("output"),showWarnings=F)
#----------------------------------------------------------------------------------------------------



#----------------------------------------------------------------------------------------------------
#- find the files on the share drive, copy them over to the data folder in the root directory
files <- list.files("W:/WORKING_DATA/GHS39/MEGA/Share/Harvest/",pattern=".csv",full.names=T)
files.short <- list.files("W:/WORKING_DATA/GHS39/MEGA/Share/Harvest/",pattern=".csv",full.names=F)

topath <- paste(getwd(),"/data/harvests/",sep="")

for (i in 1:length(files)){
  file.copy(from=files[i],to=paste(topath,files.short[i],sep=""),overwrite=T)
}
#----------------------------------------------------------------------------------------------------



#----------------------------------------------------------------------------------------------------
#- load libraries
Library <- function(pkg, ...){
  
  PACK <- .packages(all.available=TRUE)
  pkgc <- deparse(substitute(pkg))
  
  if(pkgc %in% PACK){
    library(pkgc, character.only=TRUE)
  } else {
    install.packages(pkgc, ...)
    library(pkgc, character.only=TRUE)
  }
  
}
Library(doBy)
Library(lubridate)
Library(plotBy)
#----------------------------------------------------------------------------------------------------
