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







#- standard error of the mean
se <- function(dat,na.rm=F,...){
  if(na.rm==T){
    dat <- subset(dat,is.na(dat)==F)
  }
  std <- sd(dat)
  n <- length(dat)
  se <- std/sqrt(n)
  return(se)
}





# Adds error bars to a plot
adderrorbars <- function(x,y,SE,direction,barlen=0.04,...){
  
  if(length(direction)>1)stop("direction must be of length one.")
  if(direction == "updown")
    direction <- c("up","down")
  else if(direction == "rightleft" | direction == "leftright")direction <- c("left","right")
  
  if("up" %in% direction)
    arrows(x0=x, x1=x, y0=y, y1=y+SE, code=3, angle=90, length=barlen,...)
  if("down" %in% direction) 
    arrows(x0=x, x1=x, y0=y, y1=y-SE, code=3, angle=90, length=barlen,...)
  if("left" %in% direction) 
    arrows(x0=x, x1=x-SE, y0=y, y1=y, code=3, angle=90, length=barlen,...)
  if("right" %in% direction)
    arrows(x0=x, x1=x+SE, y0=y, y1=y, code=3, angle=90, length=barlen,...)  
  
}
