#----------------------------------------------------------------------------------------------------
#- Read in the data, calculate growth analysis terms, plot
#----------------------------------------------------------------------------------------------------



#----------------------------------------------------------------------------------------------------
#- read in the data
files <- list.files("data/harvests/",pattern=".csv",full.names=T)

dat.l <- list()
for (i in 1:length(files)){
  dat.l[[i]] <- read.csv(files[i])
  
  #- initial or final
  dat.l[[i]]$Type <- substr(files[i],start=26,stop=29)
}
dat1 <- do.call(rbind,dat.l)
dat1$Date <- as.Date(dat1$Date,format="%d/%m/%Y")
dat1$Type <- factor(dat1$Type,levels=c("init","fina"))
#----------------------------------------------------------------------------------------------------



#----------------------------------------------------------------------------------------------------
#- average across initial and final, break apart, recombine for growth analysis
dat1$Totmass <- rowSums(dat1[,c("Leafmass","Stemmass","Rootmass")])
dat1$lnTotmass <- log(dat1$Totmass)
dat1.m <- summaryBy(LA+Height+Leafmass+Stemmass+Rootmass+Totmass+lnTotmass~Species+Treatment+Type,data=dat1,
                    FUN=mean,keep.names=T,na.rm=T)

#- break apart, rename, merge back together 
dat1.m.init <- subset(dat1.m,Type=="init")
names(dat1.m.init)[4:10] <- paste(names(dat1.m.init)[4:10],".init",sep="")
dat1.m.fina <- subset(dat1.m,Type=="fina")
names(dat1.m.fina)[4:10] <- paste(names(dat1.m.fina)[4:10],".fina",sep="")
dat2.m <- merge(dat1.m.init[,c(1,2,4:10)],dat1.m.fina[,c(1,2,4:10)],by=c("Species","Treatment"))


#- calculate absolute and relative growth rates
dat2.m$AGR <- with(dat2.m,(Totmass.fina-Totmass.init)/10)
dat2.m$RGR <- with(dat2.m,(lnTotmass.fina-lnTotmass.init)/10)

#- subset to just taxa with AGR data
dat3.m <- subset(dat2.m,!is.na(AGR))
dat3.m$Species <- factor(dat3.m$Species)

#- calculate leaf area ratio, averaged across interval
dat3.m$LAR <- rowMeans(dat3.m[,c("LA.init","LA.fina")])/rowMeans(dat3.m[,c("Totmass.init","Totmass.fina")])

#- calculate unit leaf rate, averaged across interval
dat3.m$ULR <- with(dat3.m,RGR/LAR)

#- calculate specific leaf area, averaged across interval
dat3.m$SLA <- rowMeans(dat3.m[,c("LA.init","LA.fina")])/rowMeans(dat3.m[,c("Leafmass.init","Leafmass.fina")])
#----------------------------------------------------------------------------------------------------



#----------------------------------------------------------------------------------------------------
#- a few simple plots
plotBy(AGR~Treatment|Species,data=dat3.m,legendwhere="bottomleft",type="b",pch=16,ylim=c(-20,250),
       ylab="AGR (mg day-1)")
plotBy(RGR~Treatment|Species,data=dat3.m,legendwhere="bottomleft",type="b",pch=16,
       ylab="RGR (mg mg-1 day-1)")
plotBy(LAR~Treatment|Species,data=dat3.m,legendwhere="bottomleft",type="b",pch=16,
       ylab="LAR (cm2 mg-1)")
plotBy(ULR~Treatment|Species,data=dat3.m,legendwhere="bottomleft",type="b",pch=16,
       ylab="ULR (RGR/LAR)")
plotBy(SLA~Treatment|Species,data=dat3.m,legendwhere="bottomleft",type="b",pch=16,
       ylab="SLA (cm2 mg-1)")

#----------------------------------------------------------------------------------------------------
