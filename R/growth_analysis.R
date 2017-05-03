#----------------------------------------------------------------------------------------------------
#- Read in the data, calculate growth analysis terms, plot
#----------------------------------------------------------------------------------------------------


#----------------------------------------------------------------------------------------------------
#- run the prepare script, which will copy over new data
source("R/prepare.R")
#----------------------------------------------------------------------------------------------------



#----------------------------------------------------------------------------------------------------
#- read in the data
files <- list.files("data/harvests/",pattern=".csv",full.names=T)

dat.l <- list()
names <- list()
for (i in 1:length(files)){
  dat.l[[i]] <- read.csv(files[i])
  
  #- initial or final
  dat.l[[i]]$Type <- substr(files[i],start=26,stop=29)
  
  names[[i]] <- names(dat.l[[i]])
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

#- calculate leaf mass fraction, averaged across interval
dat3.m$LMF <- rowMeans(dat3.m[,c("Leafmass.init","Leafmass.fina")])/rowMeans(dat3.m[,c("Totmass.init","Totmass.fina")])

#- calculate stem mass fraction, averaged across interval
dat3.m$SMF <- rowMeans(dat3.m[,c("Stemmass.init","Stemmass.fina")])/rowMeans(dat3.m[,c("Totmass.init","Totmass.fina")])

#- calculate root mass fraction, averaged across interval
dat3.m$RMF <- rowMeans(dat3.m[,c("Rootmass.init","Rootmass.fina")])/rowMeans(dat3.m[,c("Totmass.init","Totmass.fina")])


#- sort species by name, relevel factors
dat3.m$Species <- factor(as.character(dat3.m$Species))
#----------------------------------------------------------------------------------------------------



#----------------------------------------------------------------------------------------------------
#- a few simple plots
pdf("output/MEGA_growth_analysis_first_look.pdf")
ptsize <- 1.5



#- define the color palette. note that this is increasingly problematic with lots of species
nspecies <- length(unique(dat3.m$Species))
rampcolors <- c("pink","blue","darkgrey","forestgreen","green","yellow","orange","red")
#creates a scale of colors
myColorRamp_raw <- function(colors, values) {
  v <- (values - min(values))/diff(range(values))
  #v <- (values - 0)/10
  #v <- values
  x <- colorRamp(colors)(v)
  rgb(x[,1], x[,2], x[,3], maxColorValue = 255)
}
palette(myColorRamp_raw(rampcolors,values=unique(as.numeric(dat3.m$Species))))

par(oma=c(0,0,4,0))
plotBy(AGR~Treatment|Species,data=dat3.m,legendwhere="bottomleft",type="b",pch=16,ylim=c(-10,250),
       cex=ptsize,ylab="AGR (mg day-1)",legend=F)
legend(x=17,y=350,legend=levels(dat3.m$Species),col=palette()[1:nspecies],pch=16,lty=1,ncol=3,xpd=NA,cex=0.8,bty="n")
plotBy(Totmass.fina~Treatment|Species,data=dat3.m,legendwhere="bottomleft",type="b",pch=16,legend=F,
       cex=ptsize,ylab="final mass (mg-1)")
plotBy(RGR~Treatment|Species,data=dat3.m,legendwhere="bottomleft",type="b",pch=16,legend=F,
       cex=ptsize,ylab="RGR (mg mg-1 day-1)")
plotBy(LAR~Treatment|Species,data=dat3.m,legendwhere="bottomleft",type="b",pch=16,legend=F,
       cex=ptsize,ylab="LAR (cm2 mg-1)")
plotBy(ULR~Treatment|Species,data=dat3.m,legendwhere="bottomleft",type="b",pch=16,legend=F,
       cex=ptsize,ylab="ULR (RGR/LAR)")
plotBy(SLA~Treatment|Species,data=dat3.m,legendwhere="bottomleft",type="b",pch=16,legend=F,
       cex=ptsize,ylab="SLA (cm2 mg-1)")
plotBy(LMF~Treatment|Species,data=dat3.m,legendwhere="topleft",type="b",pch=16,legend=F,
       cex=ptsize,ylab="LMF (mg mg-1)")
plotBy(SMF~Treatment|Species,data=dat3.m,legendwhere="topleft",type="b",pch=16,legend=F,
       cex=ptsize,ylab="SMF (mg mg-1)")
plotBy(RMF~Treatment|Species,data=dat3.m,legendwhere="topright",type="b",pch=16,legend=F,
       cex=ptsize,ylab="RMF (mg mg-1)")
dev.off()
#----------------------------------------------------------------------------------------------------

