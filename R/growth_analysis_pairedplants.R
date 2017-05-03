#----------------------------------------------------------------------------------------------------
#- Read in the data, calculate growth analysis terms, plot.
#  This is an alternative version that merged based on plant pairs (initial and final harvests)
#----------------------------------------------------------------------------------------------------


#----------------------------------------------------------------------------------------------------
#- run the prepare script, which will copy over new data
source("R/prepare.R")
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
dat1$Comment <- NULL
#----------------------------------------------------------------------------------------------------



#----------------------------------------------------------------------------------------------------
#- average across initial and final, break apart, recombine for growth analysis
dat1$Totmass <- rowSums(dat1[,c("Leafmass","Stemmass","Rootmass")])
dat1$lnTotmass <- log(dat1$Totmass)


#- break apart, rename, merge back together 
dat1.init <- subset(dat1,Type=="init")
names(dat1.init)[5:12] <- paste(names(dat1.init)[5:12],".init",sep="")
dat1.fina <- subset(dat1,Type=="fina")
names(dat1.fina)[5:12] <- paste(names(dat1.fina)[5:12],".fina",sep="")
dat2 <- merge(dat1.init[,c(2:9,11:12)],dat1.fina[,c(2:9,11:12)],by=c("Species","Treatment","Tree"))

#- calculate absolute and relative growth rates
dat2$AGR <- with(dat2,(Totmass.fina-Totmass.init)/10)
dat2$RGR <- with(dat2,(lnTotmass.fina-lnTotmass.init)/10)

#- subset to just taxa with AGR data
dat3 <- subset(dat2,!is.na(AGR))
dat3$Species <- factor(dat3$Species)

#- calculate leaf area ratio, averaged across interval
dat3$LAR <- rowMeans(dat3[,c("LA.init","LA.fina")])/rowMeans(dat3[,c("Totmass.init","Totmass.fina")])

#- calculate unit leaf rate, averaged across interval
dat3$ULR <- with(dat3,RGR/LAR)

#- calculate specific leaf area, averaged across interval
dat3$SLA <- rowMeans(dat3[,c("LA.init","LA.fina")])/rowMeans(dat3[,c("Leafmass.init","Leafmass.fina")])

#- calculate leaf mass fraction, averaged across interval
dat3$LMF <- rowMeans(dat3[,c("Leafmass.init","Leafmass.fina")])/rowMeans(dat3[,c("Totmass.init","Totmass.fina")])

#- calculate stem mass fraction, averaged across interval
dat3$SMF <- rowMeans(dat3[,c("Stemmass.init","Stemmass.fina")])/rowMeans(dat3[,c("Totmass.init","Totmass.fina")])

#- calculate root mass fraction, averaged across interval
dat3$RMF <- rowMeans(dat3[,c("Rootmass.init","Rootmass.fina")])/rowMeans(dat3[,c("Totmass.init","Totmass.fina")])


#- sort species by name, relevel factors
dat3$Species <- factor(as.character(dat3$Species))
#----------------------------------------------------------------------------------------------------




#----------------------------------------------------------------------------------------------------
#- aveage across species and treatments
dat3.m <- summaryBy(AGR+RGR+LAR+ULR+SLA+LMF+SMF+RMF~Species+Treatment,data=dat3,
                    FUN=c(mean,se),keep.names=F,na.rm=T)
#----------------------------------------------------------------------------------------------------


#----------------------------------------------------------------------------------------------------
#- a few simple plots
pdf("output/MEGA_growth_analysis_first_look_paired.pdf")
ptsize <- 1.5



#- define the color palette. note that this is increasingly problematic with lots of species
nspecies <- length(unique(dat3.m$Species))
rampcolors <- c("black","darkgrey","darkblue","blue","forestgreen","green","yellow","orange","red","violetred2")
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

# absolute growth rate
plotBy(AGR.mean~Treatment|Species,data=dat3.m,legendwhere="bottomleft",type="b",pch=16,ylim=c(-10,250),
       cex=ptsize,ylab="AGR (mg day-1)",legend=F,
       panel.first=adderrorbars(x=dat3.m$Treatment,y=dat3.m$AGR.mean,SE=dat3.m$AGR.se,direction="updown"))
legend(x=17,y=370,legend=levels(dat3.m$Species),col=palette()[1:nspecies],pch=16,lty=1,ncol=3,xpd=NA,cex=0.8,bty="n")

# relative growth rate
plotBy(RGR.mean~Treatment|Species,data=dat3.m,legendwhere="bottomleft",type="b",pch=16,legend=F,
       cex=ptsize,ylab="RGR (mg mg-1 day-1)",
       panel.first=adderrorbars(x=dat3.m$Treatment,y=dat3.m$RGR.mean,SE=dat3.m$RGR.se,direction="updown"))
abline(h=0)
legend(x=17,y=0.35,legend=levels(dat3.m$Species),col=palette()[1:nspecies],pch=16,lty=1,ncol=3,xpd=NA,cex=0.8,bty="n")

# leaf area ratio
plotBy(LAR.mean~Treatment|Species,data=dat3.m,legendwhere="bottomleft",type="b",pch=16,legend=F,
       cex=ptsize,ylab="LAR (cm2 mg-1)",
       panel.first=adderrorbars(x=dat3.m$Treatment,y=dat3.m$LAR.mean,SE=dat3.m$LAR.se,direction="updown"))
legend(x=17,y=0.25,legend=levels(dat3.m$Species),col=palette()[1:nspecies],pch=16,lty=1,ncol=3,xpd=NA,cex=0.8,bty="n")

# unit leaf rate
plotBy(ULR.mean~Treatment|Species,data=dat3.m,legendwhere="bottomleft",type="b",pch=16,legend=F,
       cex=ptsize,ylab="ULR (RGR/LAR)",
       panel.first=adderrorbars(x=dat3.m$Treatment,y=dat3.m$ULR.mean,SE=dat3.m$ULR.se,direction="updown"))
abline(h=0)
legend(x=17,y=4.5,legend=levels(dat3.m$Species),col=palette()[1:nspecies],pch=16,lty=1,ncol=3,xpd=NA,cex=0.8,bty="n")

# specific leaf area
plotBy(SLA.mean~Treatment|Species,data=dat3.m,legendwhere="bottomleft",type="b",pch=16,legend=F,
       cex=ptsize,ylab="SLA (cm2 mg-1)",
       panel.first=adderrorbars(x=dat3.m$Treatment,y=dat3.m$SLA.mean,SE=dat3.m$SLA.se,direction="updown"))
legend(x=17,y=0.45,legend=levels(dat3.m$Species),col=palette()[1:nspecies],pch=16,lty=1,ncol=3,xpd=NA,cex=0.8,bty="n")

# leaf mass fraction
plotBy(LMF.mean~Treatment|Species,data=dat3.m,legendwhere="topleft",type="b",pch=16,legend=F,
       cex=ptsize,ylab="LMF (mg mg-1)",
       panel.first=adderrorbars(x=dat3.m$Treatment,y=dat3.m$LMF.mean,SE=dat3.m$LMF.se,direction="updown"))
legend(x=17,y=0.9,legend=levels(dat3.m$Species),col=palette()[1:nspecies],pch=16,lty=1,ncol=3,xpd=NA,cex=0.8,bty="n")

# stem mass fraction
plotBy(SMF.mean~Treatment|Species,data=dat3.m,legendwhere="topleft",type="b",pch=16,legend=F,
       cex=ptsize,ylab="SMF (mg mg-1)",
       panel.first=adderrorbars(x=dat3.m$Treatment,y=dat3.m$SMF.mean,SE=dat3.m$SMF.se,direction="updown"))
legend(x=17,y=0.38,legend=levels(dat3.m$Species),col=palette()[1:nspecies],pch=16,lty=1,ncol=3,xpd=NA,cex=0.8,bty="n")

# mass fraction
plotBy(RMF.mean~Treatment|Species,data=dat3.m,legendwhere="topright",type="b",pch=16,legend=F,
       cex=ptsize,ylab="RMF (mg mg-1)",
       panel.first=adderrorbars(x=dat3.m$Treatment,y=dat3.m$RMF.mean,SE=dat3.m$RMF.se,direction="updown"))
legend(x=17,y=0.48,legend=levels(dat3.m$Species),col=palette()[1:nspecies],pch=16,lty=1,ncol=3,xpd=NA,cex=0.8,bty="n")
dev.off()
#----------------------------------------------------------------------------------------------------




#----------------------------------------------------------------------------------------------------
# RGR components
rampcolors <- c("blue","forestgreen","yellow","red")
palette(myColorRamp_raw(rampcolors,values=unique(as.numeric(dat3.m$Treatment))))

pdf("output/MEGA_RGRdecomposition_first_look_paired.pdf")

#- plot ULR vs. RGR
plot(ULR.mean~RGR.mean,col=as.factor(Treatment),data=dat3.m,pch=16,cex=0.5)
adderrorbars(x=dat3.m$RGR.mean,y=dat3.m$ULR.mean,SE=dat3.m$ULR.se,direction="updown",col=as.factor(dat3.m$Treatment))
adderrorbars(x=dat3.m$RGR.mean,y=dat3.m$ULR.mean,SE=dat3.m$RGR.se,direction="leftright",col=as.factor(dat3.m$Treatment))
points(ULR.mean~RGR.mean,col=as.factor(Treatment),data=dat3.m,pch=16,cex=1.5)
legend("topleft",pch=16,cex=0.8,legend=unique(as.numeric(dat3.m$Treatment)),col=palette()[1:6],title="Temperature")


#- plot LAR vs. RGR
plot(LAR.mean~RGR.mean,col=as.factor(Treatment),data=dat3.m,pch=16,cex=0.5)
adderrorbars(x=dat3.m$RGR.mean,y=dat3.m$LAR.mean,SE=dat3.m$LAR.se,direction="updown",col=as.factor(dat3.m$Treatment))
adderrorbars(x=dat3.m$RGR.mean,y=dat3.m$LAR.mean,SE=dat3.m$RGR.se,direction="leftright",col=as.factor(dat3.m$Treatment))
points(LAR.mean~RGR.mean,col=as.factor(Treatment),data=dat3.m,pch=16,cex=1.5)
legend("topleft",pch=16,cex=0.8,legend=unique(as.numeric(dat3.m$Treatment)),col=palette()[1:6],title="Temperature")


#- plot SLA vs. RGR
plot(SLA.mean~RGR.mean,col=as.factor(Treatment),data=dat3.m,pch=16,cex=0.5)
adderrorbars(x=dat3.m$RGR.mean,y=dat3.m$SLA.mean,SE=dat3.m$SLA.se,direction="updown",col=as.factor(dat3.m$Treatment))
adderrorbars(x=dat3.m$RGR.mean,y=dat3.m$SLA.mean,SE=dat3.m$RGR.se,direction="leftright",col=as.factor(dat3.m$Treatment))
points(SLA.mean~RGR.mean,col=as.factor(Treatment),data=dat3.m,pch=16,cex=1.5)
legend("topleft",pch=16,cex=0.8,legend=unique(as.numeric(dat3.m$Treatment)),col=palette()[1:6],title="Temperature")
dev.off()
#----------------------------------------------------------------------------------------------------
