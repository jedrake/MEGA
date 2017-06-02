#Collate and get climate data from s39 files
library(HIEv)
#- read in the "fast" datasets (airT, RH, and PAR)

fastfiles <- list.files(path="Climate/2017-02-17_s39_climate/",pattern="fast",full.names=T)
dat <- list()
for(i in 1:length(fastfiles)){
  #- read in the data
  dat[[i]] <- readTOA5(fastfiles[i])
  
  #- extract the room number from the filename
  name <- tolower(fastfiles[i])
  dat[[i]]$bay <- as.factor(substr(str_extract(name,pattern="room[0-9]"),start=5,stop=5))
}
dat.fast.17feb <- do.call(rbind,dat)

fastfiles <- list.files(path="Climate/2017-02-20_s39_climate/",pattern="fast",full.names=T)
dat <- list()
for(i in 1:length(fastfiles)){
  #- read in the data
  dat[[i]] <- readTOA5(fastfiles[i])
  
  #- extract the room number from the filename
  name <- tolower(fastfiles[i])
  dat[[i]]$bay <- as.factor(substr(str_extract(name,pattern="room[0-9]"),start=5,stop=5))
}
dat.fast.20feb <- do.call(rbind,dat)

fastfiles <- list.files(path="Climate/2017-03-16_s39_climate/",pattern="fast",full.names=T)
dat <- list()
for(i in 1:length(fastfiles)){
  #- read in the data
  dat[[i]] <- readTOA5(fastfiles[i])
  
  #- extract the room number from the filename
  name <- tolower(fastfiles[i])
  dat[[i]]$bay <- as.factor(substr(str_extract(name,pattern="room[0-9]"),start=5,stop=5))
}
dat.fast.16mar <- do.call(rbind,dat) #no room 6


fastfiles <- list.files(path="Climate/2017-03-27_s39_climate/",pattern="fast",full.names=T)
dat <- list()
for(i in 1:length(fastfiles)){
  #- read in the data
  dat[[i]] <- readTOA5(fastfiles[i])
  
  #- extract the room number from the filename
  name <- tolower(fastfiles[i])
  dat[[i]]$bay <- as.factor(substr(str_extract(name,pattern="room[0-9]"),start=5,stop=5))
}
dat.fast.27mar <- do.call(rbind,dat)

fastfiles <- list.files(path="Climate/2017-04-07_s39_climate/",pattern="fast",full.names=T)
dat <- list()
for(i in 1:length(fastfiles)){
  #- read in the data
  dat[[i]] <- readTOA5(fastfiles[i])
  
  #- extract the room number from the filename
  name <- tolower(fastfiles[i])
  dat[[i]]$bay <- as.factor(substr(str_extract(name,pattern="room[0-9]"),start=5,stop=5))
}
dat.fast.7apr <- do.call(rbind,dat)

fastfiles <- list.files(path="Climate/2017-05-02_s39_climate/",pattern="fast",full.names=T)
dat <- list()
for(i in 1:length(fastfiles)){
  #- read in the data
  dat[[i]] <- readTOA5(fastfiles[i])
  
  #- extract the room number from the filename
  name <- tolower(fastfiles[i])
  dat[[i]]$bay <- as.factor(substr(str_extract(name,pattern="room[0-9]"),start=5,stop=5))
}
dat.fast.2may <- do.call(rbind,dat)

fastfiles <- list.files(path="Climate/MEGA_2017_climate/",pattern="fast",full.names=T)
dat <- list()
for(i in 1:length(fastfiles)){
  #- read in the data
  dat[[i]] <- readTOA5(fastfiles[i])
  
  #- extract the room number from the filename
  name <- tolower(fastfiles[i])
  dat[[i]]$bay <- as.factor(substr(str_extract(name,pattern="room[0-9]"),start=5,stop=5))
}
dat.fast.26may <- do.call(rbind,dat)

#- subset to data just for our experiment.
feb17<- subset(dat.fast.17feb, Date >= as.Date("2017-02-14"))
feb20<- subset(dat.fast.20feb, Date >= as.Date("2017-02-14"))
mar16<- subset(dat.fast.16mar, Date >= as.Date("2017-02-14"))
mar27<- subset(dat.fast.27mar, Date >= as.Date("2017-02-14"))
apr7<- subset(dat.fast.7apr, Date >= as.Date("2017-02-14"))
may2<- subset(dat.fast.2may, Date >= as.Date("2017-02-14"))
may26<- subset(dat.fast.26may, Date >= as.Date("2017-02-14"))

xtabs(~Date+bay,data=feb17)
xtabs(~Date+bay,data=feb20)
xtabs(~Date+bay,data=mar16) #for some reason the data up to 2017-03-03 has disappeared in bay 1???
xtabs(~Date+bay,data=mar27)
xtabs(~Date+bay,data=may2)  #again data has disappeared in bay 1 up to 2017-04-18
xtabs(~Date+bay,data=may26) 

megaclim<- droplevels(subset(may26, bay %in% c(2:4,6,7))) #rooms 2 to 4 and 6 and 7 all ok

r1early<-droplevels(subset(feb20, bay==1))#2017-02-14 to 2017-02-20
r1mid<-droplevels(subset(mar27,Date >as.Date("2017-02-14")& bay==1)) #2017-03-03 to 03-27
r1late<- droplevels(subset(may26, Date >as.Date("2017-02-14")& bay==1)) #2017-04-18 to 05-26

mega<-rbind(r1early,megaclim)
mega2<-rbind(r1mid, mega)
mega3<-rbind(r1late, mega2)

mega3$Treatment<- ifelse(mega3$bay == 1, 32,
                         ifelse(mega3$bay==2, 21.5,
                                ifelse(mega3$bay==3 & mega3$DateTime < as.POSIXct("2017-02-25 12:30:00"),28.5,
                                       ifelse(mega3$bay == 3  & mega3$DateTime > as.POSIXct("2017-02-25 12:30:00"),18,
                                              ifelse( mega3$bay == 4 & mega3$DateTime < as.POSIXct("2017-02-21 08:00:00"),35.5,
                                                      ifelse(mega3$bay==4 & mega3$DateTime > as.POSIXct("2017-02-21 08:00:00") & mega3$DateTime < as.POSIXct("2017-02-25 12:30:00"),18,
                                                             ifelse(mega3$bay==4 & mega3$DateTime > as.POSIXct("2017-02-25 12:30:00") & mega3$DateTime < as.POSIXct("2017-03-03 15:30:00"),28.5,
                                                                    ifelse(mega3$bay==4 & mega3$DateTime > as.POSIXct("2017-03-03 15:30:00"),25,
                                                                           ifelse(mega3$bay==6 & mega3$DateTime < as.POSIXct("2017-02-21 08:00:00"),18,
                                                                                  ifelse(mega3$bay==6 & mega3$DateTime > as.POSIXct("2017-02-21 08:00:00"),35.5,
                                                                                         ifelse(mega3$bay==7 & mega3$DateTime < as.POSIXct("2017-03-03 15:30:00"),25,28.5)))))))))))


xtabs(~Date+bay,data=mega3)

names(mega3)<- c("DateTime","RECORD","BattV_Min","Tair","RH","PAR","Date","Source","Bay","Treatment")
write.csv(mega3,"GHS39_MEGA_MET-AIR_20170214-20170526_R.csv")

#- hourly averages
climatemega$DateTime_hr <- as.POSIXct(round.POSIXt(climatemega$DateTime,units="hours"))


#- create hourly averages
dat.fast.hr <- dplyr::summarize(group_by(climatemega,DateTime_hr,Treatment),
                                BattV=mean(BattV,na.rm=T),
                                Tair=mean(Tair,na.rm=T),
                                RH=mean(RH,na.rm=T),
                                PAR=mean(PAR,na.rm=T))
dat.fast.hr <- as.data.frame(dat.fast.hr)
dat.fast.hr$Date <- as.Date(dat.fast.hr$DateTime_hr)

dat.fast.day <- dplyr::summarize(group_by(dat.fast.hr,Date,Treatment),
                                 PARsum = sum(PAR),
                                 PARmax = max(PAR,na.rm=T),
                                 Tair=mean(Tair,na.rm=T),
                                 RH=mean(RH,na.rm=T))
dat.fast.day <- as.data.frame(dat.fast.day)
dat.fast.day <- subset(dat.fast.day,Date>as.Date("2017-02-20") & Date < as.Date("2017-05-26"))
dat.fast.day$PARsum_mol <- dat.fast.day$PARsum*60*60*1e-6
dat.fast.day$Treatment<- as.factor(dat.fast.day$Treatment)

colors <- c("black","grey","red","forestgreen","purple", "orange")
windows(40,30)
par(mfrow=c(3,1), mar=c(0.3,2,0.3,0.8), oma=c(5,6,6,2.5))
size=1.5
# plot airT
plotBy(Tair~Date|Treatment,data=dat.fast.day,pch=15,type="b",legend=F,axes=F,col=colors,cex=size, ylim=c(15,38))
magaxis(side=c(2,4),labels=c(1,0),box=T,las=1,cex.axis=1.5)
axis.Date(side=1,at=seq(from=min(dat.fast.day$Date),max(dat.fast.day$Date),by="day"),labels=F,las=2,cex.axis=1.5,tcl=0.5)
mtext(text="Mean air T (deg C)",side=2,outer=F,line=5,cex=1.2)
legend(x=as.Date("2017-03-03"),y=42,xpd=NA,legend=levels(dat.fast.day$Treatment),
       col=colors,pch=15,ncol=6,cex=1.5,title="Room", bty="n")
# plot RH
plotBy(RH~Date|Treatment,data=dat.fast.day,pch=15,type="b",legend=F,axes=F,col=colors,cex=size, ylim=c(60,100))
magaxis(side=c(2,4),labels=c(1,0),box=T,las=1,cex.axis=1.5)
axis.Date(side=1,at=seq(from=min(dat.fast.day$Date),max(dat.fast.day$Date),by="day"),labels=F,las=2,cex.axis=1.5,tcl=0.5)
mtext(text="Mean RH (%)",side=2,outer=F,line=5,cex=1.2)

# plot PAR
plotBy(PARsum~Date|Treatment,data=dat.fast.day,pch=15,type="b",legend=F,axes=F,col=colors,cex=size)
magaxis(side=c(2,4),labels=c(1,0),box=T,las=1,cex.axis=1.5)
axis.Date(side=1,at=seq(from=min(dat.fast.day$Date),max(dat.fast.day$Date),by="day"),labels=T,las=2,cex.axis=1.5,tcl=0.5)
mtext(text="Sum PAR (umol m-2)",side=2,outer=F,line=5,cex=1.2)

# ######################################################################################################
# #linear PAR
# linpar<-read.csv("data/GHS39_MEGA_MET-LINPAR_20170217-20170526_R.csv", skip=19)
# linpar$DateTime<-as.POSIXct(paste(linpar$Date,linpar$Time, sep=' '), format="%d/%m/%Y %T")
# names(linpar)<-c("Date","Time","Chan1.PAR","Chan2.PAR","Chan3.PAR","Chan1.mV.","Chan2.mV.","Chan3.mV.","Int.Batt.V","Int.Batt.T","Ext.pow","Ext.pow.V","Ext.pow.ma","Diag","DateTime") 
# 
# library(tidyr)
# Linpar<- gather(linpar[,c(-6:-8)], Chan, PAR, Chan1.PAR:Chan3.PAR, factor_key=TRUE)
# 
# Linpar$Chan<- as.factor(Linpar$Chan)
# Linpar$PAR<- as.numeric(Linpar$PAR)
# Linpar$Channel<- as.factor(substr(Linpar$Chan,start=1,stop=5))
# 
# LINpar<-Linpar[,c(3:9, 11:12)]
# 
# write.csv(LINpar,"GHS39_MEGA_MET-LINPAR_20170215-20170526_L1.csv")
# 
# 
# #calibration test at the end
# calend<- subset(Linpar,DateTime > as.POSIXct("2017-05-26 15:20:00") & DateTime < as.POSIXct("2017-05-26 16:10:00"))
# 
# #channel 3 is way higher than the other two, do not use
# plotBy(PAR~DateTime, data=subset(calend,Chan=="Chan1.PAR"),type = "l", ylim=c(400,1000))
# lines(PAR~DateTime, data=subset(calend,Chan=="Chan2.PAR"),type = "l", col="red")
# lines(PAR~DateTime, data=subset(calend,Chan=="Chan3.PAR"),type = "l", col="green") #channel 3 is way higher than the other two
# 
# m1<-lm(PAR~date_num*chan, data=cal_long)
# summary(m1)
# 
# windows(10,10)
# plotBy(PAR~date_num|chan, data=cal_long)
# abline(lm(PAR~date_num, data=subset(cal_long, chan=="Chan1.PAR")))
# abline(lm(PAR~date_num, data=subset(cal_long, chan=="Chan2.PAR")), col="red")
# abline(lm(PAR~date_num, data=subset(cal_long, chan=="Chan3.PAR")), col="green")
# 
