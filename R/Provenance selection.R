#-------------------------------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------------------------------
#- Information about seed sources used in this study
#-------------------------------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------------------------------
seed<- read.csv("Data/Seed_20160919.csv")
seed1<- droplevels(subset(seed, Seedlot.Type == "Wild")) #wild sources only
seed1<- subset(seed1, Latitude !=0) #remove any that do not have coordinates

#only pick provs that have been tested since year 2006 or collected since 2000
seed1$Latest.Viability.Testdate<- as.Date(seed1$Latest.Viability.Testdate, format="%d-%b-%y")
seed1$Seedlot.Collected.Date<- as.Date(seed1$Seedlot.Collected.Date, format="%d-%b-%y")
seed1<- subset(seed1, Latest.Viability.Testdate > as.Date("2006-01-01")| Seedlot.Collected.Date > as.Date("2000-01-01"))

#sort proveances into difference geographical zones to assure we get a nice geographical spread
seed1$zone<- ifelse(seed1$Longitude<1300000 & seed1$Latitude<255000,1,
                    ifelse(seed1$Longitude<1300000 & seed1$Latitude>255000,4,
                           ifelse(seed1$Longitude>1300000&seed1$Longitude<1430000 & seed1$Latitude<255000,2,
                                  ifelse(seed1$Longitude>1300000&seed1$Longitude<1430000 & seed1$Latitude>255000,5,
                                         ifelse(seed1$Longitude>1430000 & seed1$Latitude<255000,3,
                                                ifelse(seed1$Latitude > 400000,7,6))))))
seed1$ssp<- as.factor(paste(seed1$Genus, seed1$Species, sep=" "))

#remove hybrids
seed2<- seed1[!(seed1$ssp %in% c("Eucalyptus camaldulensis ig. 'camaltereti'","Eucalyptus saligna ig. botryoides",
                                 "Eucalyptus tereticornis ig. 'tereticamal'")),]


#plot all provs on map - some areas do not have great coverage, e.g.the great victoria desert area. 
#But that is understandable
library(mapdata)
library(maps)
seed2$Lat<-seed2$Latitude/-10000
seed2$Lon<-seed2$Longitude/10000
map("worldHires","Australia", xlim=c(113,154), ylim=c(-45,-9), col="black", fill=F)
with(subset(seed2, zone==1), points(Lon, Lat, pch=19, col="red"))
with(subset(seed2, zone==2), points(Lon, Lat, pch=19, col="orange"))
with(subset(seed2, zone==3), points(Lon, Lat, pch=19, col="yellow"))
with(subset(seed2, zone==4), points(Lon, Lat, pch=19, col="green"))
with(subset(seed2, zone==5), points(Lon, Lat, pch=19, col="cyan"))
with(subset(seed2, zone==6), points(Lon, Lat, pch=19, col="pink"))
with(subset(seed2, zone==7), points(Lon, Lat, pch=19, col="grey"))
abline(v=c(130, 143), h=c(-25.5, -40))
box()

#remove camaldulensis and add previously randomly selected proveances of E camal
Ecamal<- subset(seed2, Seedlot.Number %in% c(15440, 20715,20903,19913))
seed3<- seed2[!(seed2$Genus_species == "Eucalyptus camaldulensis"),]
seed3<- rbind(seed3,Ecamal)

#plot all provs on map - many of the above provenances were camaldulensis 
#still a decent geographical spread though
library(mapdata)
library(maps)
seed3$Lat<-seed3$Latitude/-10000
seed3$Lon<-seed3$Longitude/10000
map("worldHires","Australia", xlim=c(113,154), ylim=c(-45,-9), col="black", fill=F)
with(subset(seed3, zone==1), points(Lon, Lat, pch=19, col="red"))
with(subset(seed3, zone==2), points(Lon, Lat, pch=19, col="orange"))
with(subset(seed3, zone==3), points(Lon, Lat, pch=19, col="yellow"))
with(subset(seed3, zone==4), points(Lon, Lat, pch=19, col="green"))
with(subset(seed3, zone==5), points(Lon, Lat, pch=19, col="cyan"))
with(subset(seed3, zone==6), points(Lon, Lat, pch=19, col="pink"))
with(subset(seed3, zone==7), points(Lon, Lat, pch=19, col="grey"))
abline(v=c(130, 143), h=c(-25.5, -40))
box()

#manually choose provenances in each zone 

#zone1
zone1<- subset(seed3, zone==1)
zone1.provs<- subset(seed3, zone==1 & Seedlot.Number%in% c(
  #17293,# Corymbia cadophora  - failed to germinate    
  14058,# Corymbia eremaea       
  #15781,# Eucalyptus intertexta  
  #14047,# Eucalyptus oxymitra    
  13785,# Eucalyptus pachyphylla
  #15782, # Eucalyptus socialis - failed to germinate
  20715, #camaldulensis obtusa
  15440 #camaldulensis subcinerea
))
#zone2
zone2<- subset(seed3, zone==2)
zone2.provs<- subset(seed3, zone==2 & (Seedlot.Number%in% c(
  #21240,# Eucalyptus brassiana                
  18009, # Eucalyptus herbertiana
  #17493,# Eucalyptus brevifolia 
  16883,# Eucalyptus nudicaulis
  #17484,# Eucalyptus pachyphylla 
  #17482,# Eucalyptus normantonensis
  #17085,# Eucalyptus socialis 
  #14040,# Corymbia chippendalei              
  #17242,# Eucalyptus gamophylla - failed to germinate              
  17377# Eucalyptus gillenii                 
  #17079,# Eucalyptus gongylocarpa         
  #17088# Eucalyptus trivalvis
  #17475,# Eucalyptus orbifolia               
  #17472,# Eucalyptus sessilis                
  #17303,# Eucalyptus lucens                   
  #14023 # Eucalyptus mannensis ssp. mannensis
  #21360# Eucalyptus coolabah                 
  #14037,# Corymbia eremaea
  #17081,# Eucalyptus intertexta
  #17476,# Eucalyptus oxymitra
)))

#zone3
zone3<- subset(seed3, zone==3)
zone3.provs<- subset(seed3, zone==3 & (Seedlot.Number%in% c(
  #20856,#brassiana
  21026,#pellita
  20653,#tessellaris
  #21164,#torelliana
  16881,#dimorpha
  21344,#citriodora ssp. citriodora
  #19665,#citriodora ssp. variegata
  #21185,#cloeziana
  #20902,#chlorophylla
  #17397,#exserta - failed to germinate
  #15942,#hallii  - failed to germinate
  #17821,#hylandii
  #21346,#intermedia
  #21253,#longirostrata
  #20491, #major
  #20456,#microcarpa 
  #20771, #moluccana
  #19158, #phoenicea - failed to germinate
  #20492,#propinqua
  #15945,#robusta 
  #20011,#saligna
  #16882,#sicilifolia   
  #20904,#staigeriana
  #16645, #tereticornis - failed to germinate
  #20480 #thozetiana
  #16884# xanthope
  #18569,#grandis 
  20903 #camaldulensis simulata
)))

#zone4
zone4<- subset(seed3, zone==4)
zone4.provs<- subset(seed3, zone==4 & (Seedlot.Number%in% c(
  #18334,# brevistylis - failed to germinate
  14081,# concinna
  15779,# striaticalyx
  #20841,# horistes
  #19430,# salubris
  #15812,# incrassata
  #19429,# todtiana
  17987,# salmonophloia
  #19926,# occidentalis  - failed to germinate
  #15380,# kondininensis
  19413,# melanoxylon
  #12270,# jucunda
  #17969# acies
  #16530,# aequioperta
  #19437# astringens
  #17974,# caesiassp.magna
  #15802,# capillosassp.capillosa
  #17982, # cerasiformis
  #19415, # cernua
  #19408,# clivicola
  #19418,# cornuta
  #19416,# densa ssp.densa
  #19406,# dielsii
  #19405,#diptera
  #19405,# dolichorhyncha
  #17672,# falcata
  #17980,# georgei
  #16462,# halophila
  #17977,# incerata
  #16461,# indurata
  #18333# jacksonii
  #16456,# kumarlensis
  #16391,# leptocalyx
  #17979,# longicornis
  #17691,# macrocarpa ssp.macrocarpa
  #17989# megacornuta
  #17976,# ornata
  #18335,# patens
  #16280,# phaenophylla
  #19434,# platypusvar.platypus
  #19428,# pluricaulis
  #15402,# quadrans
  #17996,# redacta
  #15460,# rigidula
  19099# rudis
  #15404# salicola
  #15448,# sargentii
  #15817,# sheathiana
  #17978,# steedmanii
  #19407,# stoatei
  #19436,# suggrandisssp.alipes
  #17972,# suggrandisssp.suggrandis
  #17990,# tetraptera
  #15421# yilgarnensis
)))

#zone5
zone5<- subset(seed3, zone==5)
zone5.provs<- subset(seed3, zone==5 & (Seedlot.Number%in% c(
  #15208,# diversifolia
  20272,# cosmophylla
  15897,# obliqua
  #21357,# coolabah
  #20388,# cladocalyx
  20271,# fasciculosa
  #19864,# globulus ssp. bicostata
  #20554,# largiflorens
  #16020 # viminalis ssp. cygnetensis - failed to germinate
  #20270,# macrorhyncha ssp. macrorhyncha
  #21358,# melliodora
)))

#zone6
zone6<- subset(seed3, zone==6)
zone6.provs<- subset(seed3, zone==6 & (Seedlot.Number%in% c(
  #17269,# curtisii  - failed to germinate
  15607,# microcorys
  #17149,# tenuipes  - failed to germinate
  19318,# pilularis
  #18349,# michaeliana
  #19804,# volcanica
  #21150,# pauciflora ssp. niphophila - failed to germinate
  21359,# melliodora  
  #19786,# macrorhyncha ssp. macrorhyncha   - failed to germinate
  16099,# cypellocarpa
  #19283,#globulus ssp. bicostata
  17911,# dunnii
  18700,# grandis
  #18164,# nitens  
  19801,# nobilis 
  #20499,# propinqua
  20834,# saligna 
  20490,# sideroxylon 
  #19131,# sieberi 
  #13953,# resinifera 
  15937,# robusta 
  #15606,# acmenoides    
  #20692,# agglomerata
  #18303,# aggregata
  #15279,# amplifolia var. amplifolia
  #18730,# amplifolia var. sessiliflora
  #16833,# andrewsii ssp. campanulata 
  #16009,# apothalassica 
  #16841,# approximans
  #15994,# arenacea 
  19605,# badjensis  
  #17505,# bakeri  
  #15995,# baxteri  
  #20406, # punctata  - failed to germinate
  #21144,# bosistoana 
  #19351, # botryoides  
  #18317, # brookeriana
  #20496,# caliginosa 
  #20497,# cameronii
  #18711, # camphora ssp. camphora
  #19664, # Corymbia citriodora ssp. variegata
  #20962, # cloeziana
  12812, # coolabah
  #20317,# dawsonii
  #18733,# deanei  
  #20410,# dives 
  #21349,# dorrigoensis  
  #19366,# elata 
  #15309,# fraxinoides
  #19364,# froggattii 
  #19695,# glaucescens 
  #20690,# globoidea  
  #17609,# globulus ssp. globulus  
  #20230, # globulus ssp. maidenii 
  #18111,# globulus ssp. pseudoglobulus 
  #21329,# infera  1 tree
  #14447,# kitsoniana 
  #20155,# kybeanensis 
  #15297,# longifolia 
  #21251,# longirostrata 
  #20897,# macarthurii 
  #20599,# Corymbia maculata  
  #15603,# major 
  #16098,# mannifera ssp. mannifera  
  #16517,# mannifera ssp. praecox 
  #16029,# microcarpa 
  #15306,# muelleriana
  #15344,# nortonii
  #15900, # obliqua 
  #19101,# paniculata  
  #21137,# pauciflora ssp. pauciflora
  #20680,# polybractea
  #16512,# pryoriana
  #15919,# pyrocarpa  
  #19367,# quadrangulata 
  #21367,# radiata ssp. radiata 
  #15152,# regnans 
  #15617,# rubida ssp. rubida  
  #20137,# smithii 
  #20391,# stenostoma  
  #17768,# tereticornis ssp. tereticornis  
  #20447,# tricarpa 
  #15211,# viminalis ssp. viminalis  
  #17003,# Corymbia watsoniana 
  #20511,# williamsiana  
  #20765# youmanii
  19913#camaldulensis camaldulensis
)))

#zone7
zone7<- subset(seed3, zone==7)
zone7.provs<- subset(seed3, zone==7 & (Seedlot.Number%in% c(
  18673,# Eucalyptus globulus ssp. globulus  
  #16469,# Eucalyptus johnstonii   - failed to germinate              
  15172# Eucalyptus regnans                  
  #18399# Eucalyptus viminalis ssp. viminalis
  #15907,# Eucalyptus obliqua
)))

#51 selected provenances - 36 successfully completed
provenances<- rbind(zone1.provs,zone2.provs,zone3.provs,zone4.provs,zone5.provs,zone6.provs,zone7.provs)
sum(table(provenances$zone))


#get range sizes and phylogenetic info on selected species
phyl<- read.csv("Data/phylogeny-range-2016-09-19.csv")
phyl2<- phyl[,c(1:10,12)]
names(phyl2)[1]<- "Scientific.name"

seeds<- merge(seed3,phyl2, by="Scientific.name")
species<- as.character(droplevels(unique(provenances$ssp)))

sel_sp<-seeds[seeds$ssp %in% species,]

hist(unique(sel_sp$range),breaks=10, main="36 species", xlab="Range") #slightly oversamples mid and wide ranged provs
hist(unique(seeds$range),breaks=10, main="all available species", xlab = "Range") #histogram of range sizes in all avaliable species at the Seed centre

#plot selected provenances on map 
library(mapdata)
library(maps)
seed2$Lat<-seed2$Latitude/-10000
seed2$Lon<-seed2$Longitude/10000
map("worldHires","Australia", xlim=c(113,154), ylim=c(-45,-9), col="black", fill=F)
with(seed2, points(Lon, Lat, pch=19, col="grey"))
with(provenances, points(Lon, Lat, pch=19, col="black"))
box()
mtext("Selected provenances", side=3)

#-------------------------------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------------------------------

#extract environmental variables for provenances
library(raster)
library(rgdal)
library(stringr)
library(scales)
library(oz)
library(maps)
library(mapdata)
library(RNCEP)
library(mapplots)
library(plotrix)
library(dismo)
library(rgeos)

#change coords for E. cosmophylla - it is in the middle of the water
provenances[provenances$Seedlot.Number==20272,c("Lat")] <- -35.93
provenances[provenances$Seedlot.Number==20272,c("Lon")]<- 136.63
biodat <- getData("worldclim", var="bio", res=2.5, path="C:/Users/30034792/OneDrive/T data")
biodat1<-subset(biodat,1) #mean annual T
biodat2<-subset(biodat,2) #mean diurnal range
biodat3<-subset(biodat,3) #Isothermality 
biodat4<-subset(biodat,4) #Temperature Seasonality
biodat5 <- subset(biodat,5)#max T of warmest month
biodat6<-subset(biodat,6) #min T of coldest month
biodat7<-subset(biodat,7) #Temperature Annual Range
biodat8<-subset(biodat,8) #Mean Temperature of Wettest Quarter
biodat9<-subset(biodat,9) #Mean Temperature of Driest Quarter
biodat10 <- subset(biodat,10) #mean T warmest quarter
biodat11 <- subset(biodat,11) #mean T coldest quarter
biodat12 <- subset(biodat,12) #Annual Precipitation
biodat13 <- subset(biodat,13) #Precipitation of Wettest Month
biodat14 <- subset(biodat,14) #Precipitation of Driest Month
biodat15 <- subset(biodat,15) #Precipitation Seasonality 
biodat16 <- subset(biodat,16) #Precipitation of Wettest Quarter
biodat17 <- subset(biodat,17) #Precipitation of Driest Quarter
biodat18 <- subset(biodat,18) #Precipitation of Warmest Quarter
biodat19 <- subset(biodat,19) #Precipitation of Coldest Quarter

YbrevRange <- extent(100.00, 155.00, -50, -8)
biodat.oz1 <- crop(biodat1,YbrevRange)
biodat.oz2 <- crop(biodat2,YbrevRange)
biodat.oz3 <- crop(biodat3,YbrevRange)
biodat.oz4 <- crop(biodat4,YbrevRange)
biodat.oz5 <- crop(biodat5,YbrevRange)
biodat.oz6 <- crop(biodat6,YbrevRange)
biodat.oz7 <- crop(biodat7,YbrevRange)
biodat.oz8 <- crop(biodat8,YbrevRange)
biodat.oz9 <- crop(biodat9,YbrevRange)
biodat.oz10 <- crop(biodat10,YbrevRange)
biodat.oz11 <- crop(biodat11,YbrevRange)
biodat.oz12 <- crop(biodat12,YbrevRange)
biodat.oz13<- crop(biodat13,YbrevRange)
biodat.oz14 <- crop(biodat14,YbrevRange)
biodat.oz15 <- crop(biodat15,YbrevRange)
biodat.oz16 <- crop(biodat16,YbrevRange)
biodat.oz17 <- crop(biodat17,YbrevRange)
biodat.oz18 <- crop(biodat18,YbrevRange)
biodat.oz19 <- crop(biodat19,YbrevRange)

xy <- SpatialPoints(cbind(provenances$Lon,provenances$Lat))
provenances$bio1 <- extract(biodat.oz1/10,xy,method="bilinear",fun=mean, buffer=15000) 
provenances$bio2 <- extract(biodat.oz2/10,xy,method="bilinear",fun=mean, buffer=15000) 
provenances$bio3 <- extract(biodat.oz3/10,xy,method="bilinear",fun=mean, buffer=15000) 
provenances$bio4 <- extract(biodat.oz4/10,xy,method="bilinear",fun=mean, buffer=15000) 
provenances$bio5 <- extract(biodat.oz5/10,xy,method="bilinear",fun=mean, buffer=15000)
provenances$bio6 <- extract(biodat.oz6/10,xy,method="bilinear",fun=mean, buffer=15000)
provenances$bio7 <- extract(biodat.oz7/10,xy,method="bilinear",fun=mean, buffer=15000) 
provenances$bio8 <- extract(biodat.oz8/10,xy,method="bilinear",fun=mean, buffer=15000) 
provenances$bio9 <- extract(biodat.oz9/10,xy,method="bilinear",fun=mean, buffer=15000) 
provenances$bio10 <- extract(biodat.oz10/10,xy,method="bilinear",fun=mean, buffer=15000)
provenances$bio11 <- extract(biodat.oz11/10,xy,method="bilinear",fun=mean, buffer=15000)
provenances$bio12 <- extract(biodat.oz12,xy,method="bilinear",fun=mean, buffer=15000) 
provenances$bio13 <- extract(biodat.oz13,xy,method="bilinear",fun=mean, buffer=15000) 
provenances$bio14 <- extract(biodat.oz14,xy,method="bilinear",fun=mean, buffer=15000) 
provenances$bio15 <- extract(biodat.oz15,xy,method="bilinear",fun=mean, buffer=15000) 
provenances$bio16 <- extract(biodat.oz16,xy,method="bilinear",fun=mean, buffer=15000) 
provenances$bio17 <- extract(biodat.oz17,xy,method="bilinear",fun=mean, buffer=15000) 
provenances$bio18 <- extract(biodat.oz18,xy,method="bilinear",fun=mean, buffer=15000) 
provenances$bio19 <- extract(biodat.oz19,xy,method="bilinear",fun=mean, buffer=15000) 

XY <- SpatialPoints(cbind(seed2$Longitude/10000,seed2$Latitude/-10000))
seed2$bio1 <- extract(biodat.oz1/10,XY,method="bilinear",fun=mean, buffer=15000) 
seed2$bio2 <- extract(biodat.oz2/10,XY,method="bilinear",fun=mean, buffer=15000) 
seed2$bio3 <- extract(biodat.oz3/10,XY,method="bilinear",fun=mean, buffer=15000) 
seed2$bio4 <- extract(biodat.oz4/10,XY,method="bilinear",fun=mean, buffer=15000) 
seed2$bio5 <- extract(biodat.oz5/10,XY,method="bilinear",fun=mean, buffer=15000)
seed2$bio6 <- extract(biodat.oz6/10,XY,method="bilinear",fun=mean, buffer=15000)
seed2$bio7 <- extract(biodat.oz7/10,XY,method="bilinear",fun=mean, buffer=15000) 
seed2$bio8 <- extract(biodat.oz8/10,XY,method="bilinear",fun=mean, buffer=15000) 
seed2$bio9 <- extract(biodat.oz9/10,XY,method="bilinear",fun=mean, buffer=15000) 
seed2$bio10 <- extract(biodat.oz10/10,XY,method="bilinear",fun=mean, buffer=15000)
seed2$bio11 <- extract(biodat.oz11/10,XY,method="bilinear",fun=mean, buffer=15000)
seed2$bio12 <- extract(biodat.oz12,XY,method="bilinear",fun=mean, buffer=15000) 
seed2$bio13 <- extract(biodat.oz13,XY,method="bilinear",fun=mean, buffer=15000) 
seed2$bio14 <- extract(biodat.oz14,XY,method="bilinear",fun=mean, buffer=15000) 
seed2$bio15 <- extract(biodat.oz15,XY,method="bilinear",fun=mean, buffer=15000) 
seed2$bio16 <- extract(biodat.oz16,XY,method="bilinear",fun=mean, buffer=15000) 
seed2$bio17 <- extract(biodat.oz17,XY,method="bilinear",fun=mean, buffer=15000) 
seed2$bio18 <- extract(biodat.oz18,XY,method="bilinear",fun=mean, buffer=15000) 
seed2$bio19 <- extract(biodat.oz19,XY,method="bilinear",fun=mean, buffer=15000) 


write.csv(provenances,file="Data/GHS39_MEGA_SEEDINFO.bioclim-20170602.csv")

plot(seed2$bio1~seed2$bio12, data=seed2, ylab="MAT", xlab="MAP", pch=20, col="grey", xlim=c(50,2500), ylim=c(9,30))
points(provenances$bio1~provenances$bio12,data=provenances, ylab="MAT", xlab="MAP", pch=20, col="black", cex=2)
legend("topright",legend=c("All available","36 completed"), pch=c(20,20), col=c("grey","black"), pt.cex= c(1,2))

hist(provenances$bio12)
hist(provenances$bio1)

plot(seed2$bio5~seed2$bio14, data=seed2, ylab="Tmax warmest month", xlab="Prec driest month", pch=20, col="grey", xlim=c(0,70), ylim=c(20,42))
points(provenances$bio5~provenances$bio14,data=provenances, ylab="MAT", xlab="MAP", pch=20, col="black", cex=2)
legend("topright",legend=c("All available","36 completed"), pch=c(20,20), col=c("grey","black"), pt.cex= c(1,2))

#MAT~Range
sel<- merge(provenances,phyl2, by="Scientific.name")

seeds<- merge(seed2,phyl2, by="Scientific.name")
seeds$ssp<- as.factor(paste(seeds$Genus, seeds$Species, sep=" "))

sppoint <- SpatialPoints(cbind(seeds$Longitude/10000,seeds$Latitude/-10000))
seeds$bio1 <- extract(biodat.oz1/10,sppoint,method="bilinear",fun=mean, buffer=15000) 
seeds$bio12 <- extract(biodat.oz12,sppoint,method="bilinear",fun=mean, buffer=15000) 

plot(bio1~log10(range), data=seeds, ylab="MAT", xlab="log10(Range Size)", pch=20, col="grey", xlim=c(3,max(log10(seeds$range),na.rm=T)), ylim=c(7,30))
points(bio1~log10(range), data=sel, ylab="", xlab="", pch=20, col="black", cex=2)
legend("toplef",legend=c("All available","36 completed"), pch=c(20,20), col=c("grey","black"), pt.cex= c(1,2))

plot(bio12~log10(range), data=seeds, ylab="MAP", xlab="log10(Range Size)", pch=20, col="grey", xlim=c(3,max(log10(seeds$range),na.rm=T)), ylim=c(7,2500))
points(bio12~log10(range), data=sel, ylab="", xlab="", pch=20, col="black", cex=2)
legend("topright",legend=c("All available","36 completed"), pch=c(20,20), col=c("grey","black"), pt.cex= c(1,2))

sel2<-sel[,-c(3:6,21:25)]
write.csv(sel2,file="Data/GHS39_MEGA_SEEDINFO.bioclim2-20170602.csv")
