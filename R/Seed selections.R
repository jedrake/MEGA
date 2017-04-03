#-------------------------------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------------------------------
#- This script tries to make sense of the Australian Tree Seed Centre's seed data for Eucs
#-------------------------------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------------------------------





#-------------------------------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------------------------------
#- read in the location ID's, plot on map
seeds <- read.csv("C:/Repos/MEGA/data/Eucalypts-Corymbia-TreeSeedStocks-2016-09-19_ATSC.csv")
seeds$Lat <- seeds$Latitude/-10000
seeds$Long <- seeds$Longitude/10000


library(rworldmap)
newmap <- getMap(resolution = "low")
plotoz <- function(){
  #windows()
  plot(newmap,
       xlim = c(105, 155),
         ylim = c(-43, -10),
         asp = 1)
}
points(x=seeds$Long,y=seeds$Lat,pch=1,col="black",cex=1)

#-------------------------------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------------------------------
















#-------------------------------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------------------------------
#- read in the taxanomic information
taxo <- read.csv("C:/Repos/MEGA/data/Eucalypts-Corymbia-TreeSeedStocks-2016-09-19_taxonomy.csv")
names(taxo)[1] <- "Genus_species"
taxo[,c(2,3,4)] <- NULL

#- we have a 5 Blakella, 11 Corymbia, 38 Eucalyptus, 4 Eudesmia, and 156 Symphyomyrtus
table(taxo$Subgenus)

#- of Symphyomyrtus, we have 25 Adnataria, 23 bisectae, 13 Dumaria, 17 Exsertaria, 22 Glandulosae, 14 Latoangulatae,
#  and 32 Maidenaria
table(subset(taxo,Subgenus=="Symphyomyrtus")$Section)
table(subset(taxo,Subgenus=="Symphyomyrtus")$Genus_species)

#- of Eucalyptus, we have 31 Eucalyptus, 4 Frutices, and 3 Longistylus
table(subset(taxo,Subgenus=="Eucalyptus")$Section)

#- of the Eucalyptus Eucalyptus, we have 3 Eucalyptus, 2 Fraxinales, 11 Pachyphloiae, 3 Psathyroxyla,
#  5 Radiatae, 2 Strictae, and 3 White-mahoganies
table(subset(taxo,Subgenus=="Eucalyptus" & Section=="Eucalyptus")$Series)
subset(taxo,Subgenus=="Eucalyptus" & Section=="Eucalyptus" & Series=="Eucalyptus") # Eucalypts are obliqua and pauciflora (2 subspecies)
subset(taxo,Subgenus=="Eucalyptus" & Section=="Eucalyptus" & Series=="Fraxinales") # Fraxinales are delegatensis and fraxinoides



#-------------------------------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------------------------------





#-------------------------------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------------------------------
#- merge the seeds and taxo dataframes, to get the spatial information together with the taxonomic
seeds2 <- seeds[,c("Seedlot.Number","Scientific.name","Lat","Long")]

dat <- merge(seeds2,taxo,by.x="Scientific.name",by.y="Genus_species")


pdf(file="C:/Repos/Mega/output/Subgenera_map.pdf")
#-----
#- plot the Alveolata on the map
plotoz()
points(x=subset(dat,Subgenus=="Alveolata")$Long,y=subset(dat,Subgenus=="Alveolata")$Lat,pch=16,col="red",cex=1)
title(main="Alveolata")


#-----
#- plot the blakelli on the map
plotoz()
points(x=subset(dat,Subgenus=="Blakella")$Long,y=subset(dat,Subgenus=="Blakella")$Lat,pch=16,col="red",cex=1)
title(main="Blakella")


#-----
#- plot the Corymbia on the map
plotoz()
points(x=subset(dat,Subgenus=="Corymbia")$Long,y=subset(dat,Subgenus=="Corymbia")$Lat,pch=16,col="red",cex=1)
title(main="Corymbia")


#-----
#- plot the Eucalyptus on the map
plotoz()
points(x=subset(dat,Subgenus=="Eucalyptus")$Long,y=subset(dat,Subgenus=="Eucalyptus")$Lat,pch=16,col="red",cex=1)
title(main="Eucalyptus")



#-----
#- plot the Eudesmia on the map
plotoz()
points(x=subset(dat,Subgenus=="Eudesmia")$Long,y=subset(dat,Subgenus=="Eudesmia")$Lat,pch=16,col="red",cex=1)
title(main="Eudesmia")



#-----
#- plot the Idiogenes on the map
plotoz()
points(x=subset(dat,Subgenus=="Idiogenes")$Long,y=subset(dat,Subgenus=="Idiogenes")$Lat,pch=16,col="red",cex=1)
title(main="Idiogenes")


#-----
#- plot the Symphyomyrtus on the map
plotoz()
points(x=subset(dat,Subgenus=="Symphyomyrtus")$Long,y=subset(dat,Subgenus=="Symphyomyrtus")$Lat,pch=16,col="red",cex=1)
title(main="Symphyomyrtus")
dev.off()
#-------------------------------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------------------------------






#-------------------------------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------------------------------

#- plot the Symphyomyrtus sections separately, as there are so many species

#- of Symphyomyrtus, we have 25 Adnataria, 23 bisectae, 13 Dumaria, 17 Exsertaria, 22 Glandulosae, 14 Latoangulatae,
#  and 32 Maidenaria
table(subset(taxo,Subgenus=="Symphyomyrtus")$Section)


pdf(file="C:/Repos/Mega/output/Symphyomyrtus_sections_map.pdf")

#- plot the Adnataria on the map
plotoz()
points(x=subset(dat,Subgenus=="Symphyomyrtus" & Section=="Adnataria")$Long,
       y=subset(dat,Subgenus=="Symphyomyrtus"& Section=="Adnataria")$Lat,pch=16,col="red",cex=1)
title(main="Symphyomyrtus-Adnataria")

#- plot the Bisectae on the map
plotoz()
points(x=subset(dat,Subgenus=="Symphyomyrtus" & Section=="Bisectae")$Long,
       y=subset(dat,Subgenus=="Symphyomyrtus"& Section=="Bisectae")$Lat,pch=16,col="red",cex=1)
title(main="Symphyomyrtus-Bisectae")

#- plot the Dumaria on the map
plotoz()
points(x=subset(dat,Subgenus=="Symphyomyrtus" & Section=="Dumaria")$Long,
       y=subset(dat,Subgenus=="Symphyomyrtus"& Section=="Dumaria")$Lat,pch=16,col="red",cex=1)
title(main="Symphyomyrtus-Dumaria")


#- plot the Exsertaria on the map
plotoz()
points(x=subset(dat,Subgenus=="Symphyomyrtus" & Section=="Exsertaria")$Long,
       y=subset(dat,Subgenus=="Symphyomyrtus"& Section=="Exsertaria")$Lat,pch=16,col="red",cex=1)
title(main="Symphyomyrtus-Exsertaria")

#- plot the Glandulosae on the map
plotoz()
points(x=subset(dat,Subgenus=="Symphyomyrtus" & Section=="Glandulosae")$Long,
       y=subset(dat,Subgenus=="Symphyomyrtus"& Section=="Glandulosae")$Lat,pch=16,col="red",cex=1)
title(main="Symphyomyrtus-Glandulosae")

#- plot the Latoangulatae on the map
plotoz()
points(x=subset(dat,Subgenus=="Symphyomyrtus" & Section=="Latoangulatae")$Long,
       y=subset(dat,Subgenus=="Symphyomyrtus"& Section=="Latoangulatae")$Lat,pch=16,col="red",cex=1)
title(main="Symphyomyrtus-Latoangulatae")

#- plot the Maidenaria on the map
plotoz()
points(x=subset(dat,Subgenus=="Symphyomyrtus" & Section=="Maidenaria")$Long,
       y=subset(dat,Subgenus=="Symphyomyrtus"& Section=="Maidenaria")$Lat,pch=16,col="red",cex=1)
title(main="Symphyomyrtus-Maidenaria")
dev.off()