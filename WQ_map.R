
require(dplyr)
require(sf)
require(ggplot2)
require(maps)
require(ggspatial)
require(viridis)
require(deltamapr)
require(ggrepel)

library(raster)
library(rgdal)
library(rgeos)
library(maptools)
library(wql)

setwd("~/GitHub/QuickMap")

# find file list, exclude everything except excel
filelist <- list.files(recursive = TRUE)
filelist <- subset(filelist, grepl(".xlsx",filelist))

#Load EDSM data
edsm_data<- read_excel(filelist,skip=1)
str(edsm_data)
#Add week info
edsm_data$Week<-week(edsm_data$Date)
#Add spatial components
edsm_data <- edsm_data %>% rename(Turbidity='Avg Turbidity (NTU)',Secchi='Avg Secchi (M)',Conductivity = 'Avg E/C (µS)')

colnames<- c("Turbidity","Secchi","Conductivity")

for (i in colnames){
  edsm_data[i] <- na_if(edsm_data[i], "n/p")
}

edsm_data<- edsm_data%>% mutate_at(colnames, as.numeric)

edsm_data<- edsm_data%>% st_as_sf(coords=c("Start Longitude", "Start Latitude"), crs=4326) 

#Read water boundaries shape file
Water<-st_read(file.path("Shapefiles","Hydro_poly_UTM10Copy.shp")) %>% st_transform(crs=st_crs(edsm_data))

#Remove outliers
edsm_data_filter <- st_filter(edsm_data,Water)

#add salinity
edsm_data_filter$Salinity <- wql::ec2pss(edsm_data_filter$Conductivity/1000, t=25)
edsm_data_filter$Salinity <- ifelse(edsm_data_filter$Salinity>35,NA,edsm_data_filter$Salinity)

#Plot
fig1<-ggplot() + theme_bw()+
  geom_sf(data = Water, fill="cadetblue1", color="cadetblue1") +
  geom_sf(data=edsm_data_filter %>% filter(complete.cases(Secchi)),shape=19, size=4,aes(color=Secchi))+
  coord_sf(xlim = c(-122.3, -121.35), ylim = c(37.8, 38.61),crs=st_crs(edsm_data))  +
  scale_color_viridis(discrete=FALSE)+
  guides(colour=guide_colourbar(ticks.colour = "black"))+
  labs(title = paste("EDSM Secchi depth data from",min(edsm_data_filter$Date),"to",max(edsm_data_filter$Date)))

fig1

fig2<-ggplot() + theme_bw()+
  geom_sf(data = Water, fill="cadetblue1", color="cadetblue1") +
  geom_sf(data=edsm_data_filter %>% filter(complete.cases(Salinity)),shape=19, size=4,aes(color=Salinity))+
  coord_sf(xlim = c(-122.3, -121.35), ylim = c(37.8, 38.61),crs=st_crs(edsm_data))  +
  scale_color_viridis(discrete=FALSE)+
  guides(colour=guide_colourbar(ticks.colour = "black"))+
  labs(title = paste("EDSM salinity data (in ppt) from",min(edsm_data_filter$Date),"to",max(edsm_data_filter$Date)))
  
fig2


#Print out the map
tiff(filename="EDSM_Map_Secchi.tiff", units="in",type="cairo", bg="white", height=10, 
     width=11, res=300, compression="lzw")
fig1
dev.off()

tiff(filename="EDSM_Map_Salinity.tiff", units="in",type="cairo", bg="white", height=10, 
     width=11, res=300, compression="lzw")
fig2
dev.off()
