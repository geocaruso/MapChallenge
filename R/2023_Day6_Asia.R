#MapChallenge - Day 6 - Asia

library(sf)
library(ggplot2)
library(units)

#Download land, 10deg graticule and equator/tropic lines
#from https://www.naturalearthdata.com/downloads/110m-physical-vectors/
# and unzip in data folder

#read shpfiles as sf
land<-st_read("data/ne_110m_land/ne_110m_land.shp")
grat<-st_read("data/ne_110m_graticules_10/ne_110m_graticules_10.shp")
equatrop<-st_read("data/ne_110m_geographic_lines/ne_110m_geographic_lines.shp")

#subset
Q<-equatrop[equatrop$name=="Equator",]
P<-equatrop[grep("Tropic", equatrop$name),]

#sf point from coordinates of Everest then buffered
Everest= st_sf(st_sfc(st_point(c(86.93,27.99)), crs=st_crs(grat)))
buf <- units::set_units(5000000,'m')
B<-st_buffer(Everest,buf)

#Stupid Plate Carrée Map
plate<-ggplot()+
  geom_sf(data=P,color="goldenrod")+
  geom_sf(data=grat,color="grey")+
  geom_sf(data=Q,color="black")+
  geom_sf(data=land,color=NA, fill="grey40", alpha=0.5)+
  geom_sf(data=B,fill="gold",col=NA, alpha=0.3)+
  geom_sf(data=Everest,color="black")+
  theme_bw()+
  labs(title = "#30DayMapChallenge 2023 Day 6 - Asia ",
       subtitle ="Not sure about the limits of Asia but 5000km from Mt Everest sounds like a reasonnable estimate",
       caption = "geoffrey.caruso@uni.lu with NaturalEarth data and RStudio")+
  theme(plot.subtitle = element_text(color = "grey10", size = 8),
        panel.border = element_rect(colour = "gold", fill=NA, linewidth =12))

#Reproject all layers to azimuthal equidistant projection centered on Everest
# This is distance preserving project so the buffer is a disc

wgs84.lst<-list(land=land,grat=grat,Q=Q,P=P,Everest=Everest, B=B)
aeqd.lst<-lapply(wgs84.lst,function(lyr){
  st_transform(lyr, paste0("+proj=aeqd +lat_0=",st_coordinates(Everest)[2]," +lon_0=",st_coordinates(Everest)[1]," +x_0=0 +y_0=0"))
})

#Note the following intresection of land and buffer cannot be made before reprojecting
# because the input data has invalid speherical geometry
aeqd.lst$I<-st_intersection(aeqd.lst$B, aeqd.lst$land)

azimuthal<-ggplot()+
  geom_sf(data=aeqd.lst$P,color="goldenrod")+
  geom_sf(data=aeqd.lst$grat,color="grey")+
  geom_sf(data=aeqd.lst$Q,color="black")+
  geom_sf(data=aeqd.lst$land,color=NA, fill="grey40", alpha=0.5)+
  geom_sf(data=aeqd.lst$B,fill="gold",col=NA, alpha=0.5)+
  geom_sf(data=aeqd.lst$I,fill="gold", col=NA, alpha=0.8)+
  geom_sf(data=aeqd.lst$Everest,color="black")+
 theme_bw()+
  labs(title = "#30DayMapChallenge 2023 Day 6 - Asia ",
       subtitle ="Not sure about the limits of Asia but 5000km from Mt Everest sounds like a reasonnable estimate",
       caption = "geoffrey.caruso@uni.lu with NaturalEarth data and RStudio")+
  theme(plot.subtitle = element_text(color = "grey10", size = 8),
        panel.border = element_rect(colour = "gold", fill=NA, linewidth =12))

pdf("2023_Day6/Asia.pdf")
print(azimuthal)
print(plate)
dev.off()

png("2023_Day6/Asia.png")
print(azimuthal)
dev.off()
