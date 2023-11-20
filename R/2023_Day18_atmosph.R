#MapChallenge - Day 18 - Atmos

library(sf)
library(ggplot2)
library(units)
library(gridExtra)

#Download land, 10deg graticule and equator/tropic lines
#from https://www.naturalearthdata.com/downloads/110m-physical-vectors/
# and unzip in data folder

#read shpfiles as sf
land<-st_read("data/ne_110m_land/ne_110m_land.shp")
countries<-st_read("data/ne_10m_admin_0_countries/ne_10m_admin_0_countries.shp")
grat<-st_read("data/ne_110m_graticules_10/ne_110m_graticules_10.shp")
equatrop<-st_read("data/ne_110m_geographic_lines/ne_110m_geographic_lines.shp")

Hemisphsub = c(xmin = 0, ymin = 0, xmax = 89, ymax = 89)
landv<-st_make_valid(land)
landvsub<-st_crop(landv, Hemisphsub,)
countriesv<-st_make_valid(countries)
countriesvsub<-st_crop(countriesv, Hemisphsub,)
gratsub<-st_crop(grat, Hemisphsub,)

#subset
Q<-equatrop[equatrop$name=="Equator",]
P<-equatrop[grep("Tropic", equatrop$name),]

Qsub<-st_crop(Q, Hemisphsub,)
Psub<-st_crop(P, Hemisphsub,)

#sf point from coordinates of Npole then buffered
Npole= st_sf(st_sfc(st_point(c(0,90)), crs=st_crs(grat)))
buf <- units::set_units(500000,'m')
B<-st_buffer(Npole,buf)
Bsub<-st_crop(B, Hemisphsub,)

#Reproject all layers to azimuthal equidistant projection centered on Npole
# This is distance preserving project so the buffer is a disc

wgs84.lst<-list(land=land,countries=countries,grat=grat,Q=Q,P=P,Npole=Npole, B=B,
                landvsub=landvsub,countriesvsub=countriesvsub,gratsub=gratsub,Qsub=Qsub,Psub=Psub,Bsub=Bsub)
aeqd.lst<-lapply(wgs84.lst,function(lyr){
  st_transform(lyr, paste0("+proj=aeqd +lat_0=",st_coordinates(Npole)[2]," +lon_0=",st_coordinates(Npole)[1]," +x_0=0 +y_0=0"))
})

AtmosLimits<-list(Troposphere=18,Stratosphere=50,Mesosphere=85,
                  KarmanLine=100,Thermosphere=690,Exosphere=10000)
#see https://en.wikipedia.org/wiki/K%C3%A1rm%C3%A1n_line

#K<-500000
#bufprojequator<-st_buffer(aeqd.lst$Qsub, units::set_units(K,'m'))
AtmosQ<-lapply(AtmosLimits,function(x){
  K<-x*1000
  radianalpha<-atan(st_coordinates(aeqd.lst$Qsub)[,"Y"]/st_coordinates(aeqd.lst$Qsub)[,"X"])
  deltaY<-K*sin(radianalpha)
  deltaX<-K*cos(radianalpha)
  XYAtmos<-data.frame(X=st_coordinates(aeqd.lst$Qsub)[,"X"],
                    deltaX=deltaX,
                    Y=st_coordinates(aeqd.lst$Qsub)[,"Y"],
                    deltaY=deltaY)
  XYAtmos$Xd<-XYAtmos$X+XYAtmos$deltaX
  XYAtmos$Yd<-XYAtmos$Y+XYAtmos$deltaY
  return(XYAtmos)
  }
  )

azimuthal1<-ggplot()+
  geom_sf(data=aeqd.lst$Psub,color="goldenrod")+
  geom_sf(data=aeqd.lst$gratsub,color="grey")+
  geom_sf(data=aeqd.lst$Qsub,color="black")+
  geom_line(data=AtmosQ$Thermosphere,aes(x=Xd,y=Yd),col="blue")+
  geom_line(data=AtmosQ$Troposphere,aes(x=Xd,y=Yd),col="green")+
  geom_line(data=AtmosQ$KarmanLine,aes(x=Xd,y=Yd),col="red")+
  geom_sf(data=aeqd.lst$landvsub,color="grey", fill="grey", alpha=0.5)+
  geom_sf(data=aeqd.lst$Npole,color="black")+
  #geom_sf(data=bufprojequator,fill="green", alpha=0.5)+
  theme_void()

azimuthal1_lab<-azimuthal1+
  geom_text(data=AtmosQ$Thermosphere,aes(x = Xd[20], y = Yd[25]),label = "Thermosphere \n<690km",col="blue", angle=20,vjust = 1)+
  geom_text(data=AtmosQ$KarmanLine,aes(x = Xd[40], y = Yd[40]),label = "Karman line 100km",col="red", angle=38,vjust = 0.5)+
  geom_text(data=AtmosQ$Troposphere,aes(x = Xd[70], y = Yd[65]),label = "Troposphere \n<18km",col="green", angle=65,vjust = -0.1)+
  theme(plot.subtitle = element_text(color = "grey10", size = 8),
        panel.border = element_rect(colour = "gold", fill=NA, linewidth =8))

azimuthal1_lab_title<-azimuthal1_lab+
  labs(title = "#30DayMapChallenge 2023 Day 18 - Atmosphere ",
       subtitle ="Earth Atmosphere at Equator viewed from the North Pole",
       caption = "geoffrey.caruso@uni.lu. Data: NaturalEarth, wikipedia.org/wiki/Karman_line \nMapping process: Cropped azimuthal projection, geom_line of computed atmospheric limits at equator")

azimuthal2<-azimuthal1+
  geom_line(data=AtmosQ$Exosphere,aes(x=Xd,y=Yd),col="cyan")

azimuthal2_lab<-azimuthal2+
  geom_text(data=AtmosQ$Thermosphere,aes(x = Xd[15], y = Yd[10]),label = "Thermosphere",col="blue", angle=16,vjust = 1)+
  geom_text(data=AtmosQ$KarmanLine,aes(x = Xd[40], y = Yd[40]),label = "Karman line",col="red", angle=38,vjust = 0.6)+
  geom_text(data=AtmosQ$Troposphere,aes(x = Xd[70], y = Yd[65]),label = "Troposphere ",col="green", angle=65,vjust = -1)+
  geom_text(data=AtmosQ$Exosphere,aes(x = Xd[35], y = Yd[40]),label = "Exosphere \n<10.000km",col="cyan", angle=38,)+
  theme(plot.subtitle = element_text(color = "grey10", size = 8),
        panel.border = element_rect(colour = "gold", fill=NA, linewidth =8))
 
azimuthal2_lab_title<-azimuthal2_lab+ 
  labs(title = "#30DayMapChallenge 2023 Day 18 - Atmosphere ",
       subtitle ="Earth Atmosphere at Equator viewed from the North Pole",
       caption = "geoffrey.caruso@uni.lu. Data: NaturalEarth, wikipedia.org/wiki/Karman_line \nMapping process: Cropped azimuthal projection, geom_line of computed atmospheric limits at equator")

pdf("2023_Day18_Atmos/Atmos.pdf")
print(azimuthal1_lab_title)
print(azimuthal2_lab_title)
dev.off()

Title<-"Earth Atmosphere at Equator viewed from the North Pole\n#30DayMapChallenge 2023 Day 18 - Atmosphere \n"
arr<-grid.arrange(azimuthal1_lab, azimuthal2_lab,
             ncol=2,
             top = grid::textGrob(Title, gp=grid::gpar(fontsize=18)),
             bottom= "geoffrey.caruso@uni.lu. Data: NaturalEarth, wikipedia.org/wiki/Karman_line \nMapping process: Cropped azimuthal projection, geom_line of computed atmospheric limits at equator")

pdf("2023_Day18_Atmos/azimuthal_both.pdf")
arr
dev.off()





