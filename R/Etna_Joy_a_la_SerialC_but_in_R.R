library(terra)
library(ggplot2)
library(tidyterra)
library(sf)
library(ggridges)
library(gridExtra)

#data: a line
lstr<-sf::st_linestring(matrix(c(0,0,1,1,2,2,2,4,4,4,0,7),ncol=2, byrow = TRUE))
lsfg<-sf::st_sfc(lstr)
lsf<-sf::st_sf(lsfg)

#Create perpendicular lines function
perpendicular.lines<-function(lsf,width=2,sample.density=1)
{
#sample points along and get their coordinates
lmptsf<-sf::st_line_sample(lsf,density = sample.density)
lptsf<-sf::st_cast(lmptsf,to = "POINT")
xys<-sf::st_coordinates(lptsf)

#midpoints between two consecutive coordinate pairs
xylag<-cbind(xys[-nrow(xys),],xys[-1,])
colnames(xylag)<-c("X1","Y1","X2","Y2")
xmid<-(xylag[,"X1"]+xylag[,"X2"])/2
ymid<-(xylag[,"Y1"]+xylag[,"Y2"])/2
df<-data.frame(xmid=xmid, ymid=ymid)

#angle of segment between two consecutive pairs
azim<-atan((xylag[,"X2"]-xylag[,"X1"])/(xylag[,"Y2"]-xylag[,"Y1"]))
df$azim<-azim

# xy of start and end point for perpendicular line at midpoint
#  given width
Xpos90<-xmid+width*sin(pi*90/180+azim)
Ypos90<-ymid+width*cos(pi*90/180+azim)
Xneg90<-xmid-width*sin(pi*90/180+azim)
Yneg90<-ymid-width*cos(pi*90/180+azim)

df$geom<-sprintf("LINESTRING(%s %s, %s %s)", Xpos90, Ypos90, Xneg90, Yneg90)
perp_sf = sf::st_as_sf(df, wkt = "geom")
sf::st_crs(perp_sf)<-sf::st_crs(lsf)

return(perp_sf)
}

perps<-perpendicular.lines(lsf, width=1, sample.density=1)

p<-ggplot()+
  geom_sf(data=perps,aes(col=azim))+
  geom_sf(data=lsf)
p+theme_bw()


#Application
#SOURCE: https://tinitaly.pi.ingv.it/
dtmfile1<-"data/TIN_ITALY_SW_Etna/w41510_s10/w41510_s10.tif"
dtmfile2<-"data/TIN_ITALY_SW_Etna/w42010_s10/w42010_s10.tif"
DTM1 <- terra::rast(dtmfile1)
DTM2 <- terra::rast(dtmfile2)
DTM <- terra::mosaic(DTM1,DTM2)
#lstr<-sf::st_linestring(matrix(c(1010000, 4196000,1045000, 4193000),ncol=2, byrow = TRUE))#test
#lsfg<-sf::st_sfc(lstr)
#lsf<-sf::st_sf(lsfg)
#sf::st_crs(lsf)<-terra::crs(DTM)
#perpetna<-perpendicular.lines(lsf, width=5000, sample.density=0.005)

#Line from Adrano to Castiglione di Sicilia #coords from google maps
Adranowgs84<-sf::st_linestring(matrix(c(14.821686, 37.658836,15.113025, 37.876809),ncol=2, byrow = TRUE))
Adrano84sfg<-sf::st_sfc(Adranowgs84)
Adrano84sf<-sf::st_sf(Adrano84sfg)
sf::st_crs(Adrano84sf)<-4326
lsf<-sf::st_transform(Adrano84sf, crs=terra::crs(DTM))
LabelsOD<-data.frame(sf::st_coordinates(lsf)[,1:2])
LabelsOD$Name<-c("Adrano", "Castiglione \ndi Sicilia")

perpetna<-perpendicular.lines(lsf, width=8000, sample.density=0.005)

g<-ggplot()+
  geom_spatraster(data=DTM)+
  geom_sf(data=lsf, col='red')+
  geom_sf(data=perpetna)+
  scale_fill_viridis_c()
g

#sample points along and get their coordinates
lmptetna<-sf::st_line_sample(perpetna,density = 0.01)
etnadf<-data.frame(segment=1:length(lmptetna))
etnadf$geom<-lmptetna
etnasf<-st_as_sf(etnadf)
etnaptsf<-sf::st_cast(etnasf,"POINT")
etnaptsf$persegment <- sequence(rle(etnaptsf$segment)$lengths)

DTMc<-terra::crop(DTM,etnasf)

g2<-ggplot()+
  geom_spatraster(data=DTMc, show.legend = FALSE)+
  geom_sf(data=lsf, col='white')+
  #geom_sf(data=perpetna, col="white", size=0.1)+
  scale_fill_gradient(high="gold", low="black")+
  #geom_sf(data=etnasf,col="white", size=0.05, alpha=0.5)+
  geom_text(data=LabelsOD, aes(x=X, y=Y,label=Name), col="white", size=4)+
  theme_bw()
g2

#Extract raster values from those points
extna<-extract(DTMc,etnaptsf)
names(extna)<-c('pointID',"Zval")
extnasf<-cbind(etnaptsf,extna)

g3<-ggplot()+
  geom_ridgeline(data=extnasf, aes(y=1,x=persegment,
                                   height=Zval,group=segment, col=segment),
                 fill = "black",
                 show.legend = FALSE)+
  scale_color_gradient(low="black",high="gold")+
  theme_void()+
  theme(panel.background = element_rect(fill = "black"))
g3

pdf("2023_Day31hihi/Etna_joy.pdf")
g2
g3
grid.arrange(g3, g2,ncol=2,
             top = "Across Etna \n30DayMapChallenge 2023 Day 31!! - Addicted",
             bottom= "geoffrey.caruso@uni.lu with RStats. Data: https://tinitaly.pi.ingv.it/ \n Mapping process: A R transpose of https://github.com/serialc/30DayMapChallenge/tree/main/2023/day28 \n")
dev.off()

