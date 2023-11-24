source("R/sf_stripes.R") #code for making stripes
sam<-readRDS("data/sam.rds")

library(sf)
library(ggplot2)
library(ggthemes)
library(classInt)

H<-sf_stripes(sam,"popkm2rank_f",angle=30,n=c(5,300))
p1<-ggplot()+geom_sf(data=H)+
  geom_sf(data=sam, fill=NA, linewidth=0.5, col="black")+
  theme_wsj()

#Then dots rather than lines provide a dotted pattern
H<-sf_stripes(sam,"popkm2rank_f",angle=1,n=c(10,500))
p2<-ggplot()+geom_sf(data=H, linetype = "dotted")+
  geom_sf(data=sam, fill=NA, linewidth=0.5, col="black")+
  theme_wsj()

#And now combine with a classInt discretization,
# which is a better way to obtain a discrete set
cl.intvl<-classIntervals(sam$popkm2, n=4, style="quantile")
sam$quantile.popkm2<-factor(classInt::findCols(cl.intvl))

H<-sf_stripes(sam,"quantile.popkm2",angle=45,n=c(10,300),reverse=FALSE)
p3<-ggplot()+geom_sf(data=H)+
  geom_sf(data=sam, fill=NA, linewidth=0.5, col="black")+
  theme_wsj()

#And crossing stripes:
H1<-sf_stripes(sam,"quantile.popkm2",angle=0,n=c(10,100),reverse=FALSE)
H2<-sf_stripes(sam,"quantile.popkm2",angle=45,n=c(10,100),reverse=FALSE)
p4<-ggplot()+geom_sf(data=H1)+geom_sf(data=H2)+
  geom_sf(data=sam, fill=NA, linewidth=0.5, col="black")+
  theme_wsj()

pdf("2023_DayBW/SA.pdf")
print(p1)
print(p2)
print(p3)
print(p4)
dev.off()

HDI<-st_read("data/Human_Development_Index__2019_-shp/Human_Development_Index__2019_.shp")
HDIea<-st_transform(HDI,"EPSG:8857")

#Quantile
cl.intvl<-classIntervals(HDIea$HDI_2019, n=6, style="quantile")
HDIea$quantile.HDI<-factor(classInt::findCols(cl.intvl))
#This is quite long at world scale
H1<-sf_stripes(HDIea,"quantile.HDI",angle=45,n=c(10,300),reverse=FALSE)

Wqt<-ggplot()+geom_sf(data=H1)+
  geom_sf(data=HDIea, fill=NA, linewidth=0.5, col="black")+
  theme_wsj()

#Jenks
cl.intvl<-classIntervals(HDIea$HDI_2019, n=6, style="jenks")
HDIea$jenks.HDI<-factor(classInt::findCols(cl.intvl))
#This is quite long at world scale
H2<-sf_stripes(HDIea,"jenks.HDI",angle=45,n=c(10,300),reverse=FALSE)

Wjk<-ggplot()+geom_sf(data=H2)+
  geom_sf(data=HDIea, fill=NA, linewidth=0.5, col="black")+
  theme_wsj()

#Box
cl.intvl<-classIntervals(HDIea$HDI_2019, style="box")
HDIea$box.HDI<-factor(classInt::findCols(cl.intvl))
#This is quite long at world scale
H3<-sf_stripes(HDIea,"box.HDI",angle=35,n=c(50,300),reverse=FALSE)

Wbox<-ggplot()+geom_sf(data=H3)+
  geom_sf(data=HDIea, fill=NA, linewidth=0.5, col="black")+
  theme_wsj()+
  labs(title='#Human Development Index 2019',
       subtitle = "White [0.23,0.59); Light stripes [0.59,0.74); Heavy stripes [0.74,0.83); Black [0.84 ,1.19)",
       caption = "Data downloaded from www.esri.com/geoinquiries \nMapping process: Rstats. Own function sf_stripes() and classInt box style for classification")+
  theme(plot.title = element_text(size = 14),
        plot.subtitle = element_text(size = 7),
        plot.caption = element_text(size = 8)) 


#Jenks Life Expectancy
cl.intvl<-classIntervals(HDIea$Life_expec, n=6, style="jenks")
HDIea$jenks.LifE<-factor(classInt::findCols(cl.intvl))
#This is quite long at world scale
H3<-sf_stripes(HDIea,"jenks.LifE",angle=30,n=c(10,300),reverse=FALSE)

WLE<-ggplot()+geom_sf(data=H3)+
  geom_sf(data=HDIea, fill=NA, linewidth=0.5, col="black")+
  theme_wsj()+
  labs(title='#Human Development Index',
       subtitle = "",
       caption = "Data downloaded from www.esri.com/geoinquiries")+
  theme(plot.title = element_text(size = 14),
        plot.caption = element_text(size = 8)) 


pdf("2023_DayBW/HDI.pdf")
print(Wqt)
print(Wjk)
print(Wbox)
print(WLE)
dev.off()

