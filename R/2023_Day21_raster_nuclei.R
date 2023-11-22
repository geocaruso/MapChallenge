#Day 21 Raster
#
library(foreign)#dbf input
library(ggplot2)
library(terra)
library(tidyterra)
library(sf)
library(classInt)
library(RColorBrewer)
library(ggnewscale)

#Data
#link https://www.insee.fr/fr/statistiques/2520034#consulter
#metadata: https://www.insee.fr/fr/statistiques/2520034#dictionnaire

#Data: Population of France at 200m 2016
FR<-read.dbf("data/200m-carreaux-metropole/car_m.dbf")
xy3035z<-data.frame(X=substr(FR$idINSPIRE, start = 24, stop = 30),
                Y=substr(FR$idINSPIRE, start = 16, stop = 22),
                Z=FR$ind_c)
r<-rast(xy3035z, type="xyz")
crs(r)  <- "epsg:3035"

#Data: French Departements. See Day 6 for source
DEPS<-st_read("data/DEP/DEPARTEMENT.shp")
DEPS3035<-st_transform(DEPS,crs=3035)

#quickplot of raster input and departments
p<-ggplot()+
  geom_spatraster(data = r)+
  geom_sf(data=DEPS3035, fill=NA, col="gold")+
  labs(fill = "Population")

pdf("2023_Day21_Raster_Nuclei/raster.pdf")
p
dev.off()

#Compute nuclei (contiguous (at 200m) urban cells)
# Output: a sf polygon and a summary table by classes of pop
# Do it for each Departement

DEPnum.lst<-as.list(sort(DEPS3035$INSEE_DEP))

DEPNuclei<-lapply(DEPnum.lst, function(x){ 
  D<-DEPS3035[DEPS3035$INSEE_DEP==x,]
  message(x)
  
  #I buffer the limits of Deps to avoid too many border effects,
  #side effect is then pop is not exactly the same as department...could recrop afterwards but then need to change make.nuclei function
  Db<-st_buffer(D, dist=200)
  rD<-terra::crop(r,Db, mask=TRUE)
  
  nucleiD<-make.nuclei(rD)
  return(nucleiD)
  })

saveRDS(DEPNuclei,"2023_Day21_Raster_Nuclei/DEPNuclei.RDS")
st_write(DEPNuclei[[57]]$sf,"2023_Day21_Raster_Nuclei/Nuclei56Morbihan.gpkg")

#Get Dispersed pop share per dpt
df<-data.frame(Dpt=unlist(DEPnum.lst),
           sh_dispersed=unlist(lapply(lapply(DEPNuclei,"[[", "classes"),function(x){x[1,6]})),
           sh_over5000=unlist(lapply(lapply(DEPNuclei,"[[", "classes"),function(x){100-x[6,6]}))
)


#Mapping
DEPS3035_sh<-merge(DEPS3035,df,by.x="INSEE_DEP", by.y="Dpt")

p2<-ggplot()+
  geom_sf(data=DEPS3035_sh, aes(fill=sh_dispersed), col="gold")+
  labs(fill = "Dispersed \nPopulation")
p3<-ggplot()+
  geom_sf(data=DEPS3035_sh, aes(fill=sh_over5000), col="gold")+
  labs(fill = "Dispersed \nPopulation")

pdf("2023_Day21_Raster_Nuclei/sh_dispersed.pdf")
p2
p3
dev.off()

rNA<-subst(r, 0, NA)#redefines zeros as A in raster

ncl<-5
n_jenks5.cl.i<-classIntervals(DEPS3035_sh$sh_dispersed, n=ncl, style="jenks")
DEPS3035_sh$n_jenks5<-factor(classInt::findCols(n_jenks5.cl.i))
cl.colours<-brewer.pal(n = 5,name = "YlGn")
leg.labels<-paste(format(round(n_jenks5.cl.i$brks[1:ncl],digits=0), nsmall=0),
                  format(round(n_jenks5.cl.i$brks[2:(ncl+1)],digits=0), nsmall=0),
                  sep=" - ")

p2cl<-ggplot()+
  geom_sf(data=DEPS3035_sh, aes(fill=n_jenks5), col="white",linewidth=0.1)+
   scale_fill_manual("n_jenks5",
                     name = "Dispersed\nPopulation (%) \n(Jenks)",
                     breaks = seq(1:ncl),
                     values=cl.colours,
                     labels=leg.labels,
                     na.value="white")+
  new_scale_fill() +
  geom_spatraster(data = rNA, aes(fill=Z), alpha=0.2, show.legend = FALSE)+
  scale_fill_gradient("Z", low = "grey10",high = "grey10", na.value="transparent")+
  theme_bw()+
  labs(title = "Revisiting dispersed population in France",
       subtitle ="Population living out of a continuous settlement (200m) comprising at least 150 inhabitants
       \n#30DayMapChallenge 2023 Day 21 - raster ",
       caption = "geoffrey.caruso@uni.lu with RStats. Data: INSEE, 2016, 200m resolution \nMapping process: terra::rast() of population coordinates, for each \'Departement\' crop & compute contiguous \npopulation nuclei (\'noyaux d'habitat\') using terra::patches(), aggregate per population classes, \noverlay spatRaster and sf polygons (ggplot2) using classInt for discretization.")+
  theme(plot.subtitle = element_text(color = "grey10", size = 8),
        panel.border = element_rect(colour = "gold", fill=NA, linewidth =12),
        legend.direction = "horizontal", legend.position = c(.47,.07),
        legend.title=element_text(colour = "goldenrod", size=7),
        axis.text.y = element_text(margin = margin(0,-1.5,0,1.5, unit = 'cm'),
                                   colour = "goldenrod", size=7),
        axis.text.x = element_blank(), #had to remove them otherwise conflicting with map key
        axis.ticks=element_blank(),
        panel.grid.major=element_line(colour="goldenrod", linewidth=0.2, linetype = "dashed")
        )




ncl<-5
n_jenks5.cl.i<-classIntervals(DEPS3035_sh$sh_over5000, n=ncl, style="jenks")
DEPS3035_sh$n_jenks5_over5000<-classInt::findCols(n_jenks5.cl.i)
cl.colours<-brewer.pal(n = 5,name = "YlOrRd")
leg.labels<-paste(format(round(n_jenks5.cl.i$brks[1:ncl],digits=0), nsmall=0),
                  format(round(n_jenks5.cl.i$brks[2:(ncl+1)],digits=0), nsmall=0),
                  sep=" - ")


p3cl<-ggplot()+
  geom_sf(data=DEPS3035_sh, aes(fill=factor(n_jenks5_over5000)), col="white",linewidth=0.1)+
  scale_fill_manual("n_jenks5_over5000",name = "Share of Population\nin nuclei >5000 inhab. \n(Jenks)",
                    breaks = seq(1:ncl),
                    values=cl.colours,
                    labels=leg.labels,
                    na.value="white")+
  theme_bw()

pdf("2023_Day21_Raster_Nuclei/sh_dispersed_cl.pdf")
p2cl
p3cl
dev.off()






               