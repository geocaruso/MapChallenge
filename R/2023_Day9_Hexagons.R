#MapChallenge - Day 9 - Hexagons

library(sf)
library(ggplot2)
library(grid)

#Data
#
#Bees data from compulsory declarations, downloaded as ODS per department at
#https://agriculture.gouv.fr/sites/default/files/2022_declarations_tranche_dep_region.ods
#See https://agriculture.gouv.fr/informations-publiques within "documents à la demande du publique" for explanation
#Download as ODS, which I change to csv in OpenOffice (as readODS versions were not nice with me)
BeeDeclColonies<-read.csv("data/2022_declarations_tranche_dep_region.csv")

#France metropolitaine Departments (COG version, 2023)
#https://wxs.ign.fr/x02uy2aiwjo9bm8ce5plwqmr/telechargement/prepackage/ADMINEXPRESS-COG_SHP_TERRITOIRES_PACK_2023-05-04$ADMIN-EXPRESS-COG_3-2__SHP_LAMB93_FXX_2023-05-03/file/ADMIN-EXPRESS-COG_3-2__SHP_LAMB93_FXX_2023-05-03.7z
#This is 238 Mb! Unzip, keep only DEPARTEMENT, read as sf
D<-read_sf("data/DEP/DEPARTEMENT.shp")

#data cleaning and merging
#Variables: keep only number of colonies (23:40) and department code and label (1 and 2)
#Records: keep only metropolitan France (<97) and remove total (104)
BeeColonies<-BeeDeclColonies[-(97:104),c(1,2,23:40)]
B<-BeeColonies[,1:2]
B$tot=rowSums(BeeColonies[,-(1:2)])

DB<-merge(D,B, by.x="INSEE_DEP", by.y="dep")
DB$totperkm2<-DB$tot/(st_area(DB)/1000000)

#Creating a dot based density sf, i.e. a point per "DotColony"
# Dots will then be "hexbined"
set.seed(101)
DotColony<-500 
Bees<-st_sample(DB, size = round(DB$tot / DotColony))

#Creating a hexagon coverage for binning in
honeycombs<-st_sf(st_make_grid(DB,square=FALSE,n=18))

#Count dots in honeycombs
H<-st_intersects(honeycombs, Bees)
DotsCounts<-lengths(H)
#Be careful this is counts of dots, which must thus be multiplied by DotColony
honeycombs$Colonies<-DotsCounts*DotColony
honeycombs2<-honeycombs[honeycombs$Colonies!=0,]

#Added layers
FR<-st_union(D[D$INSEE_REG!=94,])
XFR<-st_convex_hull(FR)#To create France Hexagon (need to remove Corsica)
XFRbuf<-st_simplify(st_buffer(XFR,40000))
CORS<-st_union(D[D$INSEE_REG==94,])
XCORS<-st_convex_hull(CORS)
XCORSbuf<-st_simplify(st_buffer(XCORS,20000))

#Intersect
honeycombs3<-st_intersection(honeycombs2,c(FR,CORS))

#3D effect using a shifted honeycomb by half the interval
hxy<-st_coordinates(honeycombs2[1,]) #Take first hexagon to measure distances
deltaY<-(hxy[3,"Y"]-hxy[2,"Y"])/2 #knowing hex polygons start from bottom node and clockwise
deltaX<-(hxy[5,"X"]-hxy[3,"X"])/2
shifted_geom<-st_geometry(honeycombs2)+c(deltaX,deltaY)
honeycombs4<-honeycombs2
st_geometry(honeycombs4)<-shifted_geom
st_crs(honeycombs4)<-st_crs(honeycombs2)

#Map
honey<-ggplot()+
  geom_sf(data=XFRbuf, col="goldenrod", fill= "cornsilk")+
  geom_sf(data=XCORSbuf, col="goldenrod", fill= "cornsilk")+
  geom_sf(data=honeycombs4, fill=NA, linewidth=0.1, col="gold")+
  geom_sf(data=honeycombs3, aes(fill=Colonies/1000),col="cornsilk",linewidth=2)+
  #geom_sf(data=honeycombs4, fill=NA, linewidth=0.1, aes(col=Colonies/1000))+
  geom_sf(data=honeycombs3, fill=NA, col="white",linewidth=0.5)+
  geom_sf(data=FR, col="goldenrod", fill= NA)+
geom_sf(data=CORS, col="goldenrod", fill= NA)+
geom_sf(data=Bees, size=0.7,col="grey20")+
geom_sf(data=Bees, size=0.05,col="gold", pch=6)+
geom_sf(data=Bees, size=0.2,col="grey20", pch=18)+
  scale_fill_gradient(low = "gold", high = "darkred",
                      breaks=c(5,10,15,20,25, 30),
                      name="Honeybee \ncolonies (x1000)")+
  #scale_color_gradient(low = "gold", high = "darkred")+
  theme_bw()+
  guides(fill = guide_colourbar(barwidth = 20, barheight = 0.5,
                                ticks.linewidth=1,
                                label.theme = element_text(colour = "goldenrod", angle = 0, size=7)
                                )
         )+
  labs(title = "Honeycombs of honeycombs in the Hexagon!",
       subtitle ="#30DayMapChallenge 2023 Day 9 - Hegaxons ",
       caption = "geoffrey.caruso@uni.lu with RStats. Data: DGAL, Declaration des ruches, 2022 \n Mapping process: 'Departement' scale total colonies, re-shuffled randomly into dots within 'departements', then hex-binned.")+
  theme(plot.subtitle = element_text(color = "grey10", size = 8),
        panel.border = element_rect(colour = "gold", fill=NA, linewidth =12),
        legend.direction = "horizontal", legend.position = c(.45,.065),
        legend.title=element_text(colour = "goldenrod", size=7),
        axis.text.y = element_text(margin = margin(0,-1.5,0,1.5, unit = 'cm'),
                                   colour = "goldenrod", size=7),
        axis.text.x = element_blank(), #had to remove them otherwise conflicting with map key
        axis.ticks=element_blank(),
        panel.grid.major=element_line(colour="goldenrod", linewidth=0.2, linetype = "dashed")
        )+
  annotation_custom(grobTree(textGrob("0°", x=0.37,  y=0.95, hjust=0,gp=gpar(col="goldenrod", fontsize=7))))+
  annotation_custom(grobTree(textGrob("5°E", x=0.62,  y=0.95, hjust=0,gp=gpar(col="goldenrod", fontsize=7))))+
  annotation_custom(grobTree(textGrob("10°E", x=0.85,  y=0.95, hjust=0,gp=gpar(col="goldenrod", fontsize=7))))+
  annotation_custom(grobTree(textGrob("5°W", x=0.11,  y=0.95, hjust=0,gp=gpar(col="goldenrod", fontsize=7))))

#What I expected for 3D
cubic<-ggplot()+
  geom_sf(data=honeycombs3, fill=NA)+
  geom_sf(data=honeycombs4, fill=NA, col="goldenrod")+
  theme_bw()

#Classical discretized map (quantile)
source("https://raw.githubusercontent.com/geocaruso/cartolux/main/R/ggplot.themap.R")
choro<-ggplot.themap(DB,varname ="tot",
                     n=7,
                     low.colour = "gold", high.colour = "darkred",
                     n.digits=0,
                     leg.title="Colonies per sq.km \n[quantiles]",
                     outline.colour="white",
                     outline.width=0.1,
                     main.title="Honeybee colonies density per French Departement",
                     sub.title="geoffrey.caruso@uni.lu  (Data: DGAL, Declaration des ruches, 2022)")

#Bundle
pdf("2023_Day9/2023_Day9_Hexagons.pdf")
print(honey)
print(cubic)
print(choro)
dev.off()



