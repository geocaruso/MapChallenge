#MapChallenge - Day 9 - Hexagons

library(sf)
library(ggplot2)

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
XFRbuf<-st_simplify(st_buffer(Hull,40000))
CORS<-st_union(D[D$INSEE_REG==94,])
XCORS<-st_convex_hull(CORS)
XCORSbuf<-st_simplify(st_buffer(XCORS,20000))

#Intersect
honeycombs3<-st_intersection(honeycombs2,FR)

#Map
honey<-ggplot()+
geom_sf(data=XFRbuf, col="goldenrod", fill= "cornsilk")+
geom_sf(data=XCORSbuf, col="goldenrod", fill= "cornsilk")+
geom_sf(data=honeycombs2, aes(fill=Colonies),col="cornsilk",linewidth=2)+
geom_sf(data=honeycombs2, fill=NA, col="white",linewidth=0.5)+
geom_sf(data=FR, col="white", fill= NA)+
geom_sf(data=Bees, size=0.1,col="grey20")+
  scale_fill_gradient(low = "gold", high = "darkred")+
  theme_void()

pdf("2023_Day9/2023_Day9_Hexagons.pdf")
print(honey)
dev.off()



