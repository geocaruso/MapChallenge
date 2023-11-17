#1st attempts OSM trees but much time downloading and 
# didnot succeed in donwloading by part or rather in making sure
# the apply loop works even when some data is not retrieved
# or when columns are not the same (as I understand)
# 
# Works for Luxembourg though

library(sf)
library(osmdata)
library(ggplot2)
library(gridExtra)

#DATA
countries<-st_read("data/ne_10m_admin_0_countries/ne_10m_admin_0_countries.shp")

trees.osm.in.sf<-function(sf,divx=4){ #divx is to divide in parts so overpass is not stuck
  bb<-st_bbox(sf)
  itvlx<-(bb[3]-bb[1])/divx
  lims<-seq(from=bb[1],by=itvlx,length.out=divx+1)
  pieces<-data.frame(xmin=lims[-(divx+1)],
                     ymin=rep(bb[2],divx),
                     xmax=lims[-1],
                     ymax=rep(bb[4],divx))
  trees<-st_sf(data.frame(osm_id=NA,genus=NA), geometry = st_sfc(st_point(c(1,1))), crs = 4326)
for (i in 1:divx) {
  print(i)
  a = st_sf(a = 1:2, geometry = st_sfc(st_point(as.numeric(pieces[i,1:2])), st_point(as.numeric(pieces[i,3:4]))), crs = 4326)
  bb<-st_bbox(a)
  opq1<-opq(bbox = bb,timeout = 60)
  query1<-add_osm_feature(opq=opq1,key = "natural", value="tree")
  try(osm1<-osmdata_sf(query1))
  try(trees<-rbind(trees,osm1$osm_points[,c("osm_id","genus")]))
}
  trees<-trees[-1,]
  treesitsct<- st_intersects(x = trees, y = sf, sparse = FALSE)
  trees_in<-trees[treesitsct==TRUE,]
  trees_withgenus<-trees_in[!is.na(trees_in$genus),]
  return(list(sf=sf,
              trees_in=trees_in,
              trees_withgenus=trees_withgenus))
}

LU<-countries[countries$SOVEREIGNT=="Luxembourg",]
LU.trees<-trees.osm.in.sf(LU) 

NL<-countries[countries$SOVEREIGNT=="Netherlands",]
NL.trees<-trees.osm.in.sf(NL,divx = 100)

AT<-countries[countries$SOVEREIGNT=="Austria",]
AT.trees<-trees.osm.in.sf(AT,divx = 100)

BE<-countries[countries$SOVEREIGNT=="Belgium",]
BE.trees<-trees.osm.in.sf(BE,divx = 100)

#MAPPING
LU0<-ggplot()+
  geom_sf(data=LU, fill="white", col='goldenrod')+
  theme_bw()

LU1<-LU0+geom_sf(data=LU.trees$trees_in, col="darkgreen", size=0.1)

LU2<-LU0+geom_sf(data=LU.trees$trees_withgenus, aes(col=tolower(genus)), size=0.2)+
  theme(legend.position = "none")

pdf("2023_Day15/treesLUX_arrange.pdf")
grid.arrange(LU1, LU2,
             ncol=2,
             top = "OSM street trees in Luxembourg - all & with a Genus \n30DayMapChallenge 2023 Day 15 - OSM",
             bottom= "geoffrey.caruso@uni.lu with RStats. Data: OSM contributors")
dev.off()



