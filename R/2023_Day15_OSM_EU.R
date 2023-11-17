#MapChallenge - Day 15 - OSM
#geoffrey.caruso@uni.lu

####DATA
library(giscoR) #remotes::install_github("dieghernan/giscoR")
library(osmdata)
library(sf)
library(ggplot2)
library(classInt)

gridEU<-giscoR::gisco_get_grid(resolution = "10", spatialtype = "REGION")
#gridEU<-st_crop(gridEU,vector(xmin=,ymin=,xmax=,ymax=))

#BENELUX
BENELUXGRD<-gridEU[grepl("BE|NL|LU", gridEU$NUTS2016_0),]
BENELUXGRD4326<-st_transform(BENELUXGRD, crs=4326)
BENELUXGRD4326$myid<-factor(1:nrow(BENELUXGRD4326))
BENELUXGRD4326$n<-NA
BENELUXGRD4326$ngenus<-NA

BENELUX_L<-split(BENELUXGRD4326, f=BENELUXGRD4326$myid)

ntrees.osm<-function(L){
  nngenus<-lapply(L,function(x){
  message(paste(x$myid, length(L), sep=" / "))
  b<-st_bbox(x)
  opq1<-opq(bbox = b,timeout = 60)
  query1<-add_osm_feature(opq=opq1,key = "natural", value="tree")
  osm1<-osmdata_sf(query1)
  osmsf<-osm1$osm_points
  x$n<-nrow(osmsf)
  x$ngenus<-nrow(osmsf[!is.na(osmsf$genus),])
  return(x)
  })
  Trees.osm<-do.call(rbind,nngenus)
  return(Trees.osm)
}

TreesBENELUXGRD4326<-ntrees.osm(BENELUX_L)
TreesBENELUX3035<-st_transform(TreesBENELUXGRD4326, crs=3035)
saveRDS(TreesBENELUX3035,"data/TreesBENELUX3035.rds")

#FR only
FRDEGRD<-gridEU[grepl("FR", gridEU$NUTS2016_0),]
FRDEGRD4326<-st_transform(FRDEGRD, crs=4326)
FRDEGRD4326$myid<-factor(1:nrow(FRDEGRD4326))
FRDEGRD4326$n<-NA
FRDEGRD4326$ngenus<-NA
FRDE_L<-split(FRDEGRD4326, f=FRDEGRD4326$myid)
TreesFRDEGRD4326<-ntrees.osm(FRDE_L)
TreesFRDEGRD3035<-st_transform(TreesFRDEGRD4326, crs=3035)
saveRDS(TreesFRDEGRD3035,"data/TreesFRDEGRD3035.rds")

#DE only
DEGRD<-gridEU[grepl("DE", gridEU$NUTS2016_0),]
DEGRD4326<-st_transform(DEGRD, crs=4326)
DEGRD4326$myid<-factor(1:nrow(DEGRD4326))
DEGRD4326$n<-NA
DEGRD4326$ngenus<-NA
DE_L<-split(DEGRD4326, f=DEGRD4326$myid)
TreesDEGRD4326<-ntrees.osm(DE_L)
TreesDEGRD3035<-st_transform(TreesDEGRD4326, crs=3035)
saveRDS(TreesDEGRD3035,"data/TreesDEGRD3035.rds")

#IT only
ITGRD<-gridEU[grepl("IT", gridEU$NUTS2016_0),]
ITGRD4326<-st_transform(ITGRD, crs=4326)
ITGRD4326$myid<-factor(1:nrow(ITGRD4326))
ITGRD4326$n<-NA
ITGRD4326$ngenus<-NA
IT_L<-split(ITGRD4326, f=ITGRD4326$myid)
TreesITGRD4326<-ntrees.osm(IT_L)
TreesITGRD3035<-st_transform(TreesITGRD4326, crs=3035)
saveRDS(TreesITGRD3035,"data/TreesITGRD3035.rds")



########
#MAPPING
TreesBENELUX3035<-readRDS("data/TreesBENELUX3035.rds")
TreesFRDEGRD3035<-readRDS("data/TreesFRDEGRD3035.rds") #FR only wity Guyana

#FR remove non metro
AllFR<-st_transform(TreesFRDEGRD3035,crs=4326)
ContFR<-st_crop(AllFR,c(xmin = -10, ymin = 35, xmax = 20, ymax = 55))
TreesFRGRD3035<-st_transform(ContFR,crs=3035)

TreesDEGRD3035<-readRDS("data/TreesDEGRD3035.rds")
TreesITGRD3035<-readRDS("data/TreesITGRD3035.rds")

p<-ggplot()+
  geom_sf(data=TreesBENELUX3035,aes(fill=n), col="goldenrod",linewidth=0.1)+
  geom_sf(data=TreesFRGRD3035,aes(fill=n), col="goldenrod",linewidth=0.1)+
  geom_sf(data=TreesDEGRD3035,aes(fill=n), col="goldenrod",linewidth=0.1)+
  geom_sf(data=TreesITGRD3035,aes(fill=n), col="goldenrod",linewidth=0.1)+
  scale_fill_gradient(low="white", high="darkgreen")+
  theme_bw()+
  theme(panel.border = element_rect(colour = "darkgreen", fill=NA, linewidth =10),
        axis.text.y = element_text(margin = margin(0,-1.2,0,1.2, unit = 'cm'),
                                   colour = "darkgreen", size=4),
        axis.text.x = element_text(margin = margin(-1.,0,1,0, unit = 'cm'),
                           colour = "darkgreen", size=4),
        axis.ticks=element_blank(),
        panel.grid.major=element_line(colour="darkgreen", linewidth=0.2, linetype = "dashed"),
        plot.title = element_text(color = "darkgreen", size = 7)
        )
        
        
#Assembling all countries and mapping using classInt
Together<-rbind(TreesBENELUX3035,
                TreesFRGRD3035,
                TreesDEGRD3035,
                TreesITGRD3035)
Together2<-Together
Together2$n[Together2$n==0]<-NA

ncl<-8
n_jenks8.cl.i<-classIntervals(Together2$n, n=ncl, style="jenks")
Together2$n_jenks8<-classInt::findCols(n_jenks8.cl.i)
cl.colours<-c("beige","cornsilk2","darkolivegreen2",
              "darkolivegreen3", "darkolivegreen4",
              "darkorange3","darkorange4","darkred")
leg.labels<-paste(format(round(n_jenks8.cl.i$brks[1:ncl],digits=0), nsmall=0),
                  format(round(n_jenks8.cl.i$brks[2:(ncl+1)],digits=0), nsmall=0),
                  sep=" - ")

p2<-ggplot()+
  geom_sf(data=Together2, aes(fill=factor(n_jenks8)), col="white",linewidth=0.1)+
  scale_fill_manual(name = "OSM trees/100km2 \n(Jenks)",
                    breaks = seq(1:ncl),
                    values=cl.colours,
                    labels=leg.labels,
                    na.value="white")+
  theme_bw()+
  theme(panel.border = element_rect(colour = "darkgreen", fill=NA, linewidth =10),
        axis.text.y = element_text(margin = margin(0,-1.2,0,1.2, unit = 'cm'),
                                   colour = "darkgreen", size=4),
        axis.text.x = element_text(margin = margin(-1.,0,1,0, unit = 'cm'),
                                   colour = "darkgreen", size=4),
        axis.ticks=element_blank(),
        panel.grid.major=element_line(colour="darkgreen", linewidth=0.2, linetype = "dashed"),
        plot.title = element_text(color = "darkgreen", size = 7),
        panel.background = element_rect(fill = "white"),
        legend.position = c(0.2, 0.2),
        legend.title = element_text(size = 7), 
        legend.text = element_text(size = 7)
  )

pdf("2023_Day15/Europe_n_4th.pdf")
#print(p)
print(p2)
dev.off()

