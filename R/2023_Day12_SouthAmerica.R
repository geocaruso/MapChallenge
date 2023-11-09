#MapChallenge - Day 12 - South America

library(sf)  
library(ggpattern)
library(rmapshaper)

admin0<-st_read("data/ne_10m_admin_0_countries/ne_10m_admin_0_countries.shp")
SAM00<-admin0[admin0$CONTINENT=="South America",]
#remove islands West of 85W parallel (Isla Isabella, etc..)
bbx<-st_bbox(SAM00)
bbx[1]<--85
SAM0<-st_crop(SAM00,bbx)
#keep basic info and remove islands and disputed territories
SAM<-SAM0[-c(12,14,15),c("ADMIN","ADM0_A3","NAME_LONG","POP_EST")]
sam102032<-st_transform(SAM, crs="ESRI:102032")
sam102032$km2<-round(st_area(sam102032)/1000000)

sam<-ms_simplify(sam102032) #simplify shapes especially given Chile border and islands

#Add variables potentially used with mapping
sam$km2rank<-rank(-sam$km2)
sam$popkm2<-sam$POP_EST/sam$km2
sam$popkm2rank<-rank(-sam$popkm2)
sam$color1<-"grey20"
sam$color2<-"grey10"

#"CHL"
sam[1,"color1"]<-"#d52b1e" 
sam[1,"color2"]<-"#0039a6"
#"BOL"
sam[2,"color1"]<-"#DA291C" 
sam[2,"color2"]<-"#007A33"
#"PER"
sam[3,"color1"]<-"#C8102E" 
sam[3,"color2"]<-"#FFFFFF"
#"ARG"
sam[4,"color1"]<-"#6CACE4" 
sam[4,"color2"]<-"#FFFFFF"
#"SUR"
sam[5,"color1"]<-"#007A33" 
sam[5,"color2"]<-"#FFFFFF"
#"GUY"
sam[6,"color1"]<-"#EF3340" 
sam[6,"color2"]<-"#000000"
#"BRA" 
sam[7,"color1"]<-"#009739" 
sam[7,"color2"]<-"#FEDD00"
#"URY"
sam[8,"color1"]<-"#001489" 
sam[8,"color2"]<-"#FFFFFF"
#"ECU"
sam[9,"color1"]<-"#FFD100" 
sam[9,"color2"]<-"#0072CE"
#"COL"
sam[10,"color1"]<-"#FFCD00" 
sam[10,"color2"]<-"#003087"
#"PRY"
sam[11,"color1"]<-"#D52B1E" 
sam[11,"color2"]<-"#0038A8"
#"VEN"
sam[12,"color1"]<-"#FCE300" 
sam[12,"color2"]<-"#EF3340"


ggsam<-ggplot()+
  geom_sf_pattern(data=sam,
                  aes(pattern_spacing = factor(popkm2rank),
                      pattern_fill=color1),
                  pattern_density = 0.3,
                  pattern_fill = sam$color2,
                  pattern_colour  = NA,
                  fill = sam$color1,
                  colour = 'grey')+
  scale_pattern_spacing_discrete()+
  theme_bw()+
labs(title = "South America population density \nor a tale of stripped patterns with flags colours",
     subtitle ="#30DayMapChallenge 2023 Day 12 - South-America (also qualifies for bad map)",
     caption = "geoffrey.caruso@uni.lu with RStats. \nData: Natural Earth and flagcolorcodes.com \n Mapping process: sf, rmapshaper::ms_simplify, ggpattern")+
  theme(legend.position ="none",
        plot.subtitle = element_text(color = "grey10", size = 8),
        panel.border = element_rect(colour = "gold", fill=NA, linewidth =12))

pdf("sam.pdf")
ggsam
dev.off()


