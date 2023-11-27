library(ggplot2)
library(sf)
library(magick)

g<-sf::st_read("https://raw.githubusercontent.com/geocaruso/cartolux/main/data/Grid1km_LAU2_Pop2021EU.gpkg")
luxwgs84<-sf::st_read("https://data.public.lu/fr/datasets/r/16103fa4-7ff1-486a-88bc-5018353957ea")
lux<-sf::st_transform(luxwgs84,crs=st_crs(g))


k<-200
g2<-g[g$Pop2021EU>k,]

set.seed(101)
p1<-sf::st_sample(g2,size = ceiling(g2$Pop2021EU/k))
png("2023_Day27_dot/Luxdots1.png",width = 768, height = 1024)
ggplot()+
  geom_sf(data=lux,fill="gold", col=NA)+
  geom_sf(data=p1, size=0.01)+theme_void()+
  coord_sf(xlim = c(st_bbox(lux)[1]-1500, st_bbox(lux)[3]+1500),
           ylim = c(st_bbox(lux)[2]-1500, st_bbox(lux)[4]+1500)
           )+
  labs(title = "Luxembourg population boxed into 1km...(~15 min walk)",
       subtitle ="#30DayMapChallenge - Day 27 - Dot\n[1 dot= 200 people]",
       caption = "geoffrey.caruso@uni.lu with RStats\nMapping process:sf::st_sample() 5 times within each 1km grid, magick::image_animate()")+
  theme(plot.title = element_text(color = "grey10", size = 24),
        plot.subtitle = element_text(color = "grey10", size = 16),
        plot.caption = element_text(color = "grey10", size = 16),
        panel.border = element_rect(colour = "gold", fill=NA, linewidth =1))

dev.off()

set.seed(102)
p2<-sf::st_sample(g2,size = ceiling(g2$Pop2021EU/k))
png("2023_Day27_dot/Luxdots2.png",width = 768, height = 1024)
ggplot()+
  geom_sf(data=lux,fill="gold", col=NA)+
  geom_sf(data=p2, size=0.01)+theme_void()+
  coord_sf(xlim = c(st_bbox(lux)[1]-1500, st_bbox(lux)[3]+1500),
           ylim = c(st_bbox(lux)[2]-1500, st_bbox(lux)[4]+1500)
           )+
  labs(title = "Luxembourg population boxed into 1km...(~15 min walk)",
       subtitle ="#30DayMapChallenge - Day 27 - Dot\n[1 dot= 200 people]",
       caption = "geoffrey.caruso@uni.lu with RStats\nMapping process:sf::st_sample() 5 times within each 1km grid, magick::image_animate()")+
  theme(plot.title = element_text(color = "grey10", size = 24),
        plot.subtitle = element_text(color = "grey10", size = 16),
        plot.caption = element_text(color = "grey10", size = 16),
        panel.border = element_rect(colour = "gold", fill=NA, linewidth =1))

dev.off()

set.seed(103)
p3<-sf::st_sample(g2,size = ceiling(g2$Pop2021EU/k))
png("2023_Day27_dot/Luxdots3.png",width = 768, height = 1024)
ggplot()+
  geom_sf(data=lux,fill="gold", col=NA)+
  geom_sf(data=p3, size=0.01)+theme_void()+
  coord_sf(xlim = c(st_bbox(lux)[1]-1500, st_bbox(lux)[3]+1500),
           ylim = c(st_bbox(lux)[2]-1500, st_bbox(lux)[4]+1500)
  )+
  labs(title = "Luxembourg population boxed into 1km...(~15 min walk)",
       subtitle ="#30DayMapChallenge - Day 27 - Dot\n[1 dot= 200 people]",
       caption = "geoffrey.caruso@uni.lu with RStats\nMapping process:sf::st_sample() 5 times within each 1km grid, magick::image_animate()")+
  theme(plot.title = element_text(color = "grey10", size = 24),
        plot.subtitle = element_text(color = "grey10", size = 16),
        plot.caption = element_text(color = "grey10", size = 16),
        panel.border = element_rect(colour = "gold", fill=NA, linewidth =1))

dev.off()

set.seed(104)
p4<-sf::st_sample(g2,size = ceiling(g2$Pop2021EU/k))
png("2023_Day27_dot/Luxdots4.png",width = 768, height = 1024)
ggplot()+
  geom_sf(data=lux,fill="gold", col=NA)+
  geom_sf(data=p4, size=0.01)+theme_void()+
  coord_sf(xlim = c(st_bbox(lux)[1]-1500, st_bbox(lux)[3]+1500),
           ylim = c(st_bbox(lux)[2]-1500, st_bbox(lux)[4]+1500)
  )+
  labs(title = "Luxembourg population boxed into 1km...(~15 min walk)",
       subtitle ="#30DayMapChallenge - Day 27 - Dot\n[1 dot= 200 people]",
       caption = "geoffrey.caruso@uni.lu with RStats\nMapping process:sf::st_sample() 5 times within each 1km grid, magick::image_animate()")+
  theme(plot.title = element_text(color = "grey10", size = 24),
        plot.subtitle = element_text(color = "grey10", size = 16),
        plot.caption = element_text(color = "grey10", size = 16),
        panel.border = element_rect(colour = "gold", fill=NA, linewidth =1))

dev.off()

set.seed(105)
p5<-sf::st_sample(g2,size = ceiling(g2$Pop2021EU/k))
png("2023_Day27_dot/Luxdots5.png",width = 768, height = 1024)
ggplot()+
  geom_sf(data=lux,fill="gold", col=NA)+
  geom_sf(data=p5, size=0.01)+theme_void()+
  coord_sf(xlim = c(st_bbox(lux)[1]-1500, st_bbox(lux)[3]+1500),
           ylim = c(st_bbox(lux)[2]-1500, st_bbox(lux)[4]+1500)
  )+
  labs(title = "Luxembourg population boxed into 1km...(~15 min walk)",
       subtitle ="#30DayMapChallenge - Day 27 - Dot\n[1 dot= 200 people]",
       caption = "geoffrey.caruso@uni.lu with RStats\nMapping process:sf::st_sample() 5 times within each 1km grid, magick::image_animate()")+
  theme(plot.title = element_text(color = "grey10", size = 24),
        plot.subtitle = element_text(color = "grey10", size = 16),
        plot.caption = element_text(color = "grey10", size = 16),
        panel.border = element_rect(colour = "gold", fill=NA, linewidth =1))
dev.off()


#animate
joint <- image_join(list(image_read("2023_Day27_dot/Luxdots1.png"),
                 image_read("2023_Day27_dot/Luxdots2.png"),
                 image_read("2023_Day27_dot/Luxdots3.png"),
                 image_read("2023_Day27_dot/Luxdots4.png"),
                 image_read("2023_Day27_dot/Luxdots5.png")))

anims<-image_animate(joint, fps = 2.5)

image_write(image = anims,
            path = "2023_Day27_dot/Luxdots.gif")


