#Proso herbarium data

setwd("~/Desktop/proso")

library(tidyverse)

herbaria<-read.csv("~/Downloads/proso_herbarium.csv")

library(maps)

map1<-map_data("usa")
states_map <- map_data("state")

#filter out bad years and regions
herbaria1<-herbaria %>% filter(decimalLongitude < 0) %>% filter(year>1800)

#first just plot points
ggplot(map1) +
  geom_polygon(aes(long, lat, group = group), color = "black", fill="white")+
  geom_point(data=herbaria1, aes(x=decimalLongitude, y=decimalLatitude, col=year))+
  theme_classic()


#which are since 1980?

herbaria2<-herbaria1 %>% filter(year>1700)

ggplot(map1) +
  geom_polygon(aes(long, lat, group = group), color = "black", fill="white")+
  geom_point(data=herbaria2, aes(x=decimalLongitude, y=decimalLatitude, col=year))+
  theme_classic()

#gif of changes over time

library(gganimate)

p<-ggplot(map1) +
  geom_polygon(aes(long, lat, group = group), color = "black", fill="white")+
  geom_point(data=herbaria2, aes(x=decimalLongitude, y=decimalLatitude, col=year, group=id))+
  theme_classic()+ 
  transition_time(year)+
  #leave earlier points on map
  shadow_mark(size = 1.3) +
  #add a title
  labs(title="Year: {frame_time}")+
  enter_fade()

#decrease duration to speed up. increasing fps gives better resolution, but slower rendering
animate(p, fps = 20, duration = 30, width = 800, height = 600, end_pause = 60)

animate(p, fps = 10, duration = 15, width = 800, height = 600, end_pause = 40)
