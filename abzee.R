library(tidyverse)
library(ggmap)
library(sf)
library(mapview)

library(ggrepel)

toploc1= tibble(place=toploc)

toploc2=mutate_geocode(toploc1,place)



toploc3 = na.omit(toploc2) # only take p completed object

toploc4= toploc2 %>% filter(!place%in%toploc3$place) # filter uncompleted fiels

toploc5 = toploc4 %>% select(-lat,-lon) %>% mutate_geocode(place)

toploc6 = full_join(toploc3,toploc5)

toploc6$lon[6]=77.24


toploc6$lat[6]=28.62


toploc6$lon[1]=77.30


toploc6$lat[1]=28.65


toploc6$lon[11]=77.21


toploc6$lat[11]=28.69


toploc6$lon[2]=77.08


toploc6$lat[2]=28.555

locations_sf <- st_as_sf(toploc6, coords = c("lon", "lat"), crs = 4326)

mapview(locations_sf)

map <- get_googlemap(center = c(77.10, 28.758), zoom = 11)
# look at these from map view if 12 zoom level in mapview keep 11 or 10 in google map rest same

ggmap(map)


bw_map <- get_googlemap(center = c(77.10, 28.758), zoom = 10,
                        color = "bw",
                        style = "feature:road|visibility:off&style=element:labels|visibility:off&style=feature:administrative|visibility:on")

ggmap(map) +
  geom_point(data = toploc6, aes(x = lon, y = lat))

ggmap(map) +
  geom_point(data = toploc6, aes(x = lon, y = lat))+
  
  geom_text_repel(data = toploc6, aes(x = lon, y = lat, label = place))

ggmap(bw_map) +
  geom_point(data = toploc6, aes(x = lon, y = lat),color="red")+
  
  geom_text_repel(data = toploc6, aes(x = lon, y = lat, label = place))

rm(map)


##########3


distr = st_read('IND_adm2.shp')

summary(distr)
# we see no delhi level district data as only 1 in delhi

summary(distr$NAME_1)

dillisf=distr %>% filter(NAME_1=="Delhi") #districts of delhi

summary(dillisf)

delhi=delhi %>% left_join(toploc6,by=c("locationa"="place"))

dilmap=ggplot(dillisf)+
  geom_sf()
  
ggplot(dillisf)+
  geom_sf()+
  geom_point(data = filter(delhi8,date=="10-1"), aes(x = lon, y = lat,color=value))+
  
  geom_text_repel(data = filter(delhi8,date=="10-1"), aes(x = lon, y = lat, label = locationa))

delhi6=delhi %>% filter(locationa%in%toploc) %>%mutate(month=month(local)) %>%  select(value,hour,day,month,weekday,locationa,lon,lat)

delhi7=delhi6 %>% select(value,month,day,lon,lat,locationa) %>% mutate(date=paste(day,month,sep= "-")) %>% 
  group_by(locationa,date) %>% summarize(value=median(value))


delhi8=delhi7 %>% left_join(toploc6,by=c("locationa"="place"))

delhi9=delhi8 %>% drop_na()

delhi9
library(gganimate)
library(animation)

p1=
  ggplot(dillisf)+
  geom_sf()
  geom_point(data = filter(delhi9,date=="10-1"), aes(x = lon, y = lat),color="red") +
  
  geom_text_repel(data = filter(delhi8,date=="10-1"),label = locationa)

delhi8=delhi8 %>% arrange(date)
gganimate(p1)

delhi8 %>% filter(date=="10-1")


ggplot(dillisf)+
  geom_sf()+
  geom_point(data = filter(delhi8,date=="10-1"), aes(x = lon, y = lat,color=value))+
  
  geom_text_repel(data = filter(delhi8,date=="10-1"), aes(x = lon, y = lat, label = locationa))

library(tidyverse)
library(gganimate)


par=ggplot(dillisf)+
  geom_sf()+
  geom_point(data = delhi8, aes(x = lon, y = lat,color=value,frame=date))

delhi9=delhi9 %>% separate(date,c("day","month")) %>% mutate(date=paste0(month,day))  %>% 
  
  library(lubridate)
 delhi9= delhi9 %>% select(-date) %>% mutate(date=paste(month,day,"2018",sep="-") )%>% 
    mutate(date=mdy(date)) %>% arrange(date) %>% mutate(datu=as.factor(date,order(date)))
  
 gganimate(par)
  
gganimate(par)