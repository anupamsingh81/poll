library(tidyverse)
library(lubridate)

delhi1=read_csv('openaq.csv')

#delhi1 same as delhi
glimpse(delhi)

delhi1=delhi1 %>% select(-city,-country,-utc,-parameter,-unit,-attribution)

toploc=delhi %>% group_by(locationa) %>% count() %>% filter(n>1000) %>% pull(locationa)


delhi %>%select(-latitude,-longitude) %>% drop_na() %>%  group_by(hour) %>% summarize(value=mean(value)) %>% View()



delhi1=delhi1 %>% mutate(hour=hour(local),day=day(local),month=month(local,label = TRUE),weekday=wday(local,label=TRUE))

delhi %>%drop_na() %>%  group_by(hour) %>% summarize(value=mean(value)) %>% View()


delhi %>%filter(month=='Jan') %>% select(-latitude,-longitude) %>% drop_na() %>%  group_by(weekday) %>% summarize(value=mean(value)) %>% View()



delhi %>%select(-latitude,-longitude) %>% drop_na() %>%  group_by(hour) %>% summarize(value=mean(value)) %>% View()


delhi %>%select(-latitude,-longitude) %>% drop_na() %>% filter(locationa%in%toploc) %>%  group_by(locationa) %>% summarize(value=mean(value)) %>%
  arrange(desc(value)) %>% View()


delhi1$value =abs(delhi1$value) # removing -

summary(delhi$value)

delhi1$locationa=str_replace(delhi1$location,"[,.:-].+","") # replace all following after ,,. or :

delhi %>% select(locationa,latitude,longitude) %>% drop_na() %>% group_by(locationa) %>% summarize(latitude=mean(latitude),
                                                                                     longitude=mean(longitude))


delhi1$locationa = fct_recode(delhi1$locationa,"Delhi Technological University" = "DTU","Income Tax Office" ="ITO")
# recode shortnames

delhi1$locationa=str_trim(delhi1$locationa)


###max


delhi %>% drop_na() %>% group_by(locationa) %>% summarize(max=max(value)) %>% arrange(desc(max)) %>% View()


delhi %>% drop_na() %>% group_by(locationa) %>% summarize(min=min(value)) %>% arrange(desc(min)) %>% View()





######
library(tidyverse)

openaq=read_csv('openaq.csv')

#convert 1985 to 198.5

delhi1$value = ifelse(delhi1$value>=1985,198.5,delhi1$value)

delhi1%>% filter(locationa%in%toploc) %>% 
  ggplot(aes(y=value,x=local))+geom_line()+facet_wrap(~locationa)

library(magrittr)
#https://github.com/tidyverse/magrittr
delhi1 %>% filter(locationa=="US Diplomatic Post")%$%
  summary(value)


delhi1 %>% filter(locationa=="US Diplomatic Post")%$%
  ts.plot(value)

jpa=c(toploc[1],toploc[5],toploc[8],toploc[10],toploc[13],toploc[15],toploc[18])
jpa

toploc
#http://ggplot2.tidyverse.org/reference/scale_gradient.html
delhi1%>% filter(locationa%in%jpa) %>% group_by(locationa,month,weekday,hour) %>%drop_na %>%  
  summarise(value=mean(value)) %>% ungroup() %>% 
  filter(month=="Jan") %>% 
  ggplot(aes(x = hour, y = weekday)) +
  geom_tile(aes(fill = value))+
 # scale_fill_gradient2( low = "green",high = "red")+
  scale_fill_gradientn(colours = terrain.colors(10))+
  facet_wrap(~locationa)
 
# ggplot(aes(y=value,x=local))+geom_line()+facet_wrap(~locationa)
