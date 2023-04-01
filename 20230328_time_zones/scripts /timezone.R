# Tidy Tuesday of week March 28th, 2023 - European Drug Development
# Created by: Jonathan Huang
# Created 2023-04-01


#load libraries
library(tidyverse)
library(here)
library(tidytuesdayR)
library(lubridate)
library(maps)
library(mapdata)
library(mapproj)
library(gganimate)

#load data
tuesdata <- tidytuesdayR::tt_load('2023-03-28')
transitions <- tuesdata$transitions
timezones <- tuesdata$timezones
timezone_countries <- tuesdata$timezone_countries
countries <- tuesdata$countries

#make a complete datasheet
zones <- left_join(timezone_countries,timezones) %>% 
  left_join(.,transitions, relationship = "many-to-many") %>% 
  left_join(.,countries) %>% 
  select(!c(comments,dst))

#make a base map
world <- map_data("world")
zone_border <- zones %>% #get min and max longitude to set boundaries 
  group_by(abbreviation) %>% 
  mutate(minlong = min(longitude),
         maxlong = max(longitude))
  filter(max(longitude)&min(longitude))

  #make a map with recorded time zones dotted, and vertical lines as boarders of where time zones are 
a <- ggplot()+
  geom_polygon(data = world, aes(x = long, 
                                 y = lat,
                                 group = group),
               color = "lightblue")+

  geom_point(data = zones, aes(x = longitude,
                                 y = latitude,
                               color = abbreviation))+
  geom_vline(xintercept = c(zone_border$minlong,zone_border$maxlong),
             color = "grey50",
             alpha = 0.5)+
  theme(legend.position = "none",
        panel.background = element_rect(fill = "azure1"),
        panel.grid = element_line(color = "black"))+
  transition_states(abbreviation, 
                    transition_length = 20, #time of animation by section
                    state_length = 15)+ #length of pause
labs(ggtitle("timezone: {current_frame}"))+
  transition_reveal(longitude*-1) #inverse direction of longitude

  animate(a,nframes = length(unique(zones$abbreviation)))
  anim_save(here("20230328_time_zones","output","worldtimezone.gif"))
  ggsave(here("20230328_time_zones","output","worldtimezone.png"))
