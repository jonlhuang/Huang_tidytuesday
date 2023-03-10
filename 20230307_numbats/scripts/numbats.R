# Tidy Tuesday of week March 7th, 2023 - Numbats
# Created by: Jonathan Huang
# Created 2023-03-09

#Load libraries
library(htmlwidgets)
library(tidyverse)
library(here)
library(tidytuesdayR)
library(lubridate)
library(ggmap)
library(ggsn)
library(gridExtra)
library(leaflet)

#import data

tuesdata <- tidytuesdayR::tt_load('2023-03-07')
numbats <- tuesdata$numbats
glimpse(numbats)
numbats <- numbats %>% drop_na(year) %>% 
  mutate(
    eventDate = ymd_hms(eventDate),
    year = year(eventDate),
    month = month(eventDate, label=TRUE, abbr=TRUE),
    wday = wday(eventDate, label=TRUE, abbr=TRUE, week_start = 1),
    hour = hour(eventDate),
    day = ymd(as.Date(eventDate)),
  )


#make a map
aus <- get_map("Australia", zoom =4, maptype = "terrain-background") #get map and change zoom & background

ggmap(aus)+
  geom_point(numbats,
             mapping = aes(x = decimalLongitude,
                           y = decimalLatitude, color = year )) #make map and points colored by year
ggsave(here("20230307_numbats","output","googlemap.png"), #save plot
       width = 11, height = 9)




#Make interactive map with Leaflet package
# Create a color palette with handmade bins.
mybins <- seq(1865, 2025, by= 20)
mypalette <- colorBin( palette="YlOrBr", 
                       domain=numbats$year, #use year
                       na.color="transparent", # if not on color palette, make transparent
                       bins=mybins)

#create text box to place in 
mytext <- paste(
  "Longitude: ", numbats$decimalLongitude, "<br/>", 
  "Latitude: ", numbats$decimalLatitude, "<br/>",
  "ID: ",numbats$recordID, "<br/>",
  "Date: ", numbats$day, sep="") %>%
  lapply(htmltools::HTML)

#validate my lat/long in leaflet as valid coordinates
validateCoords( numbats$decimalLongitude, numbats$decimalLatitude)

#mapping
m <- leaflet(numbats) %>% 
  addTiles()  %>% 
  setView( lat=-25, lng=135 , zoom=4) %>% #make view australia
  addProviderTiles("Esri.WorldImagery") %>% #change mape style
  addCircleMarkers(lat = numbats$decimalLatitude,  #specify long/lat
                   lng = numbats$decimalLongitude,
                   fillColor = ~mypalette(year), #select color with data
                   fillOpacity = 0.7, #opacity of circle
                   color="white", #outline
                   radius=8, stroke=TRUE, #size of circle
                   label = mytext) %>%  #specify label
  addLegend( pal=mypalette, values=~year, 
             opacity=0.9, title = "Sighting Year", #specify legend
             position = "bottomright" )
m

#save as html file
saveWidget(m, file = here("20230307_numbats","output","bubblemap.html"))

