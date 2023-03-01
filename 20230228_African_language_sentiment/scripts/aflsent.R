#Tidy tuesday week of 2023-02-28
#created by Jonathan Huang
# edited 2023-02-28


#load libraries
library(tidyverse)
library(here)
library(tidytuesdayR)
library(leaflet)
library(plotly)

# Get the Data

# Read in with tidytuesdayR package 
# Install from CRAN via: install.packages("tidytuesdayR")
# This loads the readme and all the datasets for the week of interest

# Either ISO-8601 date or year/week works!

tuesdata <- tidytuesdayR::tt_load('2023-02-28')

afrisenti <- tuesdata$afrisenti
languages <- tuesdata$languages
language_scripts <- tuesdata$language_scripts
language_countries <- tuesdata$language_countries
country_regions <- tuesdata$country_regions

#join metadata with afrisenti
country_language <- left_join(language_countries,country_regions)
complete <- left_join(afrisenti, country_language) 

#looking at summary data 
afrlang_summary <- complete %>% 
  mutate("num" = 1) %>%  #create a countable column
  group_by(language_iso_code, label,country) %>%  #group by language, label, country
  summarize(sum = sum(num)) #summarize the sume

unique(afrlang_summary$country) #check country

#stacked barplot
complete %>%  ggplot()+ 
  geom_bar(aes(x = country, #use bargraph
               fill = label),   #color based on label
           position = "fill") +  #make stacked plot by proportion
  labs(fill = "Sentiment",  #change legnend title
       x = "",  #make x label blank
       y = "Proportion of sentiment", #rename y axis
       title = "Proportion of African tweeter's sentiment")+ #add grpah title
  coord_flip()+  #flip stacked plot 
  scale_fill_viridis_d()+ #change bar colors
  theme_bw()+ #simple theme
  theme(plot.title = element_text(hjust = 0.5))+ #adjust plot title
  geom_hline( yintercept = c(0.33,0.66),   #add vertical line at 1/3s
              linetype = "dashed", #make dashed line
                  color = "hotpink", #make color visible
              linewidth = 1.5) #change width of line
ggsave(here("20230228_African_language_sentiment","output","proportion_sentiment.png"),
       width = 11, height = 9)
  

#make a basic map - tried to put as watermark
leaflet() %>%   #take leaflet package
  addTiles() %>%  #add tiles of maps
  setView(lng = 17, lat = 10, zoom = 3.2) %>%  #set view of the world
  addProviderTiles("OpenStreetMap.Mapnik") #specify 



   