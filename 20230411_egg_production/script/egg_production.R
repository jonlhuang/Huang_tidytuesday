# Tidy Tuesday week of 04-11-2023 EggProduction
#Created by Jonathan Huang
# Created 04-16-2023


#load libraries
library(tidyverse)
library(here)
library(tidytuesdayR)
library(ggrepel)

#load data
tuesdata <- tidytuesdayR::tt_load(2023, week = 15)

eggproduction <- tuesdata$`egg-production`
cagefreepercentages <- tuesdata$`cage-free-percentages`


#double line graph w/ labels to distinguis each 

eggproduction %>% 
   pivot_longer(cols = c(n_hens:n_eggs),
               names_to = "egg_or_hens",
               values_to = "count")%>% 
ggplot(aes(x = observed_month,
           y = count,
           color = prod_type))+
  #make line plots and scatter plots
  geom_point(aes( shape = egg_or_hens))+
  geom_line(aes(linetype = egg_or_hens,
                label = egg_or_hens))+
  geom_label_repel(aes(label = egg_or_hens))+
  # edit the legend labels
  scale_color_manual(values = c("coral","cadetblue3"))+ 
  scale_shape_manual(labels = c("number of eggs", "number of hens"),
                     values = c(1,3))+
  scale_linetype_manual(labels = c("number of eggs", "number of hens"),
                        values = c("solid","dashed"))+
  geom_label(aes(label = egg_or_hens))+
  #change labs
  labs(x = "Month",
       y = "Count",
       color = "Product Type",
       shape = "Eggs vs Hen",
       linetype = "Eggs vs Hen")+
  theme_classic()
  
ggsave(here("20230411_egg_production","output", "egg_production.pdf"),
       width = 15, height = 10)


                     