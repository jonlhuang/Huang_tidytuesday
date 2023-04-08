# Tidy Tuesday week of 04-04-2023 Primer League
#Created by Jonathan Huang
# Created 04-08-2023


# Load Library
library(tidyverse)
library(here)
library(tidytuesdayR)
library(lubridate)
library(patchwork)
library(ggpubr)


# Load Data
tuesdata <- tidytuesdayR::tt_load('2023-04-04')
soccer <- tuesdata$soccer %>% 
  mutate(Date = dmy(Date),
         year = year(Date),
         month = month(Date),
         weekoyr = week(Date)) %>% 
  mutate(year = as_factor(year),
         month = as_factor(month)) %>% 
  mutate(month = as_factor(
    factor(
      month,
      levels = c("8","9","10","11","12","1","2","3","4","5")
      )
    )
    ) #order so start of season in aug
glimpse(soccer)

#create time series with heatplot
h <- soccer %>% 
ggplot(aes(month, HS, fill = FTHG))+ #date vs #yellow card by home team to # of home field goals
  geom_tile()+
  scale_fill_gradient(low = "darkblue", high = "hotpink")+
  labs(title = "Shots taken over the course of the season by the Home team",
       x = "Month of 2021 - 2022 season",
       y = "Shots taken",
       fill = "Goals made")+
  theme_classic()+
  theme(panel.background = element_rect(fill = "midnightblue"),
        legend.position = "none")

#Away 
a <- soccer %>% 
  ggplot(aes(month, AS, fill = HTAG))+ #date vs #yellow card by home team to # of home field goals
  geom_tile()+
  scale_fill_gradient(low = "darkblue", high = "hotpink",
                      limits = c(0,6),
                      breaks = c(0,2,4,6))+
  labs(title = "Shots taken over the course of the season by the Away team",
       x = "Month of 2021 - 2022 season",
       y = "Shots taken",
       fill = "Goals Made")+
  theme_classic()+
  theme(panel.background = element_rect(fill = "midnightblue"))


#combine home and away heatplot using ggpubr
patch <- ggarrange(h/a,
          common.legend = TRUE,
          legend = "right")
ggsave(here("20230404_primer_league","output", "soccer_goals.pdf"),
       width = 15, height = 10)



