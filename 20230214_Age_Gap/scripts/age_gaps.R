#Tidy tuesday first plot
#created by Jonathan Huang
# edited 2023-02-14


#load libraries
library(tidyverse)
library(here)
library(tidytuesdayR)
library(lubridate)

#load functions

#load data
tuesdata <- tidytuesdayR::tt_load('2023-02-14')
age_gaps <- tuesdata$age_gaps
glimpse(age_gaps)

length(unique(age_gaps$actor_1_name & age_gaps$actor_2_name))
