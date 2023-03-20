# Tidy Tuesday of week March 14th, 2023 - European Drug Development
# Created by: Jonathan Huang
# Created 2023-03-19


# Load libraries
library(tidyverse)
library(here)
library(readxl)
library(janitor)
library(tidytuesdayR)
library(GGally)

#load data
tuesdata <- tidytuesdayR::tt_load('2023-03-14')
drugs <- tuesdata$drugs 

#clean data - from tidytuesday

drugs <- drugs |>
  clean_names() |> 
  mutate(
    category = factor(tolower(category)),
    # product_number is based on whether it's human or veterinary, so get rid of
    # the extras.
    product_number = str_extract(product_number, "\\d+$"),
    
    # These two are often only different in capitalization.
    active_substance = tolower(active_substance),
    
    # These are really just dates that got meaningless hours attached.
    marketing_authorisation_date = lubridate::date(
      marketing_authorisation_date
    ),
    date_of_refusal_of_marketing_authorisation = lubridate::date(
      date_of_refusal_of_marketing_authorisation
    ),
    date_of_opinion = lubridate::date(date_of_opinion),
    decision_date = lubridate::date(decision_date),
    
    # Logicals
    patient_safety = patient_safety == "yes",
    additional_monitoring = additional_monitoring == "yes",
    generic = generic == "yes",
    biosimilar = biosimilar == "yes",
    conditional_approval = conditional_approval == "yes",
    exceptional_circumstances = exceptional_circumstances == "yes",
    accelerated_assessment = accelerated_assessment == "yes",
    orphan_medicine = orphan_medicine == "yes",
    
    # Other data types
    revision_number = as.integer(revision_number),
    authorisation_status = factor(tolower(authorisation_status))
    ) 
  

#analysis
glimpse(drugs) 

#select needed column
drugclean <- drugs%>%  
  filter(category %in% "human") %>% 
  slice(1:150) %>%  #select first 150 rows to subset
  select(category,medicine_name,common_name,active_substance) %>%
  separate(col = active_substance,
           into = paste0("active_substance", 1:25),  #dont know how many columns to make, so use paste
           sep = ",") %>% 
  pivot_longer(cols = c("active_substance1":"active_substance25"),
               names_to = "substance_val",
               values_to = "substance") %>% 
  drop_na() %>% 
  group_by(medicine_name) %>% 
  mutate("num_substance"=1)
  

view(drugs[249,]$active_substance) #check longest 

#make heatmap
ggplot(drugclean)+
  geom_tile(aes(x = substance, y = medicine_name, fill = num_substance), color = "red")+ #heatplot tile
  scale_x_discrete(label = abbreviate)+
  scale_y_discrete(label = abbreviate)+
  labs(x = "Active Substance",   #change label names
       y = "Medicine",
    title = "Common substances in human medicine ")+
  theme(axis.text.x = element_text(angle = 90, size = 5), #change theme
        axis.text.y = element_text(angle = 45, size = 5),
        panel.background = element_rect(fill = "gray"))+
  scale_fill_viridis_b()
  
  

  