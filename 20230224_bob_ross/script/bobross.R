#Tidy tuesday week of 2023-02-21
#created by Jonathan Huang
# edited 2023-02-21


#load libraries
library(tidyverse)
library(here)
library(tidytuesdayR)
library(BobRossColors)
library(GGally)
#load functions

#load data
tuesdata <- tidytuesdayR::tt_load('2023-02-21')
tuesdata <- tidytuesdayR::tt_load(2023, week = 8)

bob_ross <- tuesdata$bob_ross
glimpse(bob_ross)

#Bob Ross palatte - unique colors
print(unique_bob_ross_colors)
scales::show_col(unique_bob_ross_colors$color_hex)

#cleaning the data (given on tidy tuesday bob ross page)
# The first column doesn't contain data that we need, so we can remove it
bob_ross <- select(bob_ross, -1)
# Several columns refer to presence/absence of named colors.
bob_ross <- bob_ross %>%  
  mutate(
    across(Black_Gesso:Alizarin_Crimson, as.logical)
  )
# Save the data.
# write_csv(
#   bob_ross,
#   here::here(
#     "data", "2023", "2023-02-21",
#     "bob_ross.csv"
#   )
# )

#Correlagram
#sum all the T/F for each category 
#count number of true for each color
# 
# boblong <- bob_ross %>%                        #getting summary of how much each color is used
#   pivot_longer(cols = Black_Gesso:Alizarin_Crimson,  # use long format
#                names_to = "color",
#                values_to = "present") %>% 
#   mutate(score = if_else(present %in% TRUE, 1,0))


# bob_color_sum <- bob_ross %>%                        #getting summary of how much each color is used
#   pivot_longer(cols = Black_Gesso:Alizarin_Crimson,  # use long format
#                names_to = "color",
#                values_to = "present") %>% 
#   mutate(score = if_else(present %in% TRUE, 1,0)) %>%   #turn logical into numerical values (1,0)
#   group_by(color) %>%                                   #group by color and sumamrize mean
#   summarize(sum = sum(present)) %>% 
#   pivot_wider(names_from = color,                       #Make back to wider format
#               values_from = sum) %>% 
#   mutate( painting_title = "SUM") %>%                   #add acolumn with sum to match column with bob_color 
#   relocate(where(is.numeric), .after = where(is.character))  #reorder the column 
#   
# 
# bob_color <- bob_ross %>%                               #combine summary to original df
#   select(c(painting_title,Black_Gesso:Alizarin_Crimson)) %>%    #select the appropriate column
#   rbind(bob_color_sum,.)   #combine two df by row
#   
# Need a correlation 
# #make a correlation martrix - between logical values 
# test <- boblong %>%                           # Created weird concatnated value
#   select(painting_title,color,score) %>%
#   pivot_wider(names_from = color,
#               values_from = score) %>%
#   drop_na()

# matrix <- as.matrix(test)     #trying to make a usable matrix for correlation coefficient
# cor(matrix[,unlist(lapply(matrix, is.numeric))])   #keep only numeric values (1,0)

#graph
boblong %>% ggplot(aes(color, color))+        #use geomtile for correlagram, compare color to color
  geom_tile(aes(fill = score)) +              # color to match the score
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),   #change theme and x axis to be vertical for reading
        axis.title = element_blank())
ggsave(here("20230224_bob_ross","output","failed_correlogram.png"),
       height = 9, width = 11)


#pivot to look at how color usage changed over seasons

bob_ross %>% 
  group_by(season) %>%      #try again, using simpler plot
  summarize(num_colors = mean(num_colors)) %>%  #group and summarize to have mean color used by season
  ggplot(aes(x = season, y = num_colors))+
  geom_point(color = "coral")+                #scatter plot
  geom_smooth(alpha = 0.2,                    #add a trendline (default y~x) & change color and transparency
              color = "coral")+
  geom_label(label = "BOB ROSS COLOR USAGE",   #add a label onto the actual graph
             x = 25,  
             y = 15)+
  geom_text(aes(label = if_else(num_colors>=12|num_colors <8, as.character(season),"")),  #have the highest & lowest #of colors used with the season
            hjust = 0,         #adjust text postiion and color
            vjust = 1.5,
            color = "red")+
  xlab("Season")+          #change axis names
  ylab("Number of Colors Used")+
  scale_y_continuous(breaks = seq(0,15, 5), lim = c(0,15))+    #edit x and y scale intervals
  scale_x_continuous(breaks = seq(0,35,5))+
  theme_bw()   #change theme
ggsave(here("20230224_bob_ross","output","retry_scatter.png"),
       height = 9, width = 11)
