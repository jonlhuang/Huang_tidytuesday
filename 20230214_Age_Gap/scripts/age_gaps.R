#Tidy tuesday first plot
#created by Jonathan Huang
# edited 2023-02-14


#load libraries
library(tidyverse)
library(here)
library(tidytuesdayR)
library(lubridate)
library(igraph)
library(Matrix)
#load functions

#load data
tuesdata <- tidytuesdayR::tt_load('2023-02-14')
age_gaps <- tuesdata$age_gaps
glimpse(age_gaps)

#Analyze data - making a network graph 
actor1 <- tail(names(sort(table(age_gaps$actor_1_name))), 10) #Sort to have the most common actors from first column
actor2 <- tail(names(sort(table(age_gaps$actor_2_name))), 10) #Sort to have the most common actors from second column
topname <- cbind(actor1,actor2)


#filter to have the top 10 repeating actor/actress form each group
topactor <- age_gaps %>% 
  filter(actor_1_name %in% c(actor1)|
         actor_2_name %in% c(actor2))


#need to make a edge list first - the suitable table for network graph
links <- data.frame(                # 2 columns, each row represent a connect between two points
  source = topactor$actor_1_name,
  target = topactor$actor_2_name
)
#create network object - preparation of network graphing
network <- graph_from_data_frame(links, directed = FALSE) 

#degree of connectivity between each node - magnitude of appearance
deg <- degree(network, mode = "all")

#plot graph
plot(network,   #data
     vertex.size = deg, #node size based on degree of connectivity
     vertex.label = ifelse(deg>3, topname,NA),   #label node only if appear more than 3x
     vertex.color = "darkseagreen2",            #change node color
     vertex.label.color = "darkslateblue",      #label name color
     vertex.label.cex=1,                       #placement of label 
     vertex.label.degree = 5,                  #placement of label 
     edge.color = ifelse(topactor$age_difference >20, "red", "darkgrey"),       #connecting line color, only color if >20 age gap
     edge.width = 1,      #width of edge
     layout = layout.circle, main = "Actors in film with Age Gaps >20 years")    #use a circular layout and add a title






