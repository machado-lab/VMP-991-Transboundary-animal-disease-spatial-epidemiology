# Load packages
library(ggpubr); library(tidyverse);
library(sf); library(dplyr)
library(viridis); 
library(mapview);library(ggmap);library(lubridate)

#load data
load("Q:/My Drive/GitHub/VMP_991_Transboundary-animal-disease-spatial-epidemiology/data/data.RData")

## get county level map to merge with ASF counts
ar11<-raster::getData("GADM", country="DO", level=2)
names(ar11)[4]<-"admin1"
ar11<-st_as_sf(ar11) ## to merge

controls <- st_sample(ar11, 1000) %>%
  st_sf() %>%  
  st_transform(32619)

DRadmin <- DRadmin%>% 
  st_sf() %>% 
  st_transform(32619)

controls %>% 
  ggplot() + 
  geom_sf(fill = 'white') + 
  geom_sf(data = pointcases_dr , 
             color = "#444444", 
             fill = "#444444",
             size = 1.2)+
  geom_sf(),alpha = 0.009, fill = 'yellow') 
